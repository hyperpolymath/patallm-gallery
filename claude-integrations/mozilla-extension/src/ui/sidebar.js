// SPDX-License-Identifier: AGPL-3.0-or-later
// Sidebar UI logic for Claude Mozilla Extension

document.addEventListener('DOMContentLoaded', init);

let currentConversationId = null;
let messages = [];
let isStreaming = false;

async function init() {
  // Check for API key
  const apiKey = await ClaudeStorage.getApiKey();
  if (apiKey) {
    showChatScreen();
    loadSettings();
  } else {
    showSetupScreen();
  }

  // Check for pending selection from context menu
  const storage = typeof browser !== 'undefined' ? browser.storage : chrome.storage;
  const result = await storage.local.get('pendingSelection');
  if (result.pendingSelection) {
    setContext(result.pendingSelection);
    await storage.local.remove('pendingSelection');
  }

  setupEventListeners();
}

function setupEventListeners() {
  // Setup screen
  document.getElementById('api-key-form').addEventListener('submit', handleApiKeySubmit);

  // Chat screen
  document.getElementById('chat-form').addEventListener('submit', handleSendMessage);
  document.getElementById('message-input').addEventListener('keydown', handleInputKeydown);
  document.getElementById('message-input').addEventListener('input', autoResize);
  document.getElementById('new-chat-btn').addEventListener('click', startNewConversation);
  document.getElementById('model-select').addEventListener('change', handleModelChange);
  document.getElementById('settings-btn').addEventListener('click', showSettingsScreen);
  document.getElementById('clear-context').addEventListener('click', clearContext);

  // Settings screen
  document.getElementById('back-btn').addEventListener('click', showChatScreen);
  document.getElementById('save-api-key').addEventListener('click', handleUpdateApiKey);
  document.getElementById('clear-data').addEventListener('click', handleClearData);
  document.getElementById('system-prompt').addEventListener('change', handleSettingsChange);
  document.getElementById('streaming-toggle').addEventListener('change', handleSettingsChange);
  document.getElementById('max-tokens').addEventListener('change', handleSettingsChange);
}

// Screen management
function showSetupScreen() {
  document.getElementById('setup-screen').classList.remove('hidden');
  document.getElementById('chat-screen').classList.add('hidden');
  document.getElementById('settings-screen').classList.add('hidden');
}

function showChatScreen() {
  document.getElementById('setup-screen').classList.add('hidden');
  document.getElementById('chat-screen').classList.remove('hidden');
  document.getElementById('settings-screen').classList.add('hidden');
}

function showSettingsScreen() {
  document.getElementById('setup-screen').classList.add('hidden');
  document.getElementById('chat-screen').classList.add('hidden');
  document.getElementById('settings-screen').classList.remove('hidden');
  loadSettings();
}

// API Key handling
async function handleApiKeySubmit(e) {
  e.preventDefault();
  const input = document.getElementById('api-key-input');
  const apiKey = input.value.trim();

  if (!apiKey.startsWith('sk-ant-')) {
    alert('Invalid API key format. Should start with sk-ant-');
    return;
  }

  await ClaudeStorage.saveApiKey(apiKey);
  showChatScreen();
  startNewConversation();
}

async function handleUpdateApiKey() {
  const input = document.getElementById('settings-api-key');
  const apiKey = input.value.trim();

  if (apiKey && !apiKey.startsWith('sk-ant-')) {
    alert('Invalid API key format. Should start with sk-ant-');
    return;
  }

  if (apiKey) {
    await ClaudeStorage.saveApiKey(apiKey);
    input.value = '';
    alert('API key updated');
  }
}

// Settings
async function loadSettings() {
  const settings = await ClaudeStorage.getSettings();
  const model = await ClaudeStorage.getModel();

  document.getElementById('model-select').value = model;
  document.getElementById('system-prompt').value = settings.systemPrompt || '';
  document.getElementById('streaming-toggle').checked = settings.streaming !== false;
  document.getElementById('max-tokens').value = settings.maxTokens || 4096;
}

async function handleSettingsChange() {
  const settings = {
    systemPrompt: document.getElementById('system-prompt').value,
    streaming: document.getElementById('streaming-toggle').checked,
    maxTokens: parseInt(document.getElementById('max-tokens').value) || 4096
  };
  await ClaudeStorage.saveSettings(settings);
}

async function handleModelChange(e) {
  await ClaudeStorage.saveModel(e.target.value);
}

async function handleClearData() {
  if (confirm('This will delete all conversations and settings. Continue?')) {
    await ClaudeStorage.clearAll();
    showSetupScreen();
  }
}

// Conversation management
function startNewConversation() {
  currentConversationId = 'conv_' + Date.now();
  messages = [];
  renderMessages();
  document.getElementById('message-input').focus();
}

function renderMessages() {
  const container = document.getElementById('messages');
  container.innerHTML = '';

  for (const msg of messages) {
    const div = document.createElement('div');
    div.className = `message ${msg.role}`;
    div.textContent = msg.content;
    container.appendChild(div);
  }

  container.scrollTop = container.scrollHeight;
}

function addMessage(role, content) {
  messages.push({ role, content });
  renderMessages();
}

function updateLastMessage(content) {
  if (messages.length > 0 && messages[messages.length - 1].role === 'assistant') {
    messages[messages.length - 1].content = content;
    renderMessages();
  }
}

function showTypingIndicator() {
  const container = document.getElementById('messages');
  const indicator = document.createElement('div');
  indicator.className = 'message assistant typing-indicator';
  indicator.id = 'typing';
  indicator.innerHTML = '<span></span><span></span><span></span>';
  container.appendChild(indicator);
  container.scrollTop = container.scrollHeight;
}

function hideTypingIndicator() {
  const indicator = document.getElementById('typing');
  if (indicator) indicator.remove();
}

// Context handling
let currentContext = null;

function setContext(text) {
  currentContext = text;
  const contextBar = document.getElementById('context-bar');
  const contextText = document.getElementById('context-text');
  contextText.textContent = `Context: "${text.slice(0, 100)}${text.length > 100 ? '...' : ''}"`;
  contextBar.classList.remove('hidden');
}

function clearContext() {
  currentContext = null;
  document.getElementById('context-bar').classList.add('hidden');
}

// Message sending
async function handleSendMessage(e) {
  e.preventDefault();

  if (isStreaming) return;

  const input = document.getElementById('message-input');
  let userMessage = input.value.trim();

  if (!userMessage) return;

  // Prepend context if available
  if (currentContext) {
    userMessage = `Context:\n"${currentContext}"\n\nQuestion: ${userMessage}`;
    clearContext();
  }

  input.value = '';
  input.style.height = 'auto';
  addMessage('user', userMessage);

  const apiKey = await ClaudeStorage.getApiKey();
  const model = await ClaudeStorage.getModel();
  const settings = await ClaudeStorage.getSettings();

  const apiMessages = messages.map(m => ({
    role: m.role,
    content: m.content
  }));

  const options = {
    model,
    maxTokens: settings.maxTokens || 4096,
    system: settings.systemPrompt || null
  };

  isStreaming = true;
  document.getElementById('send-btn').disabled = true;

  try {
    if (settings.streaming !== false) {
      showTypingIndicator();
      addMessage('assistant', '');

      await ClaudeAPI.streamMessage(apiKey, apiMessages, options, (chunk, fullText) => {
        hideTypingIndicator();
        updateLastMessage(fullText);
      });
    } else {
      showTypingIndicator();
      const response = await ClaudeAPI.sendMessage(apiKey, apiMessages, options);
      hideTypingIndicator();

      const assistantMessage = response.content[0]?.text || 'No response';
      addMessage('assistant', assistantMessage);
    }

    // Save conversation
    await ClaudeStorage.saveConversation(currentConversationId, messages);
  } catch (error) {
    hideTypingIndicator();
    addMessage('assistant', `Error: ${error.message}`);
    messages[messages.length - 1].role = 'error';
    renderMessages();
  } finally {
    isStreaming = false;
    document.getElementById('send-btn').disabled = false;
    input.focus();
  }
}

function handleInputKeydown(e) {
  if (e.key === 'Enter' && !e.shiftKey) {
    e.preventDefault();
    document.getElementById('chat-form').dispatchEvent(new Event('submit'));
  }
}

function autoResize(e) {
  const textarea = e.target;
  textarea.style.height = 'auto';
  textarea.style.height = Math.min(textarea.scrollHeight, 150) + 'px';
}
