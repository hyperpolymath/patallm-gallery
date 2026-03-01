// SPDX-License-Identifier: AGPL-3.0-or-later
// Claude for Gecko - Background Service Worker

const ANTHROPIC_API_URL = "https://api.anthropic.com/v1/messages";
const DEFAULT_MODEL = "claude-sonnet-4-20250514";

/**
 * Retrieves the API key from extension storage
 * @returns {Promise<string|null>} The API key or null if not set
 */
async function getApiKey() {
  const result = await browser.storage.local.get("apiKey");
  return result.apiKey || null;
}

/**
 * Sends a message to Claude API
 * @param {string} userMessage - The user's message
 * @param {string} systemPrompt - Optional system prompt
 * @param {Array} conversationHistory - Previous messages for context
 * @returns {Promise<object>} The API response
 */
async function sendMessageToClaude(userMessage, systemPrompt = "", conversationHistory = []) {
  const apiKey = await getApiKey();

  if (!apiKey) {
    throw new Error("API key not configured. Please set your Anthropic API key in the extension options.");
  }

  const messages = [
    ...conversationHistory,
    { role: "user", content: userMessage }
  ];

  const requestBody = {
    model: DEFAULT_MODEL,
    max_tokens: 4096,
    messages: messages
  };

  if (systemPrompt) {
    requestBody.system = systemPrompt;
  }

  const response = await fetch(ANTHROPIC_API_URL, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "x-api-key": apiKey,
      "anthropic-version": "2023-06-01",
      "anthropic-dangerous-direct-browser-access": "true"
    },
    body: JSON.stringify(requestBody)
  });

  if (!response.ok) {
    const errorData = await response.json().catch(() => ({}));
    throw new Error(errorData.error?.message || `API request failed: ${response.status}`);
  }

  return response.json();
}

/**
 * Creates context menu items for text selection
 */
function setupContextMenus() {
  browser.contextMenus.removeAll();

  browser.contextMenus.create({
    id: "claude-explain",
    title: "Ask Claude to explain",
    contexts: ["selection"]
  });

  browser.contextMenus.create({
    id: "claude-summarize",
    title: "Ask Claude to summarize",
    contexts: ["selection"]
  });

  browser.contextMenus.create({
    id: "claude-translate",
    title: "Ask Claude to translate",
    contexts: ["selection"]
  });

  browser.contextMenus.create({
    id: "claude-custom",
    title: "Ask Claude about this...",
    contexts: ["selection"]
  });
}

/**
 * Handles context menu clicks
 */
browser.contextMenus.onClicked.addListener(async (info, tab) => {
  const selectedText = info.selectionText;
  if (!selectedText) return;

  let prompt = "";

  switch (info.menuItemId) {
    case "claude-explain":
      prompt = `Please explain the following text:\n\n"${selectedText}"`;
      break;
    case "claude-summarize":
      prompt = `Please provide a concise summary of the following text:\n\n"${selectedText}"`;
      break;
    case "claude-translate":
      prompt = `Please translate the following text to English (or if already in English, identify the language and translate to Spanish):\n\n"${selectedText}"`;
      break;
    case "claude-custom":
      // Will be handled by popup
      browser.storage.local.set({ pendingSelection: selectedText });
      browser.action.openPopup();
      return;
  }

  try {
    const response = await sendMessageToClaude(prompt);
    const assistantMessage = response.content[0]?.text || "No response received";

    // Send result to content script for display
    browser.tabs.sendMessage(tab.id, {
      type: "CLAUDE_RESPONSE",
      text: assistantMessage
    });
  } catch (error) {
    browser.tabs.sendMessage(tab.id, {
      type: "CLAUDE_ERROR",
      error: error.message
    });
  }
});

/**
 * Handles messages from popup and content scripts
 */
browser.runtime.onMessage.addListener((message, sender, sendResponse) => {
  if (message.type === "SEND_MESSAGE") {
    sendMessageToClaude(message.text, message.systemPrompt, message.history)
      .then(response => {
        sendResponse({ success: true, data: response });
      })
      .catch(error => {
        sendResponse({ success: false, error: error.message });
      });
    return true; // Indicates async response
  }

  if (message.type === "CHECK_API_KEY") {
    getApiKey().then(key => {
      sendResponse({ hasKey: !!key });
    });
    return true;
  }
});

// Initialize context menus on install/startup
browser.runtime.onInstalled.addListener(setupContextMenus);
browser.runtime.onStartup.addListener(setupContextMenus);

console.log("Claude for Gecko background script loaded");
