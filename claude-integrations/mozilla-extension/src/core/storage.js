// SPDX-License-Identifier: AGPL-3.0-or-later
// Storage utilities for Mozilla extension

const STORAGE_KEYS = {
  API_KEY: 'claude_api_key',
  MODEL: 'claude_model',
  CONVERSATIONS: 'claude_conversations',
  SETTINGS: 'claude_settings'
};

/**
 * Get browser storage API (works in both MV2 and MV3)
 */
function getStorage() {
  return typeof browser !== 'undefined' ? browser.storage : chrome.storage;
}

/**
 * Save API key securely
 * @param {string} apiKey
 */
async function saveApiKey(apiKey) {
  const storage = getStorage();
  await storage.local.set({ [STORAGE_KEYS.API_KEY]: apiKey });
}

/**
 * Get saved API key
 * @returns {Promise<string|null>}
 */
async function getApiKey() {
  const storage = getStorage();
  const result = await storage.local.get(STORAGE_KEYS.API_KEY);
  return result[STORAGE_KEYS.API_KEY] || null;
}

/**
 * Save selected model
 * @param {string} model
 */
async function saveModel(model) {
  const storage = getStorage();
  await storage.local.set({ [STORAGE_KEYS.MODEL]: model });
}

/**
 * Get saved model
 * @returns {Promise<string>}
 */
async function getModel() {
  const storage = getStorage();
  const result = await storage.local.get(STORAGE_KEYS.MODEL);
  return result[STORAGE_KEYS.MODEL] || 'claude-sonnet-4-20250514';
}

/**
 * Save conversation history
 * @param {string} conversationId
 * @param {Array} messages
 */
async function saveConversation(conversationId, messages) {
  const storage = getStorage();
  const result = await storage.local.get(STORAGE_KEYS.CONVERSATIONS);
  const conversations = result[STORAGE_KEYS.CONVERSATIONS] || {};
  conversations[conversationId] = {
    messages,
    updatedAt: Date.now()
  };
  await storage.local.set({ [STORAGE_KEYS.CONVERSATIONS]: conversations });
}

/**
 * Get conversation history
 * @param {string} conversationId
 * @returns {Promise<Array>}
 */
async function getConversation(conversationId) {
  const storage = getStorage();
  const result = await storage.local.get(STORAGE_KEYS.CONVERSATIONS);
  const conversations = result[STORAGE_KEYS.CONVERSATIONS] || {};
  return conversations[conversationId]?.messages || [];
}

/**
 * List all conversations
 * @returns {Promise<Array>}
 */
async function listConversations() {
  const storage = getStorage();
  const result = await storage.local.get(STORAGE_KEYS.CONVERSATIONS);
  const conversations = result[STORAGE_KEYS.CONVERSATIONS] || {};
  return Object.entries(conversations)
    .map(([id, data]) => ({ id, ...data }))
    .sort((a, b) => b.updatedAt - a.updatedAt);
}

/**
 * Delete a conversation
 * @param {string} conversationId
 */
async function deleteConversation(conversationId) {
  const storage = getStorage();
  const result = await storage.local.get(STORAGE_KEYS.CONVERSATIONS);
  const conversations = result[STORAGE_KEYS.CONVERSATIONS] || {};
  delete conversations[conversationId];
  await storage.local.set({ [STORAGE_KEYS.CONVERSATIONS]: conversations });
}

/**
 * Save settings
 * @param {Object} settings
 */
async function saveSettings(settings) {
  const storage = getStorage();
  await storage.local.set({ [STORAGE_KEYS.SETTINGS]: settings });
}

/**
 * Get settings
 * @returns {Promise<Object>}
 */
async function getSettings() {
  const storage = getStorage();
  const result = await storage.local.get(STORAGE_KEYS.SETTINGS);
  return result[STORAGE_KEYS.SETTINGS] || {
    streaming: true,
    maxTokens: 4096,
    systemPrompt: ''
  };
}

/**
 * Clear all data
 */
async function clearAll() {
  const storage = getStorage();
  await storage.local.clear();
}

// Export for both module and global contexts
if (typeof module !== 'undefined' && module.exports) {
  module.exports = {
    saveApiKey, getApiKey, saveModel, getModel,
    saveConversation, getConversation, listConversations, deleteConversation,
    saveSettings, getSettings, clearAll, STORAGE_KEYS
  };
} else if (typeof window !== 'undefined') {
  window.ClaudeStorage = {
    saveApiKey, getApiKey, saveModel, getModel,
    saveConversation, getConversation, listConversations, deleteConversation,
    saveSettings, getSettings, clearAll, STORAGE_KEYS
  };
}
