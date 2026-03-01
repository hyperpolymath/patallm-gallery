// SPDX-License-Identifier: AGPL-3.0-or-later
// Claude for Gecko - Options Page Script

const apiKeyInput = document.getElementById("api-key");
const toggleVisibilityBtn = document.getElementById("toggle-visibility");
const modelSelect = document.getElementById("model-select");
const systemPromptInput = document.getElementById("system-prompt");
const saveBtn = document.getElementById("save-btn");
const clearBtn = document.getElementById("clear-btn");
const statusMessage = document.getElementById("status-message");

/**
 * Shows a status message
 * @param {string} message - Message to display
 * @param {string} type - "success" | "error"
 */
function showStatus(message, type) {
  statusMessage.textContent = message;
  statusMessage.className = `status ${type}`;
  statusMessage.classList.remove("hidden");

  setTimeout(() => {
    statusMessage.classList.add("hidden");
  }, 3000);
}

/**
 * Loads saved settings from storage
 */
async function loadSettings() {
  const result = await browser.storage.local.get([
    "apiKey",
    "model",
    "systemPrompt"
  ]);

  if (result.apiKey) {
    apiKeyInput.value = result.apiKey;
  }

  if (result.model) {
    modelSelect.value = result.model;
  }

  if (result.systemPrompt) {
    systemPromptInput.value = result.systemPrompt;
  }
}

/**
 * Saves settings to storage
 */
async function saveSettings() {
  const apiKey = apiKeyInput.value.trim();
  const model = modelSelect.value;
  const systemPrompt = systemPromptInput.value.trim();

  // Validate API key format
  if (apiKey && !apiKey.startsWith("sk-ant-")) {
    showStatus("Invalid API key format. Should start with 'sk-ant-'", "error");
    return;
  }

  try {
    await browser.storage.local.set({
      apiKey: apiKey,
      model: model,
      systemPrompt: systemPrompt
    });

    showStatus("Settings saved successfully!", "success");
  } catch (error) {
    showStatus(`Failed to save: ${error.message}`, "error");
  }
}

/**
 * Clears all extension data
 */
async function clearAllData() {
  const confirmed = confirm(
    "Are you sure you want to clear all data?\n\n" +
    "This will remove your API key and all settings."
  );

  if (confirmed) {
    try {
      await browser.storage.local.clear();
      apiKeyInput.value = "";
      modelSelect.value = "claude-sonnet-4-20250514";
      systemPromptInput.value = "";
      showStatus("All data cleared", "success");
    } catch (error) {
      showStatus(`Failed to clear: ${error.message}`, "error");
    }
  }
}

/**
 * Toggles API key visibility
 */
toggleVisibilityBtn.addEventListener("click", () => {
  const isPassword = apiKeyInput.type === "password";
  apiKeyInput.type = isPassword ? "text" : "password";
  toggleVisibilityBtn.textContent = isPassword ? "🔒" : "👁️";
});

// Event listeners
saveBtn.addEventListener("click", saveSettings);
clearBtn.addEventListener("click", clearAllData);

// Initialize
loadSettings();
