// SPDX-License-Identifier: AGPL-3.0-or-later
// Claude for Gecko - Popup Script

const messagesContainer = document.getElementById("messages");
const chatForm = document.getElementById("chat-form");
const userInput = document.getElementById("user-input");
const sendBtn = document.getElementById("send-btn");
const apiKeyWarning = document.getElementById("api-key-warning");
const settingsBtn = document.getElementById("settings-btn");

let conversationHistory = [];
let isLoading = false;

/**
 * Checks if API key is configured and shows warning if not
 */
async function checkApiKey() {
  const response = await browser.runtime.sendMessage({ type: "CHECK_API_KEY" });
  if (!response.hasKey) {
    apiKeyWarning.classList.remove("hidden");
    sendBtn.disabled = true;
  } else {
    apiKeyWarning.classList.add("hidden");
    sendBtn.disabled = false;
  }
}

/**
 * Adds a message to the chat display
 * @param {string} text - Message text
 * @param {string} role - "user" | "assistant" | "error"
 * @param {boolean} isLoading - Whether to show loading indicator
 * @returns {HTMLElement} The created message element
 */
function addMessage(text, role, isLoading = false) {
  const messageDiv = document.createElement("div");
  messageDiv.className = `message ${role}`;

  if (isLoading) {
    messageDiv.classList.add("loading");
    messageDiv.innerHTML = "<p>Thinking</p>";
  } else {
    const paragraph = document.createElement("p");
    paragraph.textContent = text;
    messageDiv.appendChild(paragraph);
  }

  messagesContainer.appendChild(messageDiv);
  messagesContainer.scrollTop = messagesContainer.scrollHeight;

  return messageDiv;
}

/**
 * Updates an existing message element
 * @param {HTMLElement} element - The message element to update
 * @param {string} text - New text content
 */
function updateMessage(element, text) {
  element.classList.remove("loading");
  element.innerHTML = "";
  const paragraph = document.createElement("p");
  paragraph.textContent = text;
  element.appendChild(paragraph);
}

/**
 * Sends a message to Claude and displays the response
 * @param {string} text - User's message
 */
async function sendMessage(text) {
  if (!text.trim() || isLoading) return;

  isLoading = true;
  sendBtn.disabled = true;
  userInput.disabled = true;

  // Add user message to display
  addMessage(text, "user");

  // Add to conversation history
  conversationHistory.push({ role: "user", content: text });

  // Show loading indicator
  const loadingMessage = addMessage("", "assistant", true);

  try {
    const response = await browser.runtime.sendMessage({
      type: "SEND_MESSAGE",
      text: text,
      history: conversationHistory.slice(0, -1) // Exclude the message we just added
    });

    if (response.success) {
      const assistantText = response.data.content[0]?.text || "No response received";
      updateMessage(loadingMessage, assistantText);

      // Add to conversation history
      conversationHistory.push({ role: "assistant", content: assistantText });
    } else {
      loadingMessage.className = "message error";
      updateMessage(loadingMessage, `Error: ${response.error}`);
    }
  } catch (error) {
    loadingMessage.className = "message error";
    updateMessage(loadingMessage, `Error: ${error.message}`);
  }

  isLoading = false;
  sendBtn.disabled = false;
  userInput.disabled = false;
  userInput.focus();
}

/**
 * Handles form submission
 */
chatForm.addEventListener("submit", (event) => {
  event.preventDefault();
  const text = userInput.value.trim();
  userInput.value = "";
  sendMessage(text);
});

/**
 * Handles Enter key (submit) vs Shift+Enter (newline)
 */
userInput.addEventListener("keydown", (event) => {
  if (event.key === "Enter" && !event.shiftKey) {
    event.preventDefault();
    chatForm.dispatchEvent(new Event("submit"));
  }
});

/**
 * Opens options page
 */
settingsBtn.addEventListener("click", () => {
  browser.runtime.openOptionsPage();
});

/**
 * Check for pending selection from context menu
 */
async function checkPendingSelection() {
  const result = await browser.storage.local.get("pendingSelection");
  if (result.pendingSelection) {
    userInput.value = `About this text: "${result.pendingSelection}"\n\nMy question: `;
    userInput.focus();
    userInput.setSelectionRange(userInput.value.length, userInput.value.length);
    await browser.storage.local.remove("pendingSelection");
  }
}

// Initialize
checkApiKey();
checkPendingSelection();
