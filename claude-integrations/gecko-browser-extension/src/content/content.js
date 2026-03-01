// SPDX-License-Identifier: AGPL-3.0-or-later
// Claude for Gecko - Content Script

/**
 * Creates and shows a response overlay
 * @param {string} text - Response text to display
 * @param {boolean} isError - Whether this is an error message
 */
function showResponseOverlay(text, isError = false) {
  // Remove any existing overlay
  removeOverlay();

  const overlay = document.createElement("div");
  overlay.id = "claude-gecko-overlay";
  overlay.innerHTML = `
    <div class="claude-gecko-response ${isError ? 'error' : ''}">
      <div class="claude-gecko-header">
        <span class="claude-gecko-title">Claude</span>
        <button class="claude-gecko-close" title="Close">&times;</button>
      </div>
      <div class="claude-gecko-content"></div>
      <div class="claude-gecko-actions">
        <button class="claude-gecko-copy" title="Copy to clipboard">Copy</button>
      </div>
    </div>
  `;

  // Set content safely
  const contentDiv = overlay.querySelector(".claude-gecko-content");
  contentDiv.textContent = text;

  // Close button handler
  overlay.querySelector(".claude-gecko-close").addEventListener("click", removeOverlay);

  // Copy button handler
  overlay.querySelector(".claude-gecko-copy").addEventListener("click", async () => {
    try {
      await navigator.clipboard.writeText(text);
      const copyBtn = overlay.querySelector(".claude-gecko-copy");
      copyBtn.textContent = "Copied!";
      setTimeout(() => {
        copyBtn.textContent = "Copy";
      }, 2000);
    } catch (err) {
      console.error("Failed to copy:", err);
    }
  });

  // Click outside to close
  overlay.addEventListener("click", (event) => {
    if (event.target === overlay) {
      removeOverlay();
    }
  });

  // Escape key to close
  document.addEventListener("keydown", handleEscape);

  document.body.appendChild(overlay);
}

/**
 * Handles escape key press
 */
function handleEscape(event) {
  if (event.key === "Escape") {
    removeOverlay();
  }
}

/**
 * Removes the response overlay
 */
function removeOverlay() {
  const overlay = document.getElementById("claude-gecko-overlay");
  if (overlay) {
    overlay.remove();
    document.removeEventListener("keydown", handleEscape);
  }
}

/**
 * Listen for messages from background script
 */
browser.runtime.onMessage.addListener((message) => {
  if (message.type === "CLAUDE_RESPONSE") {
    showResponseOverlay(message.text);
  } else if (message.type === "CLAUDE_ERROR") {
    showResponseOverlay(message.error, true);
  }
});
