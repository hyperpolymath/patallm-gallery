// SPDX-License-Identifier: AGPL-3.0-or-later
// Claude Firefox MCP - Content Script Bridge
// Injected into pages for DOM access

// This content script provides a bridge for the background script
// to execute code in the page context when needed

// Listen for messages from background script
browser.runtime.onMessage.addListener((message, sender, sendResponse) => {
  if (message.type === 'EXECUTE_IN_PAGE') {
    try {
      // Execute the code in page context
      const result = eval(message.code);
      sendResponse({ success: true, result });
    } catch (error) {
      sendResponse({ success: false, error: error.message });
    }
    return true; // Async response
  }

  if (message.type === 'GET_ELEMENT_AT') {
    const { x, y } = message;
    const el = document.elementFromPoint(x, y);
    if (el) {
      sendResponse({
        success: true,
        element: {
          tag: el.tagName.toLowerCase(),
          id: el.id,
          className: el.className,
          text: el.textContent?.slice(0, 100)
        }
      });
    } else {
      sendResponse({ success: false, error: 'No element at coordinates' });
    }
    return true;
  }
});

// Notify background that content script is ready
browser.runtime.sendMessage({ type: 'CONTENT_SCRIPT_READY' }).catch(() => {
  // Background might not be listening, that's ok
});
