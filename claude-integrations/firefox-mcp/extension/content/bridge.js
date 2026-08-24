// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// Claude Firefox MCP - Content Script Bridge
// Injected into pages for DOM access

// This content script provides a bridge for the background script
// to execute code in the page context when needed

// Listen for messages from background script
browser.runtime.onMessage.addListener((message, sender, sendResponse) => {
  if (message.type === 'EXECUTE_IN_PAGE') {
    const script = document.createElement('script');
    const id = 'mcp-exec-' + Math.random().toString(36).substr(2, 9);
    script.id = id;
    
    const listener = (event) => {
      if (event.detail.id === id) {
        document.removeEventListener('mcp-exec-result', listener);
        script.remove();
        if (event.detail.error) sendResponse({ success: false, error: event.detail.error });
        else sendResponse({ success: true, result: event.detail.result });
      }
    };
    document.addEventListener('mcp-exec-result', listener);
    
    // Inject the code into the script tag
    // We wrap it to catch errors and send the result back via CustomEvent
    script.textContent = `
      (async () => {
        try {
          const result = await (async () => {
            ${message.code}
          })();
          document.dispatchEvent(new CustomEvent('mcp-exec-result', {
            detail: { id: '${id}', result }
          }));
        } catch (error) {
          document.dispatchEvent(new CustomEvent('mcp-exec-result', {
            detail: { id: '${id}', error: error.message }
          }));
        }
      })();
    `;
    document.documentElement.appendChild(script);
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
