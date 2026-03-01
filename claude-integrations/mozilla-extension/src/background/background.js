// SPDX-License-Identifier: AGPL-3.0-or-later
// Background script for Claude Mozilla Extension

// Handle extension icon click - open sidebar
(typeof browser !== 'undefined' ? browser : chrome).action?.onClicked?.addListener?.(() => {
  (typeof browser !== 'undefined' ? browser : chrome).sidebarAction.toggle();
}) || (typeof browser !== 'undefined' ? browser : chrome).browserAction?.onClicked?.addListener?.(() => {
  (typeof browser !== 'undefined' ? browser : chrome).sidebarAction.toggle();
});

// Handle messages from content scripts and sidebar
(typeof browser !== 'undefined' ? browser : chrome).runtime.onMessage.addListener((message, sender, sendResponse) => {
  if (message.type === 'GET_PAGE_CONTENT') {
    // Get content from active tab
    getActiveTabContent().then(sendResponse);
    return true; // Async response
  }

  if (message.type === 'GET_SELECTION') {
    // Get selected text from active tab
    getSelectedText().then(sendResponse);
    return true;
  }

  if (message.type === 'COMPOSE_EMAIL') {
    // Thunderbird-specific: compose email
    composeEmail(message.data).then(sendResponse);
    return true;
  }
});

/**
 * Get content from active tab
 */
async function getActiveTabContent() {
  try {
    const tabs = await (typeof browser !== 'undefined' ? browser : chrome).tabs.query({ active: true, currentWindow: true });
    if (!tabs[0]) return { error: 'No active tab' };

    const results = await (typeof browser !== 'undefined' ? browser : chrome).tabs.executeScript(tabs[0].id, {
      code: `({
        title: document.title,
        url: window.location.href,
        text: document.body.innerText.slice(0, 10000)
      })`
    });

    return results[0] || { error: 'Could not get page content' };
  } catch (e) {
    return { error: e.message };
  }
}

/**
 * Get selected text from active tab
 */
async function getSelectedText() {
  try {
    const tabs = await (typeof browser !== 'undefined' ? browser : chrome).tabs.query({ active: true, currentWindow: true });
    if (!tabs[0]) return { text: '' };

    const results = await (typeof browser !== 'undefined' ? browser : chrome).tabs.executeScript(tabs[0].id, {
      code: 'window.getSelection().toString()'
    });

    return { text: results[0] || '' };
  } catch (e) {
    return { text: '', error: e.message };
  }
}

/**
 * Compose email in Thunderbird
 */
async function composeEmail(data) {
  // Check if we're in Thunderbird
  if (typeof browser !== 'undefined' && browser.compose) {
    try {
      await browser.compose.beginNew({
        to: data.to || [],
        subject: data.subject || '',
        body: data.body || '',
        isPlainText: data.isPlainText !== false
      });
      return { success: true };
    } catch (e) {
      return { error: e.message };
    }
  }
  return { error: 'Email composition not available (requires Thunderbird)' };
}

// Context menu for "Ask Claude about selection"
(typeof browser !== 'undefined' ? browser : chrome).contextMenus?.create?.({
  id: 'ask-claude',
  title: 'Ask Claude about this',
  contexts: ['selection']
});

(typeof browser !== 'undefined' ? browser : chrome).contextMenus?.onClicked?.addListener?.((info, tab) => {
  if (info.menuItemId === 'ask-claude') {
    // Store selection and open sidebar
    (typeof browser !== 'undefined' ? browser : chrome).storage.local.set({
      pendingSelection: info.selectionText
    });
    (typeof browser !== 'undefined' ? browser : chrome).sidebarAction.open();
  }
});

console.log('Claude Mozilla Extension background script loaded');
