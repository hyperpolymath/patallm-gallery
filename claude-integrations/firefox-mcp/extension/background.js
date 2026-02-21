// SPDX-License-Identifier: AGPL-3.0-or-later
// Claude Firefox MCP - Background Script
// Connects to native host via WebSocket

const WS_URL = 'ws://localhost:9876';
let ws = null;
let reconnectTimer = null;
let connectionStatus = 'disconnected';

// Connect to native host
function connect() {
  if (ws && ws.readyState === WebSocket.OPEN) return;

  console.log('[MCP] Connecting to native host...');
  connectionStatus = 'connecting';

  try {
    ws = new WebSocket(WS_URL);

    ws.onopen = () => {
      console.log('[MCP] Connected to native host');
      connectionStatus = 'connected';
      clearTimeout(reconnectTimer);
      updateIcon();
    };

    ws.onmessage = async (event) => {
      try {
        const message = JSON.parse(event.data);
        console.log('[MCP] Received:', message);
        await handleMessage(message);
      } catch (e) {
        console.error('[MCP] Failed to handle message:', e);
      }
    };

    ws.onclose = () => {
      console.log('[MCP] Disconnected from native host');
      connectionStatus = 'disconnected';
      ws = null;
      updateIcon();
      scheduleReconnect();
    };

    ws.onerror = (e) => {
      console.error('[MCP] WebSocket error:', e);
      connectionStatus = 'error';
      updateIcon();
    };
  } catch (e) {
    console.error('[MCP] Failed to connect:', e);
    scheduleReconnect();
  }
}

function scheduleReconnect() {
  clearTimeout(reconnectTimer);
  reconnectTimer = setTimeout(connect, 5000);
}

function updateIcon() {
  const color = connectionStatus === 'connected' ? '#22c55e' : '#ef4444';
  // Could update badge color here if needed
}

function send(message) {
  if (ws && ws.readyState === WebSocket.OPEN) {
    ws.send(JSON.stringify(message));
  } else {
    console.error('[MCP] Cannot send - not connected');
  }
}

// Handle incoming MCP tool calls
async function handleMessage(message) {
  if (message.method === 'tools/call') {
    try {
      const result = await executeToolCall(message.params);
      send({
        jsonrpc: '2.0',
        id: message.id,
        result
      });
    } catch (error) {
      send({
        jsonrpc: '2.0',
        id: message.id,
        error: { code: -32000, message: error.message }
      });
    }
  }
}

// Tool implementations
async function executeToolCall(params) {
  const { name, arguments: args } = params;

  switch (name) {
    case 'screenshot':
      return await takeScreenshot(args);
    case 'navigate':
      return await navigate(args);
    case 'read_page':
      return await readPage(args);
    case 'click':
      return await click(args);
    case 'type':
      return await typeText(args);
    case 'scroll':
      return await scroll(args);
    case 'execute_js':
      return await executeJs(args);
    case 'find':
      return await findElements(args);
    case 'form_input':
      return await formInput(args);
    case 'tabs_list':
      return await tabsList();
    case 'tabs_create':
      return await tabsCreate(args);
    case 'tabs_close':
      return await tabsClose(args);
    case 'get_page_text':
      return await getPageText(args);
    default:
      throw new Error(`Unknown tool: ${name}`);
  }
}

// Get active tab or specific tab
async function getTab(tabId) {
  if (tabId) {
    return await browser.tabs.get(tabId);
  }
  const tabs = await browser.tabs.query({ active: true, currentWindow: true });
  if (!tabs[0]) throw new Error('No active tab');
  return tabs[0];
}

// Screenshot
async function takeScreenshot(args = {}) {
  const tab = await getTab(args.tabId);
  const dataUrl = await browser.tabs.captureTab(tab.id, { format: 'png' });
  return {
    content: [{
      type: 'image',
      data: dataUrl.replace(/^data:image\/png;base64,/, ''),
      mimeType: 'image/png'
    }]
  };
}

// Navigate
async function navigate(args) {
  const { url, tabId } = args;
  const tab = await getTab(tabId);

  if (url === 'back') {
    await browser.tabs.goBack(tab.id);
  } else if (url === 'forward') {
    await browser.tabs.goForward(tab.id);
  } else {
    let targetUrl = url;
    if (!url.includes('://')) {
      targetUrl = 'https://' + url;
    }
    await browser.tabs.update(tab.id, { url: targetUrl });
  }

  // Wait for page load
  await waitForLoad(tab.id);

  const updatedTab = await browser.tabs.get(tab.id);
  return {
    content: [{
      type: 'text',
      text: `Navigated to ${updatedTab.url}`
    }]
  };
}

function waitForLoad(tabId, timeout = 15000) {
  return new Promise((resolve) => {
    const startTime = Date.now();

    const listener = (id, info) => {
      if (id === tabId && info.status === 'complete') {
        browser.tabs.onUpdated.removeListener(listener);
        resolve();
      }
    };

    browser.tabs.onUpdated.addListener(listener);

    setTimeout(() => {
      browser.tabs.onUpdated.removeListener(listener);
      resolve();
    }, timeout);
  });
}

// Read page - build accessibility tree
async function readPage(args = {}) {
  const tab = await getTab(args.tabId);
  const depth = args.depth || 15;
  const filter = args.filter || 'all';

  const results = await browser.tabs.executeScript(tab.id, {
    code: `(function() {
      let refCounter = 0;

      function getRole(el) {
        const role = el.getAttribute('role');
        if (role) return role;

        const tag = el.tagName.toLowerCase();
        const roleMap = {
          'a': 'link', 'button': 'button', 'input': 'textbox',
          'select': 'combobox', 'textarea': 'textbox', 'img': 'image',
          'h1': 'heading', 'h2': 'heading', 'h3': 'heading',
          'h4': 'heading', 'h5': 'heading', 'h6': 'heading',
          'nav': 'navigation', 'main': 'main', 'article': 'article',
          'section': 'region', 'form': 'form', 'table': 'table',
          'ul': 'list', 'ol': 'list', 'li': 'listitem', 'p': 'paragraph'
        };
        return roleMap[tag] || 'generic';
      }

      function isInteractive(el) {
        const tag = el.tagName.toLowerCase();
        const interactive = ['a', 'button', 'input', 'select', 'textarea'];
        return interactive.includes(tag) ||
               el.getAttribute('onclick') ||
               el.getAttribute('role') === 'button' ||
               el.getAttribute('tabindex') !== null;
      }

      function isVisible(el) {
        if (!el.offsetParent && el.tagName !== 'BODY') return false;
        const style = window.getComputedStyle(el);
        return style.display !== 'none' &&
               style.visibility !== 'hidden' &&
               parseFloat(style.opacity) > 0;
      }

      function buildTree(el, currentDepth, maxDepth, filterInteractive) {
        if (currentDepth > maxDepth) return null;
        if (!el || el.nodeType !== 1) return null;
        if (!isVisible(el)) return null;

        const interactive = isInteractive(el);

        if (filterInteractive && !interactive) {
          const children = [];
          for (const child of el.children) {
            const c = buildTree(child, currentDepth, maxDepth, filterInteractive);
            if (c) {
              if (Array.isArray(c)) children.push(...c);
              else children.push(c);
            }
          }
          return children.length > 0 ? children : null;
        }

        const ref = 'ref_' + (refCounter++);
        const rect = el.getBoundingClientRect();

        const node = {
          ref: ref,
          role: getRole(el),
          tag: el.tagName.toLowerCase()
        };

        // Add name
        const name = el.getAttribute('aria-label') ||
                     el.getAttribute('alt') ||
                     el.getAttribute('title') ||
                     el.getAttribute('placeholder') ||
                     (el.tagName === 'INPUT' || el.tagName === 'TEXTAREA' ? el.value : null) ||
                     (el.childNodes.length === 1 && el.childNodes[0].nodeType === 3
                       ? el.textContent.trim().slice(0, 80) : null);
        if (name) node.name = name;

        // Add bounds
        if (rect.width > 0 && rect.height > 0) {
          node.bounds = [
            Math.round(rect.left),
            Math.round(rect.top),
            Math.round(rect.right),
            Math.round(rect.bottom)
          ];
        }

        // Add href for links
        if (el.href) node.href = el.href;

        // Add type for inputs
        if (el.type) node.inputType = el.type;

        // Process children
        const children = [];
        for (const child of el.children) {
          const c = buildTree(child, currentDepth + 1, maxDepth, filterInteractive);
          if (c) {
            if (Array.isArray(c)) children.push(...c);
            else children.push(c);
          }
        }
        if (children.length > 0) node.children = children;

        return node;
      }

      const tree = buildTree(document.body, 0, ${depth}, ${filter === 'interactive'});
      return JSON.stringify(tree, null, 2);
    })()`
  });

  return {
    content: [{
      type: 'text',
      text: results[0] || '{"error": "Unable to read page"}'
    }]
  };
}

// Click
async function click(args) {
  const tab = await getTab(args.tabId);
  const { coordinate, button = 'left' } = args;

  if (!coordinate) {
    throw new Error('coordinate is required for click');
  }

  await browser.tabs.executeScript(tab.id, {
    code: `(function() {
      const x = ${coordinate[0]};
      const y = ${coordinate[1]};
      const button = '${button}';

      const el = document.elementFromPoint(x, y);
      if (!el) return 'No element at coordinates';

      const eventType = button === 'right' ? 'contextmenu' : 'click';
      const event = new MouseEvent(eventType, {
        bubbles: true,
        cancelable: true,
        view: window,
        clientX: x,
        clientY: y,
        button: button === 'right' ? 2 : 0
      });

      el.dispatchEvent(event);

      // Also try native click for interactive elements
      if (button === 'left' && typeof el.click === 'function') {
        el.click();
      }

      return 'Clicked ' + el.tagName;
    })()`
  });

  // Small delay for any navigation/updates
  await new Promise(r => setTimeout(r, 100));

  return {
    content: [{ type: 'text', text: `Clicked at [${coordinate[0]}, ${coordinate[1]}]` }]
  };
}

// Type text
async function typeText(args) {
  const tab = await getTab(args.tabId);
  const { text, coordinate } = args;

  await browser.tabs.executeScript(tab.id, {
    code: `(function() {
      let el = document.activeElement;

      ${coordinate ? `
      const clickEl = document.elementFromPoint(${coordinate[0]}, ${coordinate[1]});
      if (clickEl) {
        clickEl.focus();
        clickEl.click();
        el = clickEl;
      }
      ` : ''}

      if (el && (el.tagName === 'INPUT' || el.tagName === 'TEXTAREA' || el.isContentEditable)) {
        if (el.value !== undefined) {
          el.value = ${JSON.stringify(text)};
          el.dispatchEvent(new Event('input', { bubbles: true }));
          el.dispatchEvent(new Event('change', { bubbles: true }));
        } else if (el.isContentEditable) {
          el.textContent = ${JSON.stringify(text)};
          el.dispatchEvent(new Event('input', { bubbles: true }));
        }
        return 'Typed into ' + el.tagName;
      }
      return 'No focusable element';
    })()`
  });

  return {
    content: [{ type: 'text', text: `Typed: "${text.slice(0, 50)}${text.length > 50 ? '...' : ''}"` }]
  };
}

// Scroll
async function scroll(args) {
  const tab = await getTab(args.tabId);
  const { direction, amount = 300, coordinate } = args;

  const dx = direction === 'left' ? -amount : direction === 'right' ? amount : 0;
  const dy = direction === 'up' ? -amount : direction === 'down' ? amount : 0;

  await browser.tabs.executeScript(tab.id, {
    code: coordinate
      ? `(document.elementFromPoint(${coordinate[0]}, ${coordinate[1]}) || window).scrollBy(${dx}, ${dy})`
      : `window.scrollBy(${dx}, ${dy})`
  });

  return {
    content: [{ type: 'text', text: `Scrolled ${direction} by ${amount}px` }]
  };
}

// Execute JavaScript
async function executeJs(args) {
  const tab = await getTab(args.tabId);
  const { code } = args;

  const results = await browser.tabs.executeScript(tab.id, { code });

  let resultText;
  try {
    resultText = JSON.stringify(results[0], null, 2);
  } catch {
    resultText = String(results[0]);
  }

  return {
    content: [{ type: 'text', text: resultText || 'undefined' }]
  };
}

// Find elements
async function findElements(args) {
  const tab = await getTab(args.tabId);
  const { query } = args;

  const results = await browser.tabs.executeScript(tab.id, {
    code: `(function() {
      const query = ${JSON.stringify(query)}.toLowerCase();
      const results = [];

      // Try CSS selector first
      try {
        const els = document.querySelectorAll(${JSON.stringify(query)});
        for (const el of els) {
          const rect = el.getBoundingClientRect();
          if (rect.width > 0 && rect.height > 0) {
            results.push({
              tag: el.tagName.toLowerCase(),
              text: (el.textContent || '').trim().slice(0, 80),
              bounds: [Math.round(rect.left), Math.round(rect.top), Math.round(rect.right), Math.round(rect.bottom)]
            });
          }
          if (results.length >= 20) break;
        }
        if (results.length > 0) return JSON.stringify(results, null, 2);
      } catch {}

      // Text search
      const walker = document.createTreeWalker(document.body, NodeFilter.SHOW_ELEMENT);
      let node;

      while ((node = walker.nextNode()) && results.length < 20) {
        const text = (node.textContent || '').trim().toLowerCase();
        const label = (node.getAttribute('aria-label') || '').toLowerCase();
        const placeholder = (node.getAttribute('placeholder') || '').toLowerCase();

        if (text.includes(query) || label.includes(query) || placeholder.includes(query)) {
          const rect = node.getBoundingClientRect();
          if (rect.width > 0 && rect.height > 0) {
            results.push({
              tag: node.tagName.toLowerCase(),
              text: (node.textContent || '').trim().slice(0, 80),
              bounds: [Math.round(rect.left), Math.round(rect.top), Math.round(rect.right), Math.round(rect.bottom)]
            });
          }
        }
      }

      return JSON.stringify(results, null, 2);
    })()`
  });

  return {
    content: [{ type: 'text', text: results[0] || '[]' }]
  };
}

// Form input
async function formInput(args) {
  const tab = await getTab(args.tabId);
  const { selector, value } = args;

  await browser.tabs.executeScript(tab.id, {
    code: `(function() {
      const el = document.querySelector(${JSON.stringify(selector)});
      if (!el) return 'Element not found';

      if (el.type === 'checkbox' || el.type === 'radio') {
        el.checked = ${!!value};
      } else if (el.tagName === 'SELECT') {
        el.value = ${JSON.stringify(value)};
      } else {
        el.value = ${JSON.stringify(value)};
      }

      el.dispatchEvent(new Event('input', { bubbles: true }));
      el.dispatchEvent(new Event('change', { bubbles: true }));
      return 'Set value';
    })()`
  });

  return {
    content: [{ type: 'text', text: `Set ${selector} to ${value}` }]
  };
}

// Tabs list
async function tabsList() {
  const tabs = await browser.tabs.query({ currentWindow: true });
  const tabInfo = tabs.map(t => ({
    id: t.id,
    title: t.title,
    url: t.url,
    active: t.active
  }));

  return {
    content: [{ type: 'text', text: JSON.stringify(tabInfo, null, 2) }]
  };
}

// Create tab
async function tabsCreate(args = {}) {
  const { url } = args;
  const tab = await browser.tabs.create({ url: url || 'about:blank', active: true });

  return {
    content: [{ type: 'text', text: `Created tab ${tab.id}` }]
  };
}

// Close tab
async function tabsClose(args = {}) {
  const tab = await getTab(args.tabId);
  await browser.tabs.remove(tab.id);

  return {
    content: [{ type: 'text', text: 'Tab closed' }]
  };
}

// Get page text
async function getPageText(args = {}) {
  const tab = await getTab(args.tabId);

  const results = await browser.tabs.executeScript(tab.id, {
    code: 'document.body.innerText.slice(0, 50000)'
  });

  return {
    content: [{ type: 'text', text: results[0] || '' }]
  };
}

// Initialize
browser.browserAction.onClicked.addListener(() => {
  if (connectionStatus !== 'connected') {
    connect();
  }
  console.log('[MCP] Status:', connectionStatus);
});

// Auto-connect on startup
setTimeout(connect, 1000);

console.log('[MCP] Background script loaded');
