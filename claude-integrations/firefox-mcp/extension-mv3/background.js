// SPDX-License-Identifier: AGPL-3.0-or-later
// Claude Firefox MCP - Background Service Worker (MV3)
// Handles service worker lifecycle with reconnection

const WS_URL = 'ws://localhost:9876';
let ws = null;
let reconnectTimer = null;
let keepAliveInterval = null;

// Service worker lifecycle - reconnect on wake
self.addEventListener('activate', () => {
  console.log('[MCP] Service worker activated');
  connect();
});

// Keep service worker alive while connected
function startKeepAlive() {
  stopKeepAlive();
  keepAliveInterval = setInterval(() => {
    if (ws && ws.readyState === WebSocket.OPEN) {
      // Send ping to keep connection alive
      ws.send(JSON.stringify({ type: 'ping' }));
    }
  }, 20000); // Every 20 seconds
}

function stopKeepAlive() {
  if (keepAliveInterval) {
    clearInterval(keepAliveInterval);
    keepAliveInterval = null;
  }
}

// Connection management with reconnection
function connect() {
  if (ws && ws.readyState === WebSocket.OPEN) return;

  console.log('[MCP] Connecting to native host...');

  try {
    ws = new WebSocket(WS_URL);

    ws.onopen = () => {
      console.log('[MCP] Connected to native host');
      clearTimeout(reconnectTimer);
      startKeepAlive();
      updateBadge('connected');
    };

    ws.onmessage = async (event) => {
      try {
        const message = JSON.parse(event.data);
        if (message.type === 'pong') return; // Ignore keepalive response
        console.log('[MCP] Received:', message.method || message.id);
        await handleMessage(message);
      } catch (e) {
        console.error('[MCP] Failed to handle message:', e);
      }
    };

    ws.onclose = () => {
      console.log('[MCP] Disconnected');
      ws = null;
      stopKeepAlive();
      updateBadge('disconnected');
      scheduleReconnect();
    };

    ws.onerror = (e) => {
      console.error('[MCP] WebSocket error');
      updateBadge('error');
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

function updateBadge(status) {
  const colors = {
    connected: '#22c55e',
    disconnected: '#6b7280',
    error: '#ef4444'
  };
  browser.action.setBadgeBackgroundColor({ color: colors[status] || '#6b7280' });
  browser.action.setBadgeText({ text: status === 'connected' ? '' : '!' });
}

function send(message) {
  if (ws && ws.readyState === WebSocket.OPEN) {
    ws.send(JSON.stringify(message));
  } else {
    console.error('[MCP] Cannot send - not connected');
    // Try to reconnect
    connect();
  }
}

// Handle MCP tool calls
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

// Tool dispatch
async function executeToolCall(params) {
  const { name, arguments: args } = params;

  const tools = {
    screenshot: takeScreenshot,
    navigate: navigate,
    read_page: readPage,
    click: click,
    type: typeText,
    scroll: scroll,
    execute_js: executeJs,
    find: findElements,
    form_input: formInput,
    tabs_list: tabsList,
    tabs_create: tabsCreate,
    tabs_close: tabsClose,
    get_page_text: getPageText
  };

  const handler = tools[name];
  if (!handler) throw new Error(`Unknown tool: ${name}`);

  return await handler(args || {});
}

// Get active tab
async function getTab(tabId) {
  if (tabId) return await browser.tabs.get(tabId);
  const tabs = await browser.tabs.query({ active: true, currentWindow: true });
  if (!tabs[0]) throw new Error('No active tab');
  return tabs[0];
}

// Screenshot (MV3 uses different API)
async function takeScreenshot(args) {
  const tab = await getTab(args.tabId);
  const dataUrl = await browser.tabs.captureVisibleTab(tab.windowId, { format: 'png' });
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
    if (!url.includes('://')) targetUrl = 'https://' + url;
    await browser.tabs.update(tab.id, { url: targetUrl });
  }

  await waitForLoad(tab.id);
  const updatedTab = await browser.tabs.get(tab.id);

  return {
    content: [{ type: 'text', text: `Navigated to ${updatedTab.url}` }]
  };
}

function waitForLoad(tabId, timeout = 15000) {
  return new Promise((resolve) => {
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

// Read page - MV3 uses scripting API
async function readPage(args) {
  const tab = await getTab(args.tabId);
  const depth = args.depth || 15;
  const filterInteractive = args.filter === 'interactive';

  const results = await browser.scripting.executeScript({
    target: { tabId: tab.id },
    func: buildAccessibilityTree,
    args: [depth, filterInteractive]
  });

  return {
    content: [{
      type: 'text',
      text: results[0]?.result || '{"error": "Unable to read page"}'
    }]
  };
}

// Injected function for reading page
function buildAccessibilityTree(maxDepth, filterInteractive) {
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
    return ['a', 'button', 'input', 'select', 'textarea'].includes(tag) ||
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

  function buildTree(el, depth) {
    if (depth > maxDepth || !el || el.nodeType !== 1 || !isVisible(el)) return null;

    if (filterInteractive && !isInteractive(el)) {
      const children = [];
      for (const child of el.children) {
        const c = buildTree(child, depth);
        if (c) children.push(...(Array.isArray(c) ? c : [c]));
      }
      return children.length > 0 ? children : null;
    }

    const rect = el.getBoundingClientRect();
    const node = {
      ref: 'ref_' + (refCounter++),
      role: getRole(el),
      tag: el.tagName.toLowerCase()
    };

    const name = el.getAttribute('aria-label') || el.getAttribute('alt') ||
                 el.getAttribute('title') || el.getAttribute('placeholder') ||
                 (['INPUT', 'TEXTAREA'].includes(el.tagName) ? el.value : null) ||
                 (el.childNodes.length === 1 && el.childNodes[0].nodeType === 3
                   ? el.textContent.trim().slice(0, 80) : null);
    if (name) node.name = name;

    if (rect.width > 0 && rect.height > 0) {
      node.bounds = [Math.round(rect.left), Math.round(rect.top),
                     Math.round(rect.right), Math.round(rect.bottom)];
    }

    if (el.href) node.href = el.href;
    if (el.type) node.inputType = el.type;

    const children = [];
    for (const child of el.children) {
      const c = buildTree(child, depth + 1);
      if (c) children.push(...(Array.isArray(c) ? c : [c]));
    }
    if (children.length > 0) node.children = children;

    return node;
  }

  return JSON.stringify(buildTree(document.body, 0), null, 2);
}

// Click
async function click(args) {
  const tab = await getTab(args.tabId);
  const { coordinate, button = 'left' } = args;
  if (!coordinate) throw new Error('coordinate is required');

  await browser.scripting.executeScript({
    target: { tabId: tab.id },
    func: (x, y, btn) => {
      const el = document.elementFromPoint(x, y);
      if (!el) return;
      const event = new MouseEvent(btn === 'right' ? 'contextmenu' : 'click', {
        bubbles: true, cancelable: true, view: window,
        clientX: x, clientY: y, button: btn === 'right' ? 2 : 0
      });
      el.dispatchEvent(event);
      if (btn === 'left' && el.click) el.click();
    },
    args: [coordinate[0], coordinate[1], button]
  });

  return { content: [{ type: 'text', text: `Clicked at [${coordinate[0]}, ${coordinate[1]}]` }] };
}

// Type
async function typeText(args) {
  const tab = await getTab(args.tabId);
  const { text, coordinate } = args;

  await browser.scripting.executeScript({
    target: { tabId: tab.id },
    func: (txt, coord) => {
      let el = document.activeElement;
      if (coord) {
        const clickEl = document.elementFromPoint(coord[0], coord[1]);
        if (clickEl) { clickEl.focus(); clickEl.click(); el = clickEl; }
      }
      if (el && (el.tagName === 'INPUT' || el.tagName === 'TEXTAREA' || el.isContentEditable)) {
        if (el.value !== undefined) {
          el.value = txt;
          el.dispatchEvent(new Event('input', { bubbles: true }));
          el.dispatchEvent(new Event('change', { bubbles: true }));
        } else if (el.isContentEditable) {
          el.textContent = txt;
          el.dispatchEvent(new Event('input', { bubbles: true }));
        }
      }
    },
    args: [text, coordinate]
  });

  return { content: [{ type: 'text', text: `Typed: "${text.slice(0, 50)}"` }] };
}

// Scroll
async function scroll(args) {
  const tab = await getTab(args.tabId);
  const { direction, amount = 300, coordinate } = args;
  const dx = direction === 'left' ? -amount : direction === 'right' ? amount : 0;
  const dy = direction === 'up' ? -amount : direction === 'down' ? amount : 0;

  await browser.scripting.executeScript({
    target: { tabId: tab.id },
    func: (x, y, coord) => {
      const target = coord ? document.elementFromPoint(coord[0], coord[1]) : window;
      (target || window).scrollBy(x, y);
    },
    args: [dx, dy, coordinate]
  });

  return { content: [{ type: 'text', text: `Scrolled ${direction}` }] };
}

// Execute JS
async function executeJs(args) {
  const tab = await getTab(args.tabId);
  const results = await browser.scripting.executeScript({
    target: { tabId: tab.id },
    func: (code) => eval(code),
    args: [args.code]
  });

  let text;
  try { text = JSON.stringify(results[0]?.result, null, 2); }
  catch { text = String(results[0]?.result); }

  return { content: [{ type: 'text', text: text || 'undefined' }] };
}

// Find elements
async function findElements(args) {
  const tab = await getTab(args.tabId);
  const results = await browser.scripting.executeScript({
    target: { tabId: tab.id },
    func: (query) => {
      const results = [];
      const q = query.toLowerCase();

      // Try CSS selector
      try {
        for (const el of document.querySelectorAll(query)) {
          const rect = el.getBoundingClientRect();
          if (rect.width > 0 && rect.height > 0) {
            results.push({
              tag: el.tagName.toLowerCase(),
              text: (el.textContent || '').trim().slice(0, 80),
              bounds: [Math.round(rect.left), Math.round(rect.top),
                       Math.round(rect.right), Math.round(rect.bottom)]
            });
            if (results.length >= 20) break;
          }
        }
        if (results.length > 0) return JSON.stringify(results, null, 2);
      } catch {}

      // Text search
      const walker = document.createTreeWalker(document.body, NodeFilter.SHOW_ELEMENT);
      let node;
      while ((node = walker.nextNode()) && results.length < 20) {
        const text = (node.textContent || '').toLowerCase();
        if (text.includes(q)) {
          const rect = node.getBoundingClientRect();
          if (rect.width > 0 && rect.height > 0) {
            results.push({
              tag: node.tagName.toLowerCase(),
              text: (node.textContent || '').trim().slice(0, 80),
              bounds: [Math.round(rect.left), Math.round(rect.top),
                       Math.round(rect.right), Math.round(rect.bottom)]
            });
          }
        }
      }
      return JSON.stringify(results, null, 2);
    },
    args: [args.query]
  });

  return { content: [{ type: 'text', text: results[0]?.result || '[]' }] };
}

// Form input
async function formInput(args) {
  const tab = await getTab(args.tabId);
  await browser.scripting.executeScript({
    target: { tabId: tab.id },
    func: (selector, value) => {
      const el = document.querySelector(selector);
      if (!el) return;
      if (el.type === 'checkbox' || el.type === 'radio') el.checked = !!value;
      else el.value = value;
      el.dispatchEvent(new Event('input', { bubbles: true }));
      el.dispatchEvent(new Event('change', { bubbles: true }));
    },
    args: [args.selector, args.value]
  });

  return { content: [{ type: 'text', text: `Set ${args.selector}` }] };
}

// Tabs
async function tabsList() {
  const tabs = await browser.tabs.query({ currentWindow: true });
  return {
    content: [{
      type: 'text',
      text: JSON.stringify(tabs.map(t => ({
        id: t.id, title: t.title, url: t.url, active: t.active
      })), null, 2)
    }]
  };
}

async function tabsCreate(args) {
  const tab = await browser.tabs.create({ url: args.url || 'about:blank', active: true });
  return { content: [{ type: 'text', text: `Created tab ${tab.id}` }] };
}

async function tabsClose(args) {
  const tab = await getTab(args.tabId);
  await browser.tabs.remove(tab.id);
  return { content: [{ type: 'text', text: 'Tab closed' }] };
}

async function getPageText(args) {
  const tab = await getTab(args.tabId);
  const results = await browser.scripting.executeScript({
    target: { tabId: tab.id },
    func: () => document.body.innerText.slice(0, 50000)
  });
  return { content: [{ type: 'text', text: results[0]?.result || '' }] };
}

// Action click - manual reconnect
browser.action.onClicked.addListener(() => {
  console.log('[MCP] Manual reconnect triggered');
  connect();
});

// Auto-connect on startup
connect();

console.log('[MCP] MV3 Background service worker loaded');
