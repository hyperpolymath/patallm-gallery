# Usage Guide

> Comprehensive guide for using claude-firefox-lsp across VSCode, Neovim, and Emacs

## Table of Contents

- [VSCode Setup](#vscode-setup)
- [Neovim Setup](#neovim-setup)
- [Emacs Setup](#emacs-setup)
- [Configuration](#configuration)
- [Commands](#commands)
- [Troubleshooting](#troubleshooting)
- [Adapter-Specific Notes](#adapter-specific-notes)

## VSCode Setup

### Installation

1. **Install the LSP Server:**
   ```bash
   git clone https://github.com/hyperpolymath/claude-firefox-lsp.git
   cd claude-firefox-lsp
   ./install.sh
   ```

2. **Install VSCode Extension:**
   ```bash
   cd vscode-extension
   npm install
   npm run compile
   code --install-extension *.vsix
   ```

### Features

The VSCode extension provides:

- **Firefox Automation**: Control Firefox via Marionette protocol
- **Browser Inspection**: View DOM, execute JavaScript
- **Navigation Control**: Navigate pages, manage tabs
- **Diagnostics**: Script errors, connection issues
- **Hover Documentation**: Element info, JavaScript API docs
- **Commands**: Execute browser actions directly from editor

### Available Commands

Access via Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`):

- **Firefox: Connect** - Connect to Firefox instance
- **Firefox: Navigate** - Navigate to URL
- **Firefox: Execute Script** - Run JavaScript in page
- **Firefox: Get Element** - Find element by selector
- **Firefox: Take Screenshot** - Capture page screenshot
- **Firefox: Get Page Source** - View HTML source
- **Firefox: Close Tab** - Close active tab
- **Firefox: Disconnect** - Close connection

### Settings

Add to your workspace or user `settings.json`:

```json
{
  "lsp.serverPath": "/path/to/claude-firefox-lsp",
  "lsp.trace.server": "verbose",
  "lsp.firefox.host": "localhost",
  "lsp.firefox.port": 2828,
  "lsp.firefox.autoConnect": false
}
```

## Neovim Setup

### Using nvim-lspconfig

Add to your Neovim configuration:

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

-- Register claude-firefox-lsp if not already defined
if not configs.claude_firefox_lsp then
  configs.claude_firefox_lsp = {
    default_config = {
      cmd = {'/path/to/claude-firefox-lsp/_build/prod/rel/claude_firefox_lsp/bin/claude_firefox_lsp'},
      filetypes = {'javascript', 'html', 'css'},
      root_dir = lspconfig.util.root_pattern('.git', 'package.json'),
      settings = {
        firefox = {
          host = 'localhost',
          port = 2828,
          autoConnect = false
        }
      }
    }
  }
end

-- Setup the LSP
lspconfig.claude_firefox_lsp.setup({
  on_attach = function(client, bufnr)
    local opts = { noremap=true, silent=true, buffer=bufnr }

    -- Custom commands
    vim.api.nvim_buf_create_user_command(bufnr, 'FirefoxConnect', function()
      vim.lsp.buf.execute_command({command = 'firefox.connect'})
    end, {})

    vim.api.nvim_buf_create_user_command(bufnr, 'FirefoxNavigate', function(args)
      vim.lsp.buf.execute_command({
        command = 'firefox.navigate',
        arguments = {{url = args.args}}
      })
    end, {nargs = 1})

    vim.api.nvim_buf_create_user_command(bufnr, 'FirefoxExecute', function()
      local script = vim.fn.input('Script: ')
      vim.lsp.buf.execute_command({
        command = 'firefox.execute',
        arguments = {{script = script}}
      })
    end, {})
  end,
  capabilities = require('cmp_nvim_lsp').default_capabilities()
})
```

## Emacs Setup

### Using lsp-mode

Add to your Emacs configuration:

```elisp
(use-package lsp-mode
  :hook ((js-mode html-mode css-mode) . lsp)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     '("/path/to/claude-firefox-lsp/_build/prod/rel/claude_firefox_lsp/bin/claude_firefox_lsp"))
    :major-modes '(js-mode html-mode css-mode)
    :server-id 'claude-firefox-lsp
    :initialization-options (lambda ()
                             '(:host "localhost"
                               :port 2828
                               :autoConnect nil)))))

;; Custom commands
(defun firefox-connect ()
  "Connect to Firefox."
  (interactive)
  (lsp-execute-command "firefox.connect"))

(defun firefox-navigate (url)
  "Navigate to URL."
  (interactive "sURL: ")
  (lsp-execute-command "firefox.navigate" (vector `(:url ,url))))

(defun firefox-execute (script)
  "Execute JavaScript."
  (interactive "sScript: ")
  (lsp-execute-command "firefox.execute" (vector `(:script ,script))))

(define-key lsp-mode-map (kbd "C-c f c") 'firefox-connect)
(define-key lsp-mode-map (kbd "C-c f n") 'firefox-navigate)
(define-key lsp-mode-map (kbd "C-c f e") 'firefox-execute)
```

## Configuration

### Server Configuration

Create `.claude-firefox-lsp.json` in your project root:

```json
{
  "firefox": {
    "host": "localhost",
    "port": 2828,
    "autoConnect": false,
    "timeout": 30000,
    "enableDebugger": false
  },
  "marionette": {
    "enableWebDriver": true,
    "acceptInsecureCerts": false,
    "pageLoadStrategy": "normal"
  }
}
```

### Environment Variables

```bash
# Firefox Marionette
export FIREFOX_BIN=/usr/bin/firefox
export MARIONETTE_HOST=localhost
export MARIONETTE_PORT=2828

# Firefox profile
export FIREFOX_PROFILE=~/.mozilla/firefox/marionette.default
```

### Starting Firefox with Marionette

```bash
# Enable remote debugging
firefox --marionette --start-debugger-server 2828

# Or with specific profile
firefox --marionette --profile ~/.mozilla/firefox/marionette.default
```

## Commands

### LSP Commands

#### firefox.connect
Connect to Firefox instance.

**Parameters:**
- `host` (optional): Firefox host (default: localhost)
- `port` (optional): Marionette port (default: 2828)

**Returns:** Connection status

**Example (Neovim):**
```lua
vim.lsp.buf.execute_command({
  command = 'firefox.connect',
  arguments = {{host = 'localhost', port = 2828}}
})
```

#### firefox.navigate
Navigate to URL.

**Parameters:**
- `url`: Target URL

**Returns:** Navigation status

**Example (Neovim):**
```lua
vim.lsp.buf.execute_command({
  command = 'firefox.navigate',
  arguments = {{url = 'https://example.com'}}
})
```

#### firefox.execute
Execute JavaScript in page context.

**Parameters:**
- `script`: JavaScript code
- `args` (optional): Script arguments

**Returns:** Script result

**Example (Neovim):**
```lua
vim.lsp.buf.execute_command({
  command = 'firefox.execute',
  arguments = {{
    script = 'return document.title;'
  }}
})
```

#### firefox.getElement
Find element by CSS selector.

**Parameters:**
- `selector`: CSS selector

**Returns:** Element reference

#### firefox.getPageSource
Get page HTML source.

**Parameters:** None

**Returns:** HTML source code

#### firefox.takeScreenshot
Capture page screenshot.

**Parameters:**
- `fullPage` (optional): Capture full page (default: false)

**Returns:** Base64-encoded PNG image

#### firefox.closeTab
Close active tab.

**Parameters:** None

**Returns:** Close status

#### firefox.disconnect
Close connection to Firefox.

**Parameters:** None

**Returns:** Disconnect status

## Troubleshooting

### Connection Refused

**Symptoms:** "Connection refused" or "Unable to connect" error.

**Solutions:**

1. **Verify Firefox is running with Marionette:**
   ```bash
   ps aux | grep firefox
   # Should show --marionette flag
   ```

2. **Start Firefox with Marionette:**
   ```bash
   firefox --marionette --start-debugger-server 2828
   ```

3. **Check port is listening:**
   ```bash
   netstat -an | grep 2828
   # or
   lsof -i :2828
   ```

4. **Test connection manually:**
   ```bash
   telnet localhost 2828
   ```

### Script Execution Errors

**Symptoms:** JavaScript execution fails.

**Solutions:**

1. **Check script syntax:**
   - Ensure valid JavaScript
   - Use `return` statement for values

2. **Verify page context:**
   ```lua
   -- Get current URL first
   vim.lsp.buf.execute_command({
     command = 'firefox.execute',
     arguments = {{script = 'return window.location.href;'}}
   })
   ```

3. **Check for CSP restrictions:**
   - Content Security Policy may block script execution
   - Check browser console for errors

### Timeout Errors

**Symptoms:** "Operation timed out" error.

**Solutions:**

1. **Increase timeout:**
   ```json
   {"firefox": {"timeout": 60000}}
   ```

2. **Check page load time:**
   - Some pages may take longer to load
   - Use explicit waits for elements

## Adapter-Specific Notes

### Firefox Marionette

**Detection:** Firefox running with `--marionette` flag on port 2828

**Features:**
- WebDriver-compatible protocol
- JavaScript execution
- Element interaction
- Screenshot capture
- Cookie management
- Window/tab management

**Configuration:**
```json
{
  "adapters": {
    "marionette": {
      "host": "localhost",
      "port": 2828,
      "enableWebDriver": true,
      "acceptInsecureCerts": false,
      "pageLoadStrategy": "normal",
      "strictFileInteractability": false
    }
  }
}
```

**Marionette Protocol Capabilities:**
- `acceptInsecureCerts`: Accept self-signed certificates
- `pageLoadStrategy`: "normal", "eager", or "none"
- `strictFileInteractability`: Strict element interaction checks
- `unhandledPromptBehavior`: How to handle alerts ("dismiss", "accept")

**Known Issues:**
- Headless mode has limited screenshot capabilities
- Some Firefox-specific features not available via WebDriver
- Private browsing mode may have restrictions

**Firefox Preferences for Testing:**

Create a custom profile with these preferences in `prefs.js`:

```javascript
user_pref("marionette.port", 2828);
user_pref("devtools.debugger.remote-enabled", true);
user_pref("devtools.chrome.enabled", true);
```

**Automation Best Practices:**

1. **Use explicit waits:**
   ```lua
   -- Wait for element to appear
   vim.lsp.buf.execute_command({
     command = 'firefox.execute',
     arguments = {{
       script = [[
         return new Promise((resolve) => {
           const interval = setInterval(() => {
             const el = document.querySelector('#my-element');
             if (el) {
               clearInterval(interval);
               resolve(el.textContent);
             }
           }, 100);
         });
       ]]
     }}
   })
   ```

2. **Handle navigation timing:**
   ```lua
   -- Navigate and wait for load
   vim.lsp.buf.execute_command({command = 'firefox.navigate', arguments = {{url = 'https://example.com'}}})
   vim.wait(2000)  -- Wait 2 seconds for page load
   ```

3. **Clean up connections:**
   ```lua
   -- Always disconnect when done
   vim.lsp.buf.execute_command({command = 'firefox.disconnect'})
   ```

## Additional Resources

- **GitHub Repository:** https://github.com/hyperpolymath/claude-firefox-lsp
- **Issue Tracker:** https://github.com/hyperpolymath/claude-firefox-lsp/issues
- **Examples:** See `examples/` directory for sample configurations
- **Marionette Protocol:** https://firefox-source-docs.mozilla.org/testing/marionette/

## License

PMPL-1.0-or-later
