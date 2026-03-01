# Claude Firefox LSP - VS Code Extension

VS Code extension for Firefox browser automation via Language Server Protocol.

## Features

- **Navigate**: Load URLs in Firefox
- **Click**: Click elements by CSS selector or XPath
- **Type Text**: Enter text into form fields
- **Screenshot**: Capture page screenshots
- **Execute JavaScript**: Run custom JavaScript in page context
- **Get Content**: Extract page HTML, text, or DOM

## Requirements

- Firefox browser
- claude-firefox-lsp server installed
- Firefox started with `-marionette` flag

## Extension Settings

- `claude-firefox-lsp.serverPath`: Path to claude-firefox-lsp executable
- `claude-firefox-lsp.trace.server`: LSP trace level (off, messages, verbose)

## Usage

1. Start Firefox with Marionette:
   ```bash
   firefox -marionette -headless
   ```

2. Open VS Code command palette (Ctrl+Shift+P)

3. Run commands:
   - `Firefox: Navigate to URL`
   - `Firefox: Click Element`
   - `Firefox: Type Text`
   - `Firefox: Take Screenshot`
   - `Firefox: Execute JavaScript`
   - `Firefox: Get Page Content`

## Development

```bash
# Install dependencies
npm install

# Compile TypeScript
npm run compile

# Package extension
npm run package
```

## License

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell
