#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Quick installation script

set -e

echo "Installing LSP server dependencies..."
mix deps.get
mix compile

echo "Building VSCode extension..."
cd vscode-extension
npm install
npm run compile

echo "✓ Installation complete!"
echo ""
echo "To install VSCode extension:"
echo "  1. Open VSCode"
echo "  2. Press Ctrl+Shift+P (Cmd+Shift+P on Mac)"
echo "  3. Type 'Extensions: Install from VSIX'"
echo "  4. Select the .vsix file from vscode-extension/"
