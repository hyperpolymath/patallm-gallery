#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Install Claude Firefox MCP

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

echo "Installing Claude Firefox MCP..."

# Check for Deno
if ! command -v deno &> /dev/null; then
    echo "Error: Deno is required but not installed."
    echo "Install with: curl -fsSL https://deno.land/install.sh | sh"
    exit 1
fi

# Create bin directory
mkdir -p ~/.local/bin

# Create wrapper script
cat > ~/.local/bin/claude-firefox-mcp << 'EOF'
#!/bin/bash
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec deno run --allow-net --allow-read --allow-env "$HOME/claude-firefox-mcp/host/server.ts" "$@"
EOF

chmod +x ~/.local/bin/claude-firefox-mcp

# Symlink project if not already there
if [ ! -d "$HOME/claude-firefox-mcp" ]; then
    ln -sf "$PROJECT_DIR" "$HOME/claude-firefox-mcp"
fi

# Add to Claude Code settings
CLAUDE_SETTINGS="$HOME/.claude/settings.json"
if [ -f "$CLAUDE_SETTINGS" ]; then
    # Check if already configured
    if grep -q "claude-firefox-mcp" "$CLAUDE_SETTINGS" 2>/dev/null; then
        echo "MCP server already configured in Claude settings"
    else
        echo ""
        echo "Add to your Claude settings ($CLAUDE_SETTINGS):"
        echo ""
        cat << 'SETTINGS'
{
  "mcpServers": {
    "firefox": {
      "command": "claude-firefox-mcp",
      "args": []
    }
  }
}
SETTINGS
    fi
else
    mkdir -p "$HOME/.claude"
    cat > "$CLAUDE_SETTINGS" << 'SETTINGS'
{
  "mcpServers": {
    "firefox": {
      "command": "claude-firefox-mcp",
      "args": []
    }
  }
}
SETTINGS
    echo "Created Claude settings with Firefox MCP configured"
fi

echo ""
echo "Installation complete!"
echo ""
echo "Next steps:"
echo "1. Load the extension in Firefox:"
echo "   - Go to about:debugging"
echo "   - Click 'This Firefox'"
echo "   - Click 'Load Temporary Add-on'"
echo "   - Select: $PROJECT_DIR/extension/manifest.json"
echo ""
echo "2. Start using Claude Code - the Firefox MCP will be available"
echo ""
echo "To test manually:"
echo "  ~/.local/bin/claude-firefox-mcp"
