# Quick Start Guide

## Prerequisites

- Elixir 1.17+
- Erlang/OTP 27+
- VSCode (for extension)

## 60-Second Setup

```bash
# Clone and install
git clone https://github.com/hyperpolymath/claude-firefox-lsp.git
cd claude-firefox-lsp
./install.sh

# Install VSCode extension
code --install-extension vscode-extension/*.vsix

# Start using!
# Open a project in VSCode and the LSP will activate automatically
```

## Configuration

Add to your VSCode `settings.json`:

```json
{
  "lsp.serverPath": "/path/to/claude-firefox-lsp"
}
```

See [examples/vscode-settings.json](./examples/vscode-settings.json) for more options.

## Troubleshooting

**LSP not starting?**
- Run `mix test` to verify installation
- Check Output panel in VSCode (View → Output → select LSP)

**Completions not working?**
- Ensure adapters are detected: `mix run -e 'IO.inspect(Adapters.detect())'`

## Next Steps

- Read [USAGE.md](./USAGE.md) for detailed features
- Check [examples/](./examples/) for configurations
- Join discussions on GitHub Issues
