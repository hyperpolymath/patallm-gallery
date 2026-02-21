# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

defmodule ClaudeFirefox.LSP do
  @moduledoc """
  Claude Firefox LSP - Language Server Protocol for Firefox browser automation.

  Provides LSP-based browser automation capabilities for Firefox, enabling
  editor integration for web testing, scraping, and automation tasks.

  ## Features

  - Navigate to URLs
  - Click elements (CSS selector, XPath)
  - Type text into form fields
  - Take screenshots
  - Execute JavaScript
  - Extract page content

  ## Architecture

  Built on the Marionette protocol, Firefox's native automation interface.
  Uses GenLSP framework for LSP communication.

  ## Usage

      # Start the LSP server
      ClaudeFirefox.LSP.start()

      # In your editor (VS Code, Neovim, etc.)
      # Use LSP commands for browser automation
  """

  @version Mix.Project.config()[:version]

  @doc """
  Returns the LSP server version.
  """
  def version, do: @version

  @doc """
  List available browser adapters.
  """
  def adapters do
    [
      ClaudeFirefox.Adapters.Firefox
    ]
  end

  @doc """
  Detect which browsers are available on the system.
  """
  def detect_browsers do
    adapters()
    |> Enum.map(fn adapter ->
      case adapter.detect() do
        {:ok, true} -> {adapter, adapter.metadata()}
        _ -> nil
      end
    end)
    |> Enum.reject(&is_nil/1)
  end
end
