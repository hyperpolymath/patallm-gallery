# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

defmodule ClaudeFirefox.Adapters.Behaviour do
  @moduledoc """
  Behaviour defining the contract for browser adapters.

  Each adapter implements this behaviour to provide a consistent interface
  for detecting, controlling, and automating browsers.

  ## Example

      defmodule ClaudeFirefox.Adapters.Firefox do
        use GenServer
        @behaviour ClaudeFirefox.Adapters.Behaviour

        @impl true
        def detect() do
          case System.find_executable("firefox") do
            nil -> {:ok, false}
            _path -> {:ok, true}
          end
        end

        @impl true
        def navigate(url, opts) do
          # Send Marionette protocol command
        end
      end
  """

  @type url :: String.t()
  @type selector :: String.t()
  @type javascript :: String.t()
  @type opts :: keyword()
  @type result :: {:ok, term()} | {:error, String.t()}

  @doc """
  Detect if this browser is installed and available.

  Returns `{:ok, true}` if the browser is detected, `{:ok, false}` otherwise.
  """
  @callback detect() :: {:ok, boolean()} | {:error, String.t()}

  @doc """
  Navigate to a URL.

  ## Options

  - `:timeout` - Maximum time to wait for page load (milliseconds)
  - `:wait_for` - Selector or condition to wait for before returning
  """
  @callback navigate(url, opts) :: result

  @doc """
  Click an element specified by selector.

  ## Options

  - `:timeout` - Maximum time to wait for element (milliseconds)
  - `:button` - Mouse button (`:left`, `:right`, `:middle`)
  """
  @callback click(selector, opts) :: result

  @doc """
  Type text into an element specified by selector.

  ## Options

  - `:timeout` - Maximum time to wait for element (milliseconds)
  - `:clear` - Clear existing text before typing (boolean)
  """
  @callback type_text(selector, text :: String.t(), opts) :: result

  @doc """
  Take a screenshot of the current page or element.

  ## Options

  - `:selector` - Element selector (captures full page if omitted)
  - `:format` - Image format (`:png`, `:jpeg`)
  - `:quality` - JPEG quality (1-100)

  Returns `{:ok, binary()}` with image data.
  """
  @callback screenshot(opts) :: {:ok, binary()} | {:error, String.t()}

  @doc """
  Execute JavaScript in the page context.

  Returns `{:ok, result}` where result is the JS return value (serialized as JSON).
  """
  @callback execute_js(javascript, opts) :: result

  @doc """
  Get page content (HTML, text, or DOM representation).

  ## Options

  - `:format` - Content format (`:html`, `:text`, `:dom`)
  """
  @callback get_content(opts) :: {:ok, String.t()} | {:error, String.t()}

  @doc """
  Get browser metadata (name, version, capabilities).
  """
  @callback metadata() :: %{
              name: String.t(),
              version: String.t() | nil,
              protocol: String.t(),
              capabilities: [atom()]
            }
end
