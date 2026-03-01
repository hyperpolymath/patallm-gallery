# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

defmodule ClaudeFirefox.Adapters.Firefox do
  @moduledoc """
  Firefox browser adapter using the Marionette protocol.

  Marionette is Firefox's native automation protocol, similar to Chrome DevTools Protocol.
  It communicates over a TCP socket (default port 2828) when Firefox is started with
  the `-marionette` flag.

  ## Setup

  Start Firefox with Marionette enabled:

      firefox -marionette -headless

  Or configure via about:config:
  - Set `marionette.enabled` to true
  - Set `marionette.port` to 2828 (default)

  ## Protocol

  Marionette uses a JSON-based protocol over TCP. Messages are formatted as:
  - Length (bytes) : JSON payload

  Reference: https://firefox-source-docs.mozilla.org/testing/marionette/Protocol.html
  """

  use GenServer
  require Logger

  @behaviour ClaudeFirefox.Adapters.Behaviour

  @default_host "127.0.0.1"
  @default_port 2828
  @default_timeout 5000

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl ClaudeFirefox.Adapters.Behaviour
  def detect do
    case System.find_executable("firefox") do
      nil ->
        {:ok, false}

      path ->
        Logger.debug("Found Firefox at #{path}")
        {:ok, true}
    end
  end

  @impl ClaudeFirefox.Adapters.Behaviour
  def navigate(url, opts \\ []) do
    GenServer.call(__MODULE__, {:navigate, url, opts}, timeout(opts))
  end

  @impl ClaudeFirefox.Adapters.Behaviour
  def click(selector, opts \\ []) do
    GenServer.call(__MODULE__, {:click, selector, opts}, timeout(opts))
  end

  @impl ClaudeFirefox.Adapters.Behaviour
  def type_text(selector, text, opts \\ []) do
    GenServer.call(__MODULE__, {:type_text, selector, text, opts}, timeout(opts))
  end

  @impl ClaudeFirefox.Adapters.Behaviour
  def screenshot(opts \\ []) do
    GenServer.call(__MODULE__, {:screenshot, opts}, timeout(opts))
  end

  @impl ClaudeFirefox.Adapters.Behaviour
  def execute_js(javascript, opts \\ []) do
    GenServer.call(__MODULE__, {:execute_js, javascript, opts}, timeout(opts))
  end

  @impl ClaudeFirefox.Adapters.Behaviour
  def get_content(opts \\ []) do
    GenServer.call(__MODULE__, {:get_content, opts}, timeout(opts))
  end

  @impl ClaudeFirefox.Adapters.Behaviour
  def metadata do
    %{
      name: "Firefox",
      version: get_version(),
      protocol: "Marionette",
      capabilities: [
        :navigate,
        :click,
        :type_text,
        :screenshot,
        :execute_js,
        :get_content,
        :find_element,
        :cookies,
        :window_management
      ]
    }
  end

  # Server callbacks

  @impl true
  def init(opts) do
    host = Keyword.get(opts, :host, @default_host)
    port = Keyword.get(opts, :port, @default_port)

    state = %{
      host: host,
      port: port,
      socket: nil,
      session_id: nil,
      message_id: 0
    }

    {:ok, state, {:continue, :connect}}
  end

  @impl true
  def handle_continue(:connect, state) do
    case connect(state) do
      {:ok, new_state} ->
        {:noreply, new_state}

      {:error, reason} ->
        Logger.warning("Failed to connect to Firefox: #{inspect(reason)}")
        Logger.info("Please start Firefox with: firefox -marionette -headless")
        {:noreply, state}
    end
  end

  @impl true
  def handle_call({:navigate, url, _opts}, _from, state) do
    case send_command(state, "WebDriver:Navigate", %{url: url}) do
      {:ok, _response, new_state} ->
        {:reply, {:ok, %{url: url}}, new_state}

      {:error, reason, new_state} ->
        {:reply, {:error, reason}, new_state}
    end
  end

  @impl true
  def handle_call({:click, selector, opts}, _from, state) do
    with {:ok, element, state} <- find_element(state, selector, opts),
         {:ok, _response, state} <- send_command(state, "WebDriver:ElementClick", %{id: element}) do
      {:reply, {:ok, %{clicked: true}}, state}
    else
      {:error, reason, state} -> {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:type_text, selector, text, opts}, _from, state) do
    clear = Keyword.get(opts, :clear, false)

    with {:ok, element, state} <- find_element(state, selector, opts),
         {:ok, state} <- maybe_clear_element(state, element, clear),
         {:ok, _response, state} <-
           send_command(state, "WebDriver:ElementSendKeys", %{id: element, text: text}) do
      {:reply, {:ok, %{typed: true}}, state}
    else
      {:error, reason, state} -> {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:screenshot, opts}, _from, state) do
    selector = Keyword.get(opts, :selector)

    params =
      if selector do
        with {:ok, element, state} <- find_element(state, selector, opts) do
          %{id: element, full: false}
        else
          _ -> %{full: true}
        end
      else
        %{full: true}
      end

    case send_command(state, "WebDriver:TakeScreenshot", params) do
      {:ok, %{"value" => base64_data}, new_state} ->
        binary_data = Base.decode64!(base64_data)
        {:reply, {:ok, binary_data}, new_state}

      {:error, reason, new_state} ->
        {:reply, {:error, reason}, new_state}
    end
  end

  @impl true
  def handle_call({:execute_js, javascript, _opts}, _from, state) do
    params = %{
      script: javascript,
      args: []
    }

    case send_command(state, "WebDriver:ExecuteScript", params) do
      {:ok, %{"value" => result}, new_state} ->
        {:reply, {:ok, result}, new_state}

      {:error, reason, new_state} ->
        {:reply, {:error, reason}, new_state}
    end
  end

  @impl true
  def handle_call({:get_content, opts}, _from, state) do
    format = Keyword.get(opts, :format, :html)

    script =
      case format do
        :html -> "return document.documentElement.outerHTML;"
        :text -> "return document.body.innerText;"
        :dom -> "return document.documentElement.innerHTML;"
      end

    case send_command(state, "WebDriver:ExecuteScript", %{script: script, args: []}) do
      {:ok, %{"value" => content}, new_state} ->
        {:reply, {:ok, content}, new_state}

      {:error, reason, new_state} ->
        {:reply, {:error, reason}, new_state}
    end
  end

  # Private functions

  defp connect(state) do
    case :gen_tcp.connect(
           String.to_charlist(state.host),
           state.port,
           [:binary, active: false, packet: :raw],
           @default_timeout
         ) do
      {:ok, socket} ->
        # Read initial handshake
        case read_message(socket) do
          {:ok, _handshake} ->
            # Start a new session
            case send_command(%{state | socket: socket}, "WebDriver:NewSession", %{}) do
              {:ok, %{"sessionId" => session_id}, new_state} ->
                Logger.info("Connected to Firefox Marionette (session: #{session_id})")
                {:ok, %{new_state | session_id: session_id}}

              {:error, reason, _state} ->
                {:error, reason}
            end

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp send_command(state, command, params) do
    if state.socket == nil do
      {:error, "Not connected to Firefox", state}
    else
      message_id = state.message_id + 1

      message = %{
        "id" => message_id,
        "name" => command,
        "parameters" => params
      }

      payload = Jason.encode!(message)
      packet = "#{byte_size(payload)}:#{payload}"

      case :gen_tcp.send(state.socket, packet) do
        :ok ->
          case read_message(state.socket) do
            {:ok, response} ->
              {:ok, response, %{state | message_id: message_id}}

            {:error, reason} ->
              {:error, reason, state}
          end

        {:error, reason} ->
          {:error, inspect(reason), state}
      end
    end
  end

  defp read_message(socket) do
    case :gen_tcp.recv(socket, 0, @default_timeout) do
      {:ok, data} ->
        # Parse "length:payload" format
        case String.split(data, ":", parts: 2) do
          [_length, json] ->
            case Jason.decode(json) do
              {:ok, parsed} -> {:ok, parsed}
              {:error, _} -> {:error, "Invalid JSON response"}
            end

          _ ->
            {:error, "Invalid message format"}
        end

      {:error, reason} ->
        {:error, inspect(reason)}
    end
  end

  defp find_element(state, selector, opts) do
    strategy = Keyword.get(opts, :strategy, :css)

    strategy_name =
      case strategy do
        :css -> "css selector"
        :xpath -> "xpath"
        :id -> "id"
        :name -> "name"
        :tag -> "tag name"
      end

    params = %{
      using: strategy_name,
      value: selector
    }

    case send_command(state, "WebDriver:FindElement", params) do
      {:ok, %{"value" => element}, new_state} ->
        {:ok, element, new_state}

      {:error, reason, new_state} ->
        {:error, reason, new_state}
    end
  end

  defp maybe_clear_element(state, element, false), do: {:ok, state}

  defp maybe_clear_element(state, element, true) do
    case send_command(state, "WebDriver:ElementClear", %{id: element}) do
      {:ok, _response, new_state} -> {:ok, new_state}
      {:error, reason, new_state} -> {:error, reason, new_state}
    end
  end

  defp get_version do
    case System.cmd("firefox", ["--version"], stderr_to_stdout: true) do
      {output, 0} ->
        output
        |> String.trim()
        |> String.replace("Mozilla Firefox ", "")

      _ ->
        nil
    end
  end

  defp timeout(opts), do: Keyword.get(opts, :timeout, @default_timeout)
end
