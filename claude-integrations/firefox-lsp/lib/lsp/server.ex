# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

defmodule ClaudeFirefox.LSP.Server do
  @moduledoc """
  GenLSP server implementation for ClaudeFirefox.

  Handles LSP protocol messages and delegates to appropriate handlers.
  """
  use GenLSP

  require Logger

  alias ClaudeFirefox.Adapters.Firefox
  alias ClaudeFirefox.LSP.Handlers.{Completion, Diagnostics, Hover}

  @impl GenLSP
  def handle_info(_msg, lsp), do: {:noreply, lsp}

  def start_link(args) do
    GenLSP.start_link(__MODULE__, args, [])
  end

  @impl GenLSP
  def init(_lsp, _args) do
    {:ok, %{project_path: nil, browser: :firefox, session_active: false, documents: %{}}}
  end

  @impl GenLSP
  def handle_request(%{"method" => "initialize", "params" => params}, lsp) do
    project_path = get_in(params, ["rootUri"]) |> parse_uri()

    Logger.info("Initializing LSP for project: #{inspect(project_path)}")

    # Detect available browsers
    available_browsers = ClaudeFirefox.LSP.detect_browsers()
    Logger.info("Available browsers: #{inspect(available_browsers)}")

    server_capabilities = %{
      "textDocumentSync" => %{
        "openClose" => true,
        "change" => 1,  # Full sync
        "save" => %{"includeText" => false}
      },
      "completionProvider" => %{
        "triggerCharacters" => [".", "(", "["],
        "resolveProvider" => false
      },
      "hoverProvider" => true,
      "executeCommandProvider" => %{
        "commands" => [
          "firefox.navigate",
          "firefox.click",
          "firefox.typeText",
          "firefox.screenshot",
          "firefox.executeJs",
          "firefox.getContent",
          "firefox.detectBrowsers"
        ]
      }
    }

    result = %{
      "capabilities" => server_capabilities,
      "serverInfo" => %{
        "name" => "Claude Firefox LSP",
        "version" => ClaudeFirefox.LSP.version()
      }
    }

    new_state = Map.merge(lsp, %{project_path: project_path})
    {:reply, result, new_state}
  end

  @impl GenLSP
  def handle_request(%{"method" => "textDocument/completion", "params" => params}, lsp) do
    completions = Completion.handle(params, lsp.assigns)
    {:reply, completions, lsp}
  end

  @impl GenLSP
  def handle_request(%{"method" => "textDocument/hover", "params" => params}, lsp) do
    hover_info = Hover.handle(params, lsp.assigns)
    {:reply, hover_info, lsp}
  end

  @impl GenLSP
  def handle_request(%{"method" => "workspace/executeCommand", "params" => params}, lsp) do
    command = params["command"]
    args = params["arguments"] || []
    result = execute_command(command, args, lsp.assigns)
    {:reply, result, lsp}
  end

  @impl GenLSP
  def handle_request(_request, lsp) do
    {:reply, nil, lsp}
  end

  @impl GenLSP
  def handle_notification(%{"method" => "initialized"}, lsp) do
    Logger.info("LSP server initialized")
    {:noreply, lsp}
  end

  @impl GenLSP
  def handle_notification(%{"method" => "textDocument/didOpen", "params" => params}, lsp) do
    uri = params["textDocument"]["uri"]
    text = params["textDocument"]["text"]
    version = params["textDocument"]["version"]

    Logger.info("Document opened: #{uri}")

    # Store document state
    documents = Map.put(lsp.assigns.documents, uri, %{text: text, version: version})
    new_state = put_in(lsp.assigns.documents, documents)

    # Trigger diagnostics on open
    spawn(fn ->
      diagnostics = Diagnostics.handle(params, new_state.assigns)

      GenLSP.notify(lsp, %{
        "method" => "textDocument/publishDiagnostics",
        "params" => diagnostics
      })
    end)

    {:noreply, new_state}
  end

  @impl GenLSP
  def handle_notification(%{"method" => "textDocument/didChange", "params" => params}, lsp) do
    uri = params["textDocument"]["uri"]
    changes = params["contentChanges"]
    version = params["textDocument"]["version"]

    # Update document with full sync (change type 1)
    new_text = List.first(changes)["text"]
    documents = Map.update(lsp.assigns.documents, uri, %{text: new_text, version: version}, fn doc ->
      %{doc | text: new_text, version: version}
    end)

    new_state = put_in(lsp.assigns.documents, documents)
    {:noreply, new_state}
  end

  @impl GenLSP
  def handle_notification(%{"method" => "textDocument/didClose", "params" => params}, lsp) do
    uri = params["textDocument"]["uri"]
    Logger.info("Document closed: #{uri}")

    # Remove document from state
    documents = Map.delete(lsp.assigns.documents, uri)
    new_state = put_in(lsp.assigns.documents, documents)

    {:noreply, new_state}
  end

  @impl GenLSP
  def handle_notification(%{"method" => "textDocument/didSave", "params" => params}, lsp) do
    uri = params["textDocument"]["uri"]
    Logger.info("File saved: #{uri}")

    # Trigger diagnostics on save
    spawn(fn ->
      diagnostics = Diagnostics.handle(params, lsp.assigns)

      GenLSP.notify(lsp, %{
        "method" => "textDocument/publishDiagnostics",
        "params" => diagnostics
      })
    end)

    {:noreply, lsp}
  end

  @impl GenLSP
  def handle_notification(_notification, lsp), do: {:noreply, lsp}

  # Private helpers

  defp parse_uri(nil), do: nil
  defp parse_uri(uri) when is_binary(uri) do
    case URI.parse(uri) do
      %URI{scheme: "file", path: path} -> path
      _ -> nil
    end
  end

  # Command execution

  defp execute_command("firefox.navigate", [url | opts], _assigns) do
    opts_list = parse_opts(opts)
    Firefox.navigate(url, opts_list)
  end

  defp execute_command("firefox.click", [selector | opts], _assigns) do
    opts_list = parse_opts(opts)
    Firefox.click(selector, opts_list)
  end

  defp execute_command("firefox.typeText", [selector, text | opts], _assigns) do
    opts_list = parse_opts(opts)
    Firefox.type_text(selector, text, opts_list)
  end

  defp execute_command("firefox.screenshot", opts, _assigns) do
    opts_list = parse_opts(opts)

    case Firefox.screenshot(opts_list) do
      {:ok, binary_data} ->
        # Encode as base64 for JSON transport
        base64_data = Base.encode64(binary_data)
        {:ok, %{screenshot: base64_data, format: "png"}}

      error -> error
    end
  end

  defp execute_command("firefox.executeJs", [javascript | opts], _assigns) do
    opts_list = parse_opts(opts)
    Firefox.execute_js(javascript, opts_list)
  end

  defp execute_command("firefox.getContent", opts, _assigns) do
    opts_list = parse_opts(opts)
    Firefox.get_content(opts_list)
  end

  defp execute_command("firefox.detectBrowsers", _args, _assigns) do
    browsers = ClaudeFirefox.LSP.detect_browsers()
    {:ok, %{browsers: browsers}}
  end

  defp execute_command(_command, _args, _assigns) do
    {:error, "Unknown command"}
  end

  # Parse opts from LSP arguments (may be map or list)
  defp parse_opts([]), do: []
  defp parse_opts([%{} = opts_map | _]) do
    Enum.map(opts_map, fn {k, v} -> {String.to_atom(k), v} end)
  end
  defp parse_opts(_), do: []
end
