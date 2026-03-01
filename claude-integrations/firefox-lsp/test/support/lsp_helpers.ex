# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule LSPHelpers do
  @moduledoc """
  Common test helpers for LSP protocol testing.
  """

  @doc """
  Create a mock LSP initialize request.
  """
  def initialize_request(root_uri \\ "file:///test") do
    %{
      "jsonrpc" => "2.0",
      "id" => 1,
      "method" => "initialize",
      "params" => %{
        "rootUri" => root_uri,
        "capabilities" => %{
          "textDocument" => %{
            "completion" => %{"dynamicRegistration" => true},
            "hover" => %{"dynamicRegistration" => true}
          }
        }
      }
    }
  end

  @doc """
  Create a mock textDocument/didOpen notification.
  """
  def did_open_notification(uri, language_id, text) do
    %{
      "jsonrpc" => "2.0",
      "method" => "textDocument/didOpen",
      "params" => %{
        "textDocument" => %{
          "uri" => uri,
          "languageId" => language_id,
          "version" => 1,
          "text" => text
        }
      }
    }
  end

  @doc """
  Create a mock textDocument/completion request.
  """
  def completion_request(uri, line, character) do
    %{
      "jsonrpc" => "2.0",
      "id" => 2,
      "method" => "textDocument/completion",
      "params" => %{
        "textDocument" => %{"uri" => uri},
        "position" => %{"line" => line, "character" => character}
      }
    }
  end

  @doc """
  Create a mock textDocument/hover request.
  """
  def hover_request(uri, line, character) do
    %{
      "jsonrpc" => "2.0",
      "id" => 3,
      "method" => "textDocument/hover",
      "params" => %{
        "textDocument" => %{"uri" => uri},
        "position" => %{"line" => line, "character" => character}
      }
    }
  end

  @doc """
  Mock adapter implementation for testing.
  """
  defmodule MockAdapter do
    @behaviour :adapter_behaviour

    def detect(_path), do: {:ok, true}
    def version, do: {:ok, "1.0.0-test"}
    def metadata do
      %{
        name: "Mock",
        type: "test",
        language: "elixir"
      }
    end
  end
end
