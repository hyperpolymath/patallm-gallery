# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.GRPC.Client do
  @moduledoc """
  Client stub for the DYADT gRPC verification service.

  Connects to the gRPC endpoint and sends RPC calls using the
  length-delimited JSON wire format.

  ## Usage

      {:ok, client} = DyadtBrain.GRPC.Client.connect()
      {:ok, report} = DyadtBrain.GRPC.Client.verify_claim(client, %{
        "description" => "Created config",
        "evidence" => [%{"type" => "FileExists", "spec" => %{"path" => "/tmp/config.json"}}]
      })
      DyadtBrain.GRPC.Client.disconnect(client)
  """

  @default_host ~c"127.0.0.1"
  @default_port 4201

  @doc "Connect to the gRPC endpoint."
  def connect(opts \\ []) do
    host = Keyword.get(opts, :host, @default_host)
    port = Keyword.get(opts, :port, @default_port)

    case :gen_tcp.connect(host, port, [:binary, packet: :raw, active: false], 5000) do
      {:ok, socket} -> {:ok, socket}
      {:error, reason} -> {:error, {:connection_failed, reason}}
    end
  end

  @doc "Disconnect from the gRPC endpoint."
  def disconnect(socket) do
    :gen_tcp.close(socket)
  end

  @doc "Verify a single claim."
  def verify_claim(socket, claim) do
    call(socket, "VerifyClaim", claim)
  end

  @doc "Verify a batch of claims."
  def verify_batch(socket, claims) do
    call(socket, "VerifyBatch", %{"claims" => claims})
  end

  @doc "Get a verification report."
  def get_report(socket, claim_id) do
    call(socket, "GetReport", %{"claim_id" => claim_id})
  end

  @doc "Evaluate through SLM ensemble."
  def evaluate_slm(socket, context) do
    call(socket, "EvaluateSlm", context)
  end

  @doc "Health check."
  def health(socket) do
    call(socket, "Health", %{})
  end

  @doc "Get statistics."
  def get_stats(socket) do
    call(socket, "GetStats", %{})
  end

  @doc "List claims."
  def list_claims(socket, opts \\ %{}) do
    call(socket, "ListClaims", opts)
  end

  # Send an RPC call and wait for the response
  defp call(socket, method, params) do
    request = Jason.encode!(%{"method" => method, "params" => params})
    length = byte_size(request)
    packet = <<length::big-unsigned-32, request::binary>>

    with :ok <- :gen_tcp.send(socket, packet),
         {:ok, <<resp_length::big-unsigned-32>>} <- :gen_tcp.recv(socket, 4, 30_000),
         {:ok, resp_data} <- :gen_tcp.recv(socket, resp_length, 10_000),
         {:ok, response} <- Jason.decode(resp_data) do
      case response do
        %{"ok" => true, "result" => result} -> {:ok, result}
        %{"ok" => false, "error" => error} -> {:error, error}
        other -> {:ok, other}
      end
    else
      {:error, reason} -> {:error, reason}
    end
  end
end
