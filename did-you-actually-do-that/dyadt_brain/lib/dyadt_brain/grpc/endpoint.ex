# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.GRPC.Endpoint do
  @moduledoc """
  TCP-based gRPC endpoint for the DYADT verification service.

  Listens on the configured port (default 4201) and dispatches RPC calls
  to the `DyadtBrain.GRPC.Server` handler.

  Wire protocol: length-delimited JSON (4-byte big-endian prefix),
  matching the Rust port protocol for consistency.
  """

  use GenServer
  require Logger

  alias DyadtBrain.GRPC.{Messages, Server}

  @default_port 4201

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    port = Keyword.get(opts, :port, Application.get_env(:dyadt_brain, :grpc_port, @default_port))

    case :gen_tcp.listen(port, [
           :binary,
           packet: :raw,
           active: false,
           reuseaddr: true,
           backlog: 128
         ]) do
      {:ok, listen_socket} ->
        Logger.info("DYADT gRPC endpoint listening on port #{port}")
        # Start accepting connections
        send(self(), :accept)
        {:ok, %{listen_socket: listen_socket, port: port}}

      {:error, reason} ->
        Logger.warning("Failed to start gRPC endpoint on port #{port}: #{inspect(reason)}")
        {:ok, %{listen_socket: nil, port: port, error: reason}}
    end
  end

  @impl true
  def handle_info(:accept, %{listen_socket: nil} = state) do
    # No socket — retry after delay
    Process.send_after(self(), :accept, 5000)
    {:noreply, state}
  end

  def handle_info(:accept, %{listen_socket: socket} = state) do
    case :gen_tcp.accept(socket, 1000) do
      {:ok, client} ->
        # Spawn a handler for this connection
        Task.start(fn -> handle_connection(client) end)

      {:error, :timeout} ->
        :ok

      {:error, reason} ->
        Logger.warning("gRPC accept error: #{inspect(reason)}")
    end

    send(self(), :accept)
    {:noreply, state}
  end

  def handle_info(_msg, state), do: {:noreply, state}

  @impl true
  def terminate(_reason, %{listen_socket: socket}) when not is_nil(socket) do
    :gen_tcp.close(socket)
  end

  def terminate(_reason, _state), do: :ok

  # Connection handler

  defp handle_connection(socket) do
    case read_message(socket) do
      {:ok, data} ->
        response = dispatch_rpc(data)
        :gen_tcp.send(socket, response)
        # Keep connection alive for streaming
        handle_connection(socket)

      {:error, :closed} ->
        :ok

      {:error, reason} ->
        Logger.debug("gRPC connection error: #{inspect(reason)}")
        :ok
    end
  after
    :gen_tcp.close(socket)
  end

  defp read_message(socket) do
    case :gen_tcp.recv(socket, 4, 30_000) do
      {:ok, <<length::big-unsigned-32>>} ->
        :gen_tcp.recv(socket, length, 10_000)

      error ->
        error
    end
  end

  defp dispatch_rpc(data) do
    case Jason.decode(data) do
      {:ok, %{"method" => method} = request} ->
        params = Map.get(request, "params", %{})
        result = Server.handle_rpc(method, params)

        case result do
          {:ok, response} -> Messages.build_response(response)
          {:error, reason} -> Messages.build_response(nil, reason)
        end

      {:error, reason} ->
        Messages.build_response(nil, "Invalid request: #{inspect(reason)}")
    end
  end
end
