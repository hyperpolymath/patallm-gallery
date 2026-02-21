# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.Ports.RustPort do
  @moduledoc """
  Erlang Port to the Rust verifier binary (`dyadt serve`).

  Uses length-prefixed binary protocol:
    [4 bytes: length BE] [N bytes: JSON payload]

  The Rust side sends a ready signal on startup, then processes
  request/response pairs.

  ## Protocol

  Request:
      %{"id" => uuid, "method" => method, "params" => params}

  Response:
      %{"id" => uuid, "result" => result}
      %{"id" => uuid, "error" => %{"code" => code, "message" => msg}}
  """

  use GenServer
  require Logger

  @default_binary "dyadt"
  @ready_timeout :timer.seconds(5)

  defstruct [:port, :binary_path, :ready, :pending, :request_counter]

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Ping the Rust verifier to check connectivity."
  def ping do
    GenServer.call(__MODULE__, :ping, @ready_timeout)
  catch
    :exit, _ -> {:error, :port_unavailable}
  end

  @doc "Verify a single claim via the Rust verifier."
  def verify_claim(claim) when is_map(claim) do
    GenServer.call(__MODULE__, {:verify_claim, claim}, :timer.seconds(30))
  catch
    :exit, _ -> {:error, :port_unavailable}
  end

  @doc "Verify a batch of claims."
  def verify_batch(claims) when is_list(claims) do
    GenServer.call(__MODULE__, {:verify_batch, claims}, :timer.seconds(60))
  catch
    :exit, _ -> {:error, :port_unavailable}
  end

  @doc "Check a single piece of evidence."
  def check_evidence(evidence) when is_map(evidence) do
    GenServer.call(__MODULE__, {:check_evidence, evidence}, :timer.seconds(10))
  catch
    :exit, _ -> {:error, :port_unavailable}
  end

  @doc "Evaluate a claim through the SLM ensemble (Tiers 1+2)."
  def evaluate_slm(context) when is_map(context) do
    GenServer.call(__MODULE__, {:evaluate_slm, context}, :timer.seconds(30))
  catch
    :exit, _ -> {:error, :port_unavailable}
  end

  @doc "Extract claims from text."
  def extract_claims(text, opts \\ []) when is_binary(text) do
    params = %{
      "text" => text,
      "source" => Keyword.get(opts, :source, "brain-client"),
      "working_directory" => Keyword.get(opts, :working_directory)
    }

    GenServer.call(__MODULE__, {:extract_claims, params}, :timer.seconds(10))
  catch
    :exit, _ -> {:error, :port_unavailable}
  end

  # Server callbacks

  @impl true
  def init(opts) do
    binary_path = Keyword.get(opts, :binary, find_binary())

    state = %__MODULE__{
      port: nil,
      binary_path: binary_path,
      ready: false,
      pending: %{},
      request_counter: 0
    }

    {:ok, state, {:continue, :start_port}}
  end

  @impl true
  def handle_continue(:start_port, state) do
    case start_port(state.binary_path) do
      {:ok, port} ->
        {:noreply, %{state | port: port}}

      {:error, reason} ->
        Logger.warning("Failed to start Rust port: #{inspect(reason)}")
        {:noreply, state}
    end
  end

  @impl true
  def handle_call(:ping, from, %{ready: true} = state) do
    {request_id, state} = next_request_id(state)
    send_request(state.port, request_id, "ping", %{})
    {:noreply, %{state | pending: Map.put(state.pending, request_id, from)}}
  end

  def handle_call(:ping, _from, %{ready: false} = state) do
    {:reply, {:error, :not_ready}, state}
  end

  @impl true
  def handle_call({:verify_claim, claim}, from, state) do
    send_method(state, "verify_claim", claim, from)
  end

  def handle_call({:verify_batch, claims}, from, state) do
    send_method(state, "verify_batch", claims, from)
  end

  def handle_call({:check_evidence, evidence}, from, state) do
    send_method(state, "check_evidence", evidence, from)
  end

  def handle_call({:extract_claims, params}, from, state) do
    send_method(state, "extract_claims", params, from)
  end

  def handle_call({:evaluate_slm, context}, from, state) do
    send_method(state, "evaluate_slm", context, from)
  end

  @impl true
  def handle_info({port, {:data, data}}, %{port: port} = state) do
    case decode_message(data) do
      {:ok, %{"status" => "ready"} = msg} ->
        Logger.info("Rust verifier ready (v#{msg["version"]})")
        {:noreply, %{state | ready: true}}

      {:ok, %{"id" => id} = response} ->
        case Map.pop(state.pending, id) do
          {nil, _pending} ->
            Logger.warning("Received response for unknown request: #{id}")
            {:noreply, state}

          {from, pending} ->
            result =
              if Map.has_key?(response, "error") do
                {:error, response["error"]["message"]}
              else
                {:ok, response["result"]}
              end

            GenServer.reply(from, result)
            {:noreply, %{state | pending: pending}}
        end

      {:error, reason} ->
        Logger.error("Failed to decode port message: #{inspect(reason)}")
        {:noreply, state}
    end
  end

  def handle_info({port, {:exit_status, status}}, %{port: port} = state) do
    Logger.error("Rust verifier exited with status #{status}")

    # Reply to all pending requests with error
    for {_id, from} <- state.pending do
      GenServer.reply(from, {:error, :port_exited})
    end

    # Attempt restart after delay
    Process.send_after(self(), :restart_port, :timer.seconds(1))
    {:noreply, %{state | port: nil, ready: false, pending: %{}}}
  end

  def handle_info(:restart_port, state) do
    {:noreply, state, {:continue, :start_port}}
  end

  def handle_info(msg, state) do
    Logger.debug("RustPort received unexpected message: #{inspect(msg)}")
    {:noreply, state}
  end

  # Private helpers

  defp send_method(%{ready: false} = state, _method, _params, _from) do
    {:reply, {:error, :not_ready}, state}
  end

  defp send_method(state, method, params, from) do
    {request_id, state} = next_request_id(state)
    send_request(state.port, request_id, method, params)
    {:noreply, %{state | pending: Map.put(state.pending, request_id, from)}}
  end

  defp next_request_id(state) do
    counter = state.request_counter + 1
    id = "brain-#{counter}"
    {id, %{state | request_counter: counter}}
  end

  defp send_request(port, id, method, params) do
    message = Jason.encode!(%{"id" => id, "method" => method, "params" => params})
    data = encode_length_prefixed(message)
    Port.command(port, data)
  end

  defp start_port(binary_path) do
    if binary_path && File.exists?(binary_path) do
      port =
        Port.open({:spawn_executable, binary_path}, [
          :binary,
          :exit_status,
          {:packet, 4},
          {:args, ["serve"]}
        ])

      {:ok, port}
    else
      {:error, {:binary_not_found, binary_path}}
    end
  end

  defp find_binary do
    # Check application config first
    configured = Application.get_env(:dyadt_brain, :rust_binary)

    # Project root (dyadt_brain is a subdirectory of the Rust project)
    project_root = Path.expand("../../..", __DIR__) |> Path.join("..")

    candidates = [
      # From environment variable
      System.get_env("DYADT_BINARY"),
      # From application config (if it's an absolute path)
      (if configured && String.starts_with?(configured || "", "/"), do: configured),
      # Relative to project root (development)
      Path.join([project_root, "target", "release", "dyadt"]),
      Path.join([project_root, "target", "debug", "dyadt"]),
      # System PATH (from config name or default)
      System.find_executable(configured || @default_binary)
    ]

    Enum.find(candidates, fn
      nil -> false
      path -> File.exists?(path)
    end)
  end

  defp encode_length_prefixed(message) when is_binary(message) do
    # The {:packet, 4} option handles length-prefixing automatically
    # on the Erlang side, so we just return the raw message
    message
  end

  defp decode_message(data) when is_binary(data) do
    # {:packet, 4} strips the length prefix, so we get raw JSON
    case Jason.decode(data) do
      {:ok, decoded} -> {:ok, decoded}
      {:error, reason} -> {:error, {:json_decode, reason}}
    end
  end
end
