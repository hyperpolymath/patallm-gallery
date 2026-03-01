# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.ClaimRegistry do
  @moduledoc """
  ETS-backed registry for tracking claims and their verification status.

  Each claim moves through states:
    :pending → :verifying → :verified | :failed

  Reports are stored alongside claims for fast lookup.
  """

  use GenServer

  @table :dyadt_claims

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Register a new claim for verification."
  def register(claim) when is_map(claim) do
    GenServer.call(__MODULE__, {:register, claim})
  end

  @doc "Update claim status and attach a verification report."
  def update(claim_id, status, report \\ nil) do
    GenServer.call(__MODULE__, {:update, claim_id, status, report})
  end

  @doc "Get the verification report for a claim."
  def get_report(claim_id) do
    case :ets.lookup(@table, claim_id) do
      [{^claim_id, _claim, _status, report}] when not is_nil(report) ->
        {:ok, report}

      [{^claim_id, _claim, status, _report}] ->
        {:pending, status}

      [] ->
        {:error, :not_found}
    end
  end

  @doc "List all claims with their statuses."
  def list_all do
    :ets.tab2list(@table)
    |> Enum.map(fn {id, claim, status, report} ->
      %{id: id, claim: claim, status: status, report: report}
    end)
  end

  @doc "Registry health status."
  def status do
    case :ets.info(@table) do
      :undefined -> :down
      info -> {:ok, Keyword.get(info, :size, 0)}
    end
  end

  # Server callbacks

  @impl true
  def init(_opts) do
    table = :ets.new(@table, [:named_table, :set, :public, read_concurrency: true])
    {:ok, %{table: table}}
  end

  @impl true
  def handle_call({:register, claim}, _from, state) do
    claim_id = Map.get(claim, :id) || Map.get(claim, "id") || generate_id()
    :ets.insert(@table, {claim_id, claim, :pending, nil})
    {:reply, {:ok, claim_id}, state}
  end

  @impl true
  def handle_call({:update, claim_id, status, report}, _from, state) do
    case :ets.lookup(@table, claim_id) do
      [{^claim_id, claim, _old_status, _old_report}] ->
        :ets.insert(@table, {claim_id, claim, status, report})
        {:reply, :ok, state}

      [] ->
        {:reply, {:error, :not_found}, state}
    end
  end

  defp generate_id do
    :crypto.strong_rand_bytes(8) |> Base.hex_encode32(case: :lower, padding: false)
  end
end
