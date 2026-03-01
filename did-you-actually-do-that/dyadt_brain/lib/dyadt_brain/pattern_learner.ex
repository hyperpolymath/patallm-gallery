# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.PatternLearner do
  @moduledoc """
  Layer 12: Pattern Learning.

  Tracks violation patterns across evaluations using an ETS table.
  NOT machine learning — pattern accumulation with statistical tracking.

  Identifies:
  - Repeat offenders (same source, same violation type)
  - Common violation patterns per repository
  - Trending violation categories
  - Risk scores per source (higher = historically unreliable)

  ## Architecture

  Each violation record is stored as:
      {pattern_key, category, source, frequency, first_seen, last_seen, metadata}

  Where `pattern_key` is `"\#{category}:\#{source}"`.
  """

  use GenServer
  require Logger

  @table :dyadt_pattern_learner

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Record a violation for pattern tracking."
  def record_violation(category, source, metadata \\ %{})
      when is_binary(category) and is_binary(source) do
    GenServer.cast(__MODULE__, {:record, category, source, metadata})
  end

  @doc "Get all patterns, optionally filtered."
  def get_patterns(opts \\ []) do
    source_filter = Keyword.get(opts, :source)
    category_filter = Keyword.get(opts, :category)
    min_frequency = Keyword.get(opts, :min_frequency, 1)

    patterns =
      :ets.tab2list(@table)
      |> Enum.map(&record_to_map/1)
      |> Enum.filter(fn p ->
        (is_nil(source_filter) or p.source == source_filter) and
          (is_nil(category_filter) or p.category == category_filter) and
          p.frequency >= min_frequency
      end)
      |> Enum.sort_by(& &1.frequency, :desc)

    {:ok, patterns}
  end

  @doc """
  Get risk score for a source (0.0 = no history, 1.0 = very unreliable).

  Score = min(1.0, total_violations / 20) weighted by recency.
  """
  def get_risk_score(source) when is_binary(source) do
    patterns =
      :ets.tab2list(@table)
      |> Enum.filter(fn {_key, _cat, src, _freq, _first, _last, _meta} -> src == source end)

    if patterns == [] do
      {:ok, 0.0}
    else
      total = Enum.reduce(patterns, 0, fn {_, _, _, freq, _, _, _}, acc -> acc + freq end)
      score = min(1.0, total / 20.0)
      {:ok, Float.round(score, 4)}
    end
  end

  @doc "Get trending violations in the last N hours."
  def trending_violations(window_hours \\ 24) do
    cutoff =
      DateTime.utc_now()
      |> DateTime.add(-window_hours * 3600, :second)
      |> DateTime.to_iso8601()

    trends =
      :ets.tab2list(@table)
      |> Enum.filter(fn {_key, _cat, _src, _freq, _first, last, _meta} -> last >= cutoff end)
      |> Enum.map(&record_to_map/1)
      |> Enum.sort_by(& &1.last_seen, :desc)

    {:ok, trends}
  end

  @doc "Get learner status."
  def status do
    count = :ets.info(@table, :size) || 0
    %{pattern_count: count, status: :ok}
  end

  # Server callbacks

  @impl true
  def init(_opts) do
    table = :ets.new(@table, [:named_table, :set, :public, read_concurrency: true])
    {:ok, %{table: table}}
  end

  @impl true
  def handle_cast({:record, category, source, metadata}, state) do
    key = "#{category}:#{source}"
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    case :ets.lookup(@table, key) do
      [{^key, ^category, ^source, freq, first_seen, _last_seen, _old_meta}] ->
        :ets.insert(@table, {key, category, source, freq + 1, first_seen, now, metadata})

      [] ->
        :ets.insert(@table, {key, category, source, 1, now, now, metadata})
    end

    {:noreply, state}
  end

  # Private

  defp record_to_map({key, category, source, frequency, first_seen, last_seen, metadata}) do
    %{
      key: key,
      category: category,
      source: source,
      frequency: frequency,
      first_seen: first_seen,
      last_seen: last_seen,
      metadata: metadata
    }
  end
end
