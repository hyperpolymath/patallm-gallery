# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.RegressionBaseline do
  @moduledoc """
  Layer 10 enhancement: baseline storage and comparison for regression detection.

  Stores test results as a baseline, then compares current results against it
  to identify new failures, regressions, and improvements.

  ## Storage

  Baselines are stored as JSON at `.dyadt/baseline.json` in the repo root,
  or at the path configured via `:baseline_path`.
  """

  use GenServer
  require Logger

  @default_path ".dyadt/baseline.json"

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Store current test results as the baseline for a repo."
  def store_baseline(repo_path, test_results) when is_binary(repo_path) and is_map(test_results) do
    GenServer.call(__MODULE__, {:store, repo_path, test_results})
  end

  @doc "Load the stored baseline for a repo."
  def load_baseline(repo_path) when is_binary(repo_path) do
    GenServer.call(__MODULE__, {:load, repo_path})
  end

  @doc """
  Compare current test results against the stored baseline.

  Returns a map with:
  - `regressions` — tests that passed in baseline but fail now
  - `new_failures` — tests that didn't exist in baseline and fail
  - `new_passes` — tests that failed in baseline but pass now
  - `regression_detected` — boolean
  """
  def compare(repo_path, current_results)
      when is_binary(repo_path) and is_map(current_results) do
    GenServer.call(__MODULE__, {:compare, repo_path, current_results})
  end

  # Server callbacks

  @impl true
  def init(_opts) do
    {:ok, %{baselines: %{}}}
  end

  @impl true
  def handle_call({:store, repo_path, results}, _from, state) do
    baseline = %{
      "results" => results,
      "stored_at" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "repo" => repo_path
    }

    # Store in memory
    state = %{state | baselines: Map.put(state.baselines, repo_path, baseline)}

    # Persist to disk
    path = baseline_path(repo_path)
    ensure_dir(path)

    case Jason.encode(baseline, pretty: true) do
      {:ok, json} ->
        File.write(path, json)
        {:reply, :ok, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:load, repo_path}, _from, state) do
    case Map.get(state.baselines, repo_path) do
      nil ->
        # Try loading from disk
        path = baseline_path(repo_path)

        case File.read(path) do
          {:ok, contents} ->
            case Jason.decode(contents) do
              {:ok, baseline} ->
                state = %{state | baselines: Map.put(state.baselines, repo_path, baseline)}
                {:reply, {:ok, baseline}, state}

              {:error, reason} ->
                {:reply, {:error, {:parse_error, reason}}, state}
            end

          {:error, :enoent} ->
            {:reply, {:error, :no_baseline}, state}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end

      baseline ->
        {:reply, {:ok, baseline}, state}
    end
  end

  def handle_call({:compare, repo_path, current}, _from, state) do
    case Map.get(state.baselines, repo_path) do
      nil ->
        # No baseline — treat everything as new
        {:reply, {:ok, no_baseline_comparison(current)}, state}

      baseline ->
        comparison = do_compare(baseline["results"] || %{}, current)
        {:reply, {:ok, comparison}, state}
    end
  end

  # Private

  defp do_compare(baseline_results, current_results) do
    baseline_verdict = baseline_results["verdict"] || "Unknown"
    current_verdict = current_results["verdict"] || "Unknown"

    baseline_summary = baseline_results["test_summary"] || %{}
    current_summary = current_results["test_summary"] || %{}

    baseline_failures = Map.get(baseline_summary, "failed", 0) + Map.get(baseline_summary, "failures", 0)
    current_failures = Map.get(current_summary, "failed", 0) + Map.get(current_summary, "failures", 0)

    regressions =
      cond do
        baseline_verdict == "Confirmed" and current_verdict == "Refuted" ->
          [%{"type" => "full_regression", "description" => "Tests were passing, now failing"}]

        current_failures > baseline_failures ->
          [
            %{
              "type" => "new_failures",
              "description" =>
                "#{current_failures - baseline_failures} new test failure(s) " <>
                  "(baseline: #{baseline_failures}, current: #{current_failures})"
            }
          ]

        true ->
          []
      end

    improvements =
      cond do
        baseline_verdict == "Refuted" and current_verdict == "Confirmed" ->
          [%{"type" => "fixed", "description" => "Tests were failing, now passing"}]

        current_failures < baseline_failures ->
          [
            %{
              "type" => "fewer_failures",
              "description" =>
                "#{baseline_failures - current_failures} fewer failure(s) " <>
                  "(baseline: #{baseline_failures}, current: #{current_failures})"
            }
          ]

        true ->
          []
      end

    %{
      "regression_detected" => length(regressions) > 0,
      "regressions" => regressions,
      "improvements" => improvements,
      "baseline_verdict" => baseline_verdict,
      "current_verdict" => current_verdict,
      "compared_at" => DateTime.utc_now() |> DateTime.to_iso8601()
    }
  end

  defp no_baseline_comparison(current) do
    %{
      "regression_detected" => false,
      "regressions" => [],
      "improvements" => [],
      "baseline_verdict" => "None",
      "current_verdict" => current["verdict"] || "Unknown",
      "compared_at" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "note" => "No baseline stored — this run will become the baseline"
    }
  end

  defp baseline_path(repo_path) do
    configured = Application.get_env(:dyadt_brain, :baseline_path, @default_path)
    Path.join(repo_path, configured)
  end

  defp ensure_dir(path) do
    path |> Path.dirname() |> File.mkdir_p()
  end
end
