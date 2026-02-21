# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.VeriSimDB.DriftDetector do
  @moduledoc """
  Detect verification drift over time using VeriSimDB's temporal modality.

  Drift detection identifies:
  - Declining verification scores across sessions
  - Increasing frequency of specific violation types
  - Repos that are degrading in quality over time
  - LLM models that are becoming less reliable

  Uses the DriftTimeline relationship in VeriSimDB.
  """

  alias DyadtBrain.VeriSimDB.Client

  @timeline_collection "drift_timelines"

  @doc """
  Record a data point on the drift timeline for a repo.

  Each data point captures the verification score at a point in time.
  """
  def record_data_point(repo, score, metadata \\ %{}) when is_binary(repo) do
    point_id = "#{repo}-#{System.system_time(:millisecond)}"

    doc =
      Map.merge(metadata, %{
        "repo" => repo,
        "score" => score,
        "recorded_at" => DateTime.utc_now() |> DateTime.to_iso8601()
      })

    Client.store(@timeline_collection, point_id, doc)
  end

  @doc """
  Check if a repo is drifting (declining scores).

  Returns `{:ok, :stable}` if no drift detected, or
  `{:ok, {:drifting, trend}}` with the trend data.
  """
  def check_drift(repo) when is_binary(repo) do
    case Client.query(@timeline_collection, %{
           "filter" => %{"repo" => repo},
           "sort" => %{"recorded_at" => "desc"},
           "limit" => 20
         }) do
      {:ok, %{"documents" => docs}} when is_list(docs) and length(docs) >= 3 ->
        analyze_trend(docs)

      {:ok, _} ->
        {:ok, :insufficient_data}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Get a drift summary across all repos.
  """
  def drift_summary do
    case Client.query(@timeline_collection, %{
           "aggregate" => %{"group_by" => "repo", "metrics" => ["avg_score", "trend"]}
         }) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, reason}
    end
  end

  # Private

  defp analyze_trend(docs) do
    scores =
      docs
      |> Enum.map(&Map.get(&1, "score", 0.0))
      |> Enum.reverse()

    # Simple linear regression on the last N scores
    n = length(scores)

    if n < 3 do
      {:ok, :insufficient_data}
    else
      slope = calculate_slope(scores)

      cond do
        # Declining by more than 5% per data point
        slope < -0.05 ->
          {:ok,
           {:drifting,
            %{
              direction: :declining,
              slope: Float.round(slope, 4),
              latest_score: List.last(scores),
              data_points: n
            }}}

        # Improving
        slope > 0.05 ->
          {:ok,
           {:improving,
            %{
              direction: :improving,
              slope: Float.round(slope, 4),
              latest_score: List.last(scores),
              data_points: n
            }}}

        true ->
          {:ok, :stable}
      end
    end
  end

  defp calculate_slope(scores) do
    n = length(scores)
    xs = Enum.to_list(0..(n - 1))
    ys = scores

    x_mean = Enum.sum(xs) / n
    y_mean = Enum.sum(ys) / n

    numerator =
      Enum.zip(xs, ys)
      |> Enum.map(fn {x, y} -> (x - x_mean) * (y - y_mean) end)
      |> Enum.sum()

    denominator =
      xs
      |> Enum.map(fn x -> (x - x_mean) * (x - x_mean) end)
      |> Enum.sum()

    if denominator == 0.0 do
      0.0
    else
      numerator / denominator
    end
  end
end
