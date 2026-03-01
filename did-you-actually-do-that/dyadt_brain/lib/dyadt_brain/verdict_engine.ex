# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.VerdictEngine do
  @moduledoc """
  Aggregates verification results from all 12 layers into a final verdict.

  Verdict logic:
  - ALL layers must confirm → Confirmed
  - ANY layer refutes → Refuted
  - ALL layers unverifiable → Unverifiable
  - Otherwise → Inconclusive
  """

  use GenServer

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Aggregate layer results into a final verdict string.

  Each layer result should have an "evidence_result" map with a "verdict" key.
  """
  def aggregate(layer_results) when is_list(layer_results) do
    verdicts =
      layer_results
      |> Enum.map(fn result ->
        get_in(result, ["evidence_result", "verdict"]) ||
          Map.get(result, "verdict", "Unverifiable")
      end)

    cond do
      Enum.empty?(verdicts) ->
        "Unverifiable"

      Enum.all?(verdicts, &(&1 == "Confirmed")) ->
        "Confirmed"

      Enum.any?(verdicts, &(&1 == "Refuted")) ->
        "Refuted"

      Enum.all?(verdicts, &(&1 == "Unverifiable")) ->
        "Unverifiable"

      true ->
        "Inconclusive"
    end
  end

  # Server callbacks

  @impl true
  def init(_opts) do
    {:ok, %{}}
  end
end
