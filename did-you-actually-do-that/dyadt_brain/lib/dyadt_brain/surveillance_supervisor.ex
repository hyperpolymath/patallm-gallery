# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.SurveillanceSupervisor do
  @moduledoc """
  Tier 3: Consensus Arbiter — weighted vote aggregation with GO/NO-GO logic.

  Implements the basal ganglia model from cognitive-gating:
  - Collects votes from Policy Oracle (Tier 1) and SLM Ensemble (Tier 2)
  - Applies weighted vote aggregation
  - Makes GO/NO-GO decisions
  - Escalates to human when consensus is low

  ## Decision Logic

  The arbiter uses weighted voting:
  - Approve votes: weight 1.0
  - Reject votes: weight 1.5 (asymmetric — rejections carry more weight)
  - Abstain votes: weight 0.0

  Thresholds:
  - `confirm_threshold`: minimum approve_weight ratio to confirm (default 0.6)
  - `refute_threshold`: minimum reject_weight ratio to refute (default 0.4)
  - `escalation_threshold`: if neither threshold met, escalate to human
  """

  use GenServer
  require Logger

  alias DyadtBrain.Ports.RustPort

  @confirm_threshold 0.6
  @refute_threshold 0.4
  @min_confidence 0.5

  defstruct [
    :confirm_threshold,
    :refute_threshold,
    :min_confidence,
    evaluations: %{},
    stats: %{total: 0, confirmed: 0, refuted: 0, escalated: 0}
  ]

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Evaluate a claim through the full three-tier model.

  1. Sends claim context to Rust (Tiers 1+2: Policy Oracle + SLM Ensemble)
  2. Applies Tier 3 consensus logic
  3. Returns final verdict with reasoning

  ## Parameters

  - `context`: Map with keys:
    - `"claim_text"` — what the LLM claimed
    - `"evidence_summary"` — results from other verification layers
    - `"artifact_sample"` — code/content produced (optional)
    - `"diff_sample"` — git diff (optional)

  ## Returns

  `{:ok, result}` where result contains:
  - `verdict` — "Confirmed", "Refuted", "Inconclusive", or "Escalate"
  - `confidence` — overall confidence (0.0 to 1.0)
  - `reasoning` — human-readable explanation
  - `votes` — individual vote details
  """
  def evaluate(context) when is_map(context) do
    GenServer.call(__MODULE__, {:evaluate, context}, :timer.seconds(30))
  end

  @doc "Get surveillance statistics."
  def stats do
    GenServer.call(__MODULE__, :stats)
  end

  @doc "Get the result of a previous evaluation by ID."
  def get_evaluation(eval_id) do
    GenServer.call(__MODULE__, {:get_evaluation, eval_id})
  end

  # Server callbacks

  @impl true
  def init(opts) do
    state = %__MODULE__{
      confirm_threshold: Keyword.get(opts, :confirm_threshold, @confirm_threshold),
      refute_threshold: Keyword.get(opts, :refute_threshold, @refute_threshold),
      min_confidence: Keyword.get(opts, :min_confidence, @min_confidence)
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:evaluate, context}, _from, state) do
    {result, state} = do_evaluate(context, state)
    {:reply, result, state}
  end

  def handle_call(:stats, _from, state) do
    {:reply, {:ok, state.stats}, state}
  end

  def handle_call({:get_evaluation, eval_id}, _from, state) do
    case Map.get(state.evaluations, eval_id) do
      nil -> {:reply, {:error, :not_found}, state}
      eval -> {:reply, {:ok, eval}, state}
    end
  end

  # Private

  defp do_evaluate(context, state) do
    # Step 1: Get Tier 1+2 results from Rust
    ensemble_result =
      case RustPort.evaluate_slm(context) do
        {:ok, result} -> result
        {:error, _reason} -> fallback_evaluation(context)
      end

    # Step 2: Apply Tier 3 consensus logic
    votes = ensemble_result["votes"] || []
    approve_weight = ensemble_result["approve_weight"] || 0.0
    reject_weight = ensemble_result["reject_weight"] || 0.0
    total_weight = approve_weight + reject_weight

    {verdict, confidence, reasoning} =
      cond do
        total_weight == 0.0 ->
          {"Inconclusive", 0.0, "No votes with weight — cannot determine"}

        approve_ratio(approve_weight, total_weight) >= state.confirm_threshold ->
          conf = approve_ratio(approve_weight, total_weight)
          {"Confirmed", conf, build_reasoning(votes, "approved", conf)}

        reject_ratio(reject_weight, total_weight) >= state.refute_threshold ->
          conf = reject_ratio(reject_weight, total_weight)
          {"Refuted", conf, build_reasoning(votes, "rejected", conf)}

        true ->
          # Neither threshold met — escalate
          conf = max(approve_ratio(approve_weight, total_weight),
                     reject_ratio(reject_weight, total_weight))
          {"Escalate", conf,
           "Consensus too low to decide (approve: #{Float.round(approve_weight, 2)}, " <>
             "reject: #{Float.round(reject_weight, 2)}) — human review recommended"}
      end

    # Apply minimum confidence filter
    {final_verdict, final_confidence} =
      if confidence < state.min_confidence and verdict in ["Confirmed", "Refuted"] do
        {"Escalate", confidence}
      else
        {verdict, confidence}
      end

    eval_id = generate_eval_id()

    result = %{
      "eval_id" => eval_id,
      "verdict" => final_verdict,
      "confidence" => Float.round(final_confidence, 4),
      "reasoning" => reasoning,
      "votes" => votes,
      "approve_weight" => approve_weight,
      "reject_weight" => reject_weight
    }

    # Update stats
    stats = update_stats(state.stats, final_verdict)
    evaluations = Map.put(state.evaluations, eval_id, result)
    state = %{state | stats: stats, evaluations: evaluations}

    {{:ok, result}, state}
  end

  defp approve_ratio(approve, total) when total > 0, do: approve / total
  defp approve_ratio(_, _), do: 0.0

  defp reject_ratio(reject, total) when total > 0, do: reject / total
  defp reject_ratio(_, _), do: 0.0

  defp build_reasoning(votes, outcome, confidence) do
    voter_summary =
      votes
      |> Enum.map(fn vote ->
        "#{vote["voter"]}: #{vote["decision"]} (conf: #{vote["confidence"]})"
      end)
      |> Enum.join(", ")

    "Consensus #{outcome} with #{Float.round(confidence * 100, 1)}% confidence. " <>
      "Voters: #{voter_summary}"
  end

  defp fallback_evaluation(context) do
    # If Rust port is unavailable, run a minimal check
    # Just check for obvious stub patterns in the artifact
    claim = context["claim_text"] || ""
    artifact = context["artifact_sample"] || ""

    stub_patterns = ["todo!()", "unimplemented!()", "# TODO", "// TODO", "pass  # placeholder"]

    has_stubs = Enum.any?(stub_patterns, &String.contains?(artifact, &1))

    overconfident =
      String.contains?(String.downcase(claim), "100% complete") or
        String.contains?(String.downcase(claim), "fully implemented")

    cond do
      has_stubs ->
        %{
          "votes" => [
            %{
              "voter" => "fallback_oracle",
              "decision" => "Reject",
              "confidence" => 0.8,
              "reasoning" => "Stub patterns detected in artifact",
              "weight" => 1.5
            }
          ],
          "approve_weight" => 0.0,
          "reject_weight" => 1.5,
          "abstain_count" => 0
        }

      overconfident ->
        %{
          "votes" => [
            %{
              "voter" => "fallback_oracle",
              "decision" => "Reject",
              "confidence" => 0.6,
              "reasoning" => "Overconfident claim without sufficient evidence",
              "weight" => 1.5
            }
          ],
          "approve_weight" => 0.0,
          "reject_weight" => 1.5,
          "abstain_count" => 0
        }

      true ->
        %{
          "votes" => [
            %{
              "voter" => "fallback_oracle",
              "decision" => "Approve",
              "confidence" => 0.5,
              "reasoning" => "No obvious violations (limited analysis)",
              "weight" => 1.0
            }
          ],
          "approve_weight" => 1.0,
          "reject_weight" => 0.0,
          "abstain_count" => 0
        }
    end
  end

  defp update_stats(stats, verdict) do
    stats
    |> Map.update!(:total, &(&1 + 1))
    |> then(fn s ->
      case verdict do
        "Confirmed" -> Map.update!(s, :confirmed, &(&1 + 1))
        "Refuted" -> Map.update!(s, :refuted, &(&1 + 1))
        "Escalate" -> Map.update!(s, :escalated, &(&1 + 1))
        _ -> s
      end
    end)
  end

  defp generate_eval_id do
    :crypto.strong_rand_bytes(8) |> Base.url_encode64(padding: false)
  end
end
