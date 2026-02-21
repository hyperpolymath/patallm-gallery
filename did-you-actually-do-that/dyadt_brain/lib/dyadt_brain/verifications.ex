# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.Verifications do
  @moduledoc """
  Verification API context — the single business logic layer.

  All external API surfaces (REST, GraphQL, gRPC) MUST call through this
  module so they share the same behavior, validation, and error handling.

  This follows the stapeln `Stacks` pattern: one context module that all
  transport layers delegate to.
  """

  alias DyadtBrain.{
    VerificationManager,
    ClaimRegistry,
    SurveillanceSupervisor,
    PatternLearner
  }

  @doc "Verify a single claim through the full 12-layer pipeline."
  @spec verify_claim(map()) :: {:ok, map()} | {:error, term()}
  def verify_claim(claim) when is_map(claim) do
    result = VerificationManager.verify(claim)

    # Record any violations for pattern learning (Layer 12)
    case result do
      {:ok, report} ->
        record_patterns(report)
        {:ok, report}

      error ->
        error
    end
  end

  @doc "Verify a batch of claims."
  @spec verify_batch(list(map())) :: {:ok, list(map())} | {:error, term()}
  def verify_batch(claims) when is_list(claims) do
    VerificationManager.verify_batch(claims)
  end

  @doc "Get verification report by claim ID."
  @spec get_report(String.t()) :: {:ok, map()} | {:pending, atom()} | {:error, :not_found}
  def get_report(claim_id) when is_binary(claim_id) do
    ClaimRegistry.get_report(claim_id)
  end

  @doc "List all claims with optional filtering."
  @spec list_claims(keyword()) :: {:ok, list(map())}
  def list_claims(opts \\ []) do
    claims = ClaimRegistry.list_all()

    filtered =
      case Keyword.get(opts, :status) do
        nil -> claims
        status -> Enum.filter(claims, &(&1.status == status))
      end

    {:ok, filtered}
  end

  @doc "Get verification statistics."
  @spec get_stats() :: {:ok, map()}
  def get_stats do
    slm_stats =
      case SurveillanceSupervisor.stats() do
        {:ok, stats} -> stats
        _ -> %{total: 0, confirmed: 0, refuted: 0, escalated: 0}
      end

    pattern_stats =
      case PatternLearner.trending_violations() do
        {:ok, trending} -> trending
        _ -> []
      end

    registry_status = ClaimRegistry.status()

    {:ok,
     %{
       "registry" => registry_status,
       "slm" => slm_stats,
       "trending_violations" => pattern_stats,
       "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
     }}
  end

  @doc "Evaluate a claim through the SLM ensemble (Layer 11)."
  @spec evaluate_slm(map()) :: {:ok, map()} | {:error, term()}
  def evaluate_slm(context) when is_map(context) do
    SurveillanceSupervisor.evaluate(context)
  end

  @doc "Health check for all subsystems."
  @spec health_check() :: map()
  def health_check do
    %{
      brain: :ok,
      rust_port: DyadtBrain.Ports.RustPort.ping(),
      claim_registry: ClaimRegistry.status(),
      pattern_learner: PatternLearner.status(),
      timestamp: DateTime.utc_now()
    }
  end

  # Private

  defp record_patterns(report) do
    layers = report["layer_results"] || []
    source = dig(report, ["claim", "source"]) || "unknown"

    Enum.each(layers, fn lr ->
      verdict = dig(lr, ["evidence_result", "verdict"])

      if verdict == "Refuted" do
        category = lr["layer"] || "unknown"

        PatternLearner.record_violation(
          category,
          source,
          %{"details" => dig(lr, ["evidence_result", "details"])}
        )
      end
    end)
  rescue
    _ -> :ok
  end

  defp dig(nil, _), do: nil
  defp dig(map, []), do: map

  defp dig(map, [key | rest]) when is_map(map) do
    dig(Map.get(map, key), rest)
  end

  defp dig(_, _), do: nil
end
