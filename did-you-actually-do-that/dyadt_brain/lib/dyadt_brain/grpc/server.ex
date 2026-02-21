# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.GRPC.Server do
  @moduledoc """
  RPC method dispatcher for the DYADT gRPC service.

  Maps RPC method names to `DyadtBrain.Verifications` calls.

  ## Methods

  - `VerifyClaim` — Verify a single claim
  - `VerifyBatch` — Verify multiple claims
  - `GetReport` — Get a verification report by ID
  - `EvaluateSlm` — Run SLM ensemble evaluation
  - `Health` — System health check
  - `GetStats` — Verification statistics
  - `ListClaims` — List all claims
  """

  alias DyadtBrain.Verifications

  @doc "Dispatch an RPC method call to the appropriate handler."
  def handle_rpc(method, params) do
    case method do
      "VerifyClaim" -> verify_claim(params)
      "VerifyBatch" -> verify_batch(params)
      "GetReport" -> get_report(params)
      "EvaluateSlm" -> evaluate_slm(params)
      "Health" -> health(params)
      "GetStats" -> get_stats(params)
      "ListClaims" -> list_claims(params)
      _ -> {:error, "Unknown method: #{method}"}
    end
  end

  defp verify_claim(params) do
    Verifications.verify_claim(params)
  end

  defp verify_batch(%{"claims" => claims}) when is_list(claims) do
    Verifications.verify_batch(claims)
  end

  defp verify_batch(_), do: {:error, "Missing 'claims' array"}

  defp get_report(%{"claim_id" => id}) when is_binary(id) do
    case Verifications.get_report(id) do
      {:ok, report} -> {:ok, report}
      {:pending, status} -> {:ok, %{"status" => "pending", "substatus" => to_string(status)}}
      {:error, :not_found} -> {:error, "not_found"}
    end
  end

  defp get_report(_), do: {:error, "Missing 'claim_id'"}

  defp evaluate_slm(params) do
    Verifications.evaluate_slm(params)
  end

  defp health(_params) do
    health = Verifications.health_check()

    {:ok,
     %{
       "brain" => to_string(health.brain),
       "rust_port" => inspect(health.rust_port),
       "claim_registry" => inspect(health.claim_registry),
       "pattern_learner" => inspect(health.pattern_learner),
       "timestamp" => DateTime.to_iso8601(health.timestamp)
     }}
  end

  defp get_stats(_params) do
    Verifications.get_stats()
  end

  defp list_claims(params) do
    opts =
      case Map.get(params, "status") do
        nil -> []
        status -> [status: safe_status_atom(status)]
      end

    Verifications.list_claims(opts)
  end

  @valid_statuses %{
    "pending" => :pending,
    "running" => :running,
    "completed" => :completed,
    "failed" => :failed
  }

  defp safe_status_atom(status) when is_binary(status) do
    Map.get(@valid_statuses, status, :pending)
  end
end
