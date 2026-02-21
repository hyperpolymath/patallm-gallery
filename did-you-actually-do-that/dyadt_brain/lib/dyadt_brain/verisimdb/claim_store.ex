# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.VeriSimDB.ClaimStore do
  @moduledoc """
  Store and query verification claims and results in VeriSimDB.

  Collections:
  - `verification_claims` — claim records with metadata
  - `verification_results` — per-layer verification results
  - `slm_votes` — SLM ensemble vote records
  - `llm_sessions` — LLM session tracking
  """

  alias DyadtBrain.VeriSimDB.Client

  @claims_collection "verification_claims"
  @results_collection "verification_results"
  @votes_collection "slm_votes"
  @sessions_collection "llm_sessions"

  @doc "Store a verification claim."
  def store_claim(claim_id, claim) when is_binary(claim_id) and is_map(claim) do
    doc = %{
      "claim_text" => claim["description"] || "",
      "claim_type" => detect_claim_type(claim),
      "evidence_count" => length(claim["evidence"] || []),
      "source" => claim["source"] || "unknown",
      "repo" => claim["repo"],
      "session_id" => claim["session_id"],
      "stored_at" => DateTime.utc_now() |> DateTime.to_iso8601()
    }

    Client.store(@claims_collection, claim_id, doc)
  end

  @doc "Store a verification result for a specific layer."
  def store_result(claim_id, layer_result) when is_binary(claim_id) and is_map(layer_result) do
    result_id = "#{claim_id}-L#{layer_result["layer"] || "?"}"

    doc = %{
      "claim_id" => claim_id,
      "layer" => layer_result["layer"],
      "layer_name" => get_in(layer_result, ["layer"]),
      "verdict" => get_in(layer_result, ["evidence_result", "verdict"]),
      "details" => get_in(layer_result, ["evidence_result", "details"]),
      "verified_at" => DateTime.utc_now() |> DateTime.to_iso8601()
    }

    Client.store(@results_collection, result_id, doc)
  end

  @doc "Store SLM ensemble votes for a claim."
  def store_votes(claim_id, ensemble_result) when is_binary(claim_id) and is_map(ensemble_result) do
    votes = ensemble_result["votes"] || []

    results =
      votes
      |> Enum.with_index()
      |> Enum.map(fn {vote, idx} ->
        vote_id = "#{claim_id}-vote-#{idx}"

        doc = %{
          "claim_id" => claim_id,
          "model_name" => vote["voter"],
          "vote" => vote["decision"],
          "confidence" => vote["confidence"],
          "reasoning" => vote["reasoning"],
          "weight" => vote["weight"],
          "voted_at" => DateTime.utc_now() |> DateTime.to_iso8601()
        }

        Client.store(@votes_collection, vote_id, doc)
      end)

    errors = Enum.filter(results, &match?({:error, _}, &1))

    if Enum.empty?(errors) do
      {:ok, length(results)}
    else
      {:error, {:partial_failure, length(errors), length(results)}}
    end
  end

  @doc "Store or update an LLM session record."
  def store_session(session) when is_map(session) do
    session_id = session["id"] || generate_id()

    doc = %{
      "model" => session["model"] || "unknown",
      "repo" => session["repo"],
      "started_at" => session["started_at"] || DateTime.utc_now() |> DateTime.to_iso8601(),
      "ended_at" => session["ended_at"],
      "total_claims" => session["total_claims"] || 0,
      "confirmed_claims" => session["confirmed_claims"] || 0,
      "refuted_claims" => session["refuted_claims"] || 0,
      "overall_score" => calculate_score(session)
    }

    Client.store(@sessions_collection, session_id, doc)
  end

  @doc "Query claims by repo."
  def claims_by_repo(repo) when is_binary(repo) do
    Client.query(@claims_collection, %{"filter" => %{"repo" => repo}})
  end

  @doc "Query results by claim ID."
  def results_for_claim(claim_id) when is_binary(claim_id) do
    Client.query(@results_collection, %{"filter" => %{"claim_id" => claim_id}})
  end

  @doc "Query sessions by repo."
  def sessions_by_repo(repo) when is_binary(repo) do
    Client.query(@sessions_collection, %{"filter" => %{"repo" => repo}})
  end

  @doc "Get a claim by ID."
  def get_claim(claim_id) when is_binary(claim_id) do
    Client.get(@claims_collection, claim_id)
  end

  # Private

  defp detect_claim_type(claim) do
    evidence = claim["evidence"] || []

    types =
      evidence
      |> Enum.map(&Map.get(&1, "type", "Unknown"))
      |> Enum.uniq()

    case types do
      ["FileExists"] -> "FileCreation"
      ["GitClean"] -> "GitOperation"
      ["GitCommitExists"] -> "GitOperation"
      ["CommandSucceeds"] -> "CommandExecution"
      _ -> "Mixed"
    end
  end

  defp calculate_score(session) do
    total = session["total_claims"] || 0
    confirmed = session["confirmed_claims"] || 0

    if total > 0 do
      Float.round(confirmed / total, 4)
    else
      0.0
    end
  end

  defp generate_id do
    :crypto.strong_rand_bytes(12) |> Base.url_encode64(padding: false)
  end
end
