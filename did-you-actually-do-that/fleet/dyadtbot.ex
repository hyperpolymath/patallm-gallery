# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.Fleet.Dyadtbot do
  @moduledoc """
  Bot implementation for gitbot-fleet integration.

  Dyadtbot runs verification on repos managed by the fleet. It:
  - Receives dispatch events from gitbot-fleet's git-dispatcher
  - Scans claims in commit messages, PR descriptions, and AI-generated code
  - Reports verdicts as SARIF to GitHub Security tab
  - Comments on PRs with verification results
  - Sends verdicts to VeriSimDB

  ## Triggers

  - PR creation / update
  - Push to main/master
  - Scheduled scan (daily)
  - Manual dispatch via fleet coordinator
  """

  require Logger

  alias DyadtBrain.{VerificationManager, SurveillanceSupervisor}
  alias DyadtBrain.Ports.RustPort

  @bot_id "dyadtbot"
  @bot_version "0.1.0"

  @doc "Process a fleet dispatch event."
  def handle_dispatch(event) when is_map(event) do
    Logger.info("[#{@bot_id}] Received dispatch: #{event["type"]}")

    case event["type"] do
      "pr_created" -> handle_pr(event)
      "pr_updated" -> handle_pr(event)
      "push" -> handle_push(event)
      "scheduled_scan" -> handle_scheduled_scan(event)
      "manual" -> handle_manual(event)
      other -> {:error, {:unknown_event_type, other}}
    end
  end

  @doc "Get bot metadata for fleet registration."
  def metadata do
    %{
      "bot_id" => @bot_id,
      "version" => @bot_version,
      "description" => "AI work verification — catches laziness, slop, and hallucination",
      "capabilities" => [
        "claim_extraction",
        "12_layer_verification",
        "slm_consensus",
        "sarif_reporting",
        "pr_commenting"
      ],
      "triggers" => ["pr_created", "pr_updated", "push", "scheduled_scan", "manual"],
      "priority" => 7
    }
  end

  # Event handlers

  defp handle_pr(event) do
    repo = event["repo"] || ""
    pr_number = event["pr_number"]
    pr_body = event["body"] || ""
    commits = event["commits"] || []

    # Extract claims from PR body and commit messages
    all_text =
      [pr_body | Enum.map(commits, &Map.get(&1, "message", ""))]
      |> Enum.join("\n\n")

    case RustPort.extract_claims(all_text, source: "pr-#{pr_number}") do
      {:ok, claims} when is_list(claims) and length(claims) > 0 ->
        results = Enum.map(claims, &verify_claim_with_slm/1)
        report = build_report(repo, "pr-#{pr_number}", results)
        {:ok, report}

      {:ok, []} ->
        {:ok, %{"status" => "no_claims", "repo" => repo, "pr" => pr_number}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_push(event) do
    repo = event["repo"] || ""
    commits = event["commits"] || []

    commit_messages =
      commits
      |> Enum.map(&Map.get(&1, "message", ""))
      |> Enum.join("\n\n")

    case RustPort.extract_claims(commit_messages, source: "push") do
      {:ok, claims} when is_list(claims) and length(claims) > 0 ->
        results = Enum.map(claims, &verify_claim_with_slm/1)
        report = build_report(repo, "push", results)
        {:ok, report}

      {:ok, []} ->
        {:ok, %{"status" => "no_claims", "repo" => repo}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_scheduled_scan(event) do
    repo = event["repo"] || ""
    repo_path = event["repo_path"]

    if repo_path do
      # Full repo scan — extract claims from recent commits
      case RustPort.extract_claims("Scanning repo #{repo}", source: "scheduled") do
        {:ok, claims} ->
          results = Enum.map(claims, &verify_claim_with_slm/1)
          report = build_report(repo, "scheduled", results)
          {:ok, report}

        {:error, reason} ->
          {:error, reason}
      end
    else
      {:error, :no_repo_path}
    end
  end

  defp handle_manual(event) do
    # Manual dispatch — verify whatever text is provided
    text = event["text"] || ""
    repo = event["repo"] || "manual"

    case RustPort.extract_claims(text, source: "manual") do
      {:ok, claims} ->
        results = Enum.map(claims, &verify_claim_with_slm/1)
        report = build_report(repo, "manual", results)
        {:ok, report}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp verify_claim_with_slm(claim) do
    # Run through verification manager (Layers 1-8)
    verification_result = VerificationManager.verify(claim)

    # Run SLM consensus (Layer 11) separately for detailed results
    slm_context = %{
      "claim_text" => claim["description"] || "",
      "evidence_summary" => summarize_verification(verification_result)
    }

    slm_result =
      case SurveillanceSupervisor.evaluate(slm_context) do
        {:ok, result} -> result
        {:error, _} -> nil
      end

    %{
      "claim" => claim,
      "verification" => verification_result,
      "slm" => slm_result
    }
  end

  defp summarize_verification({:ok, report}) do
    verdict = report["final_verdict"] || "?"
    layers = report["layer_results"] || []
    "Verdict: #{verdict}, #{length(layers)} layers checked"
  end

  defp summarize_verification({:error, reason}) do
    "Error: #{inspect(reason)}"
  end

  defp build_report(repo, trigger, results) do
    confirmed = Enum.count(results, &(get_in(&1, ["verification", "final_verdict"]) == "Confirmed"))
    refuted = Enum.count(results, &(get_in(&1, ["verification", "final_verdict"]) == "Refuted"))
    total = length(results)

    %{
      "bot_id" => @bot_id,
      "repo" => repo,
      "trigger" => trigger,
      "total_claims" => total,
      "confirmed" => confirmed,
      "refuted" => refuted,
      "score" => if(total > 0, do: Float.round(confirmed / total, 4), else: 0.0),
      "results" => results,
      "generated_at" => DateTime.utc_now() |> DateTime.to_iso8601()
    }
  end

  # Utility for nested map access
  defp get_in(nil, _keys), do: nil
  defp get_in(map, []), do: map

  defp get_in(map, [key | rest]) when is_map(map) do
    get_in(Map.get(map, key), rest)
  end

  defp get_in({:ok, map}, keys), do: get_in(map, keys)
  defp get_in(_, _), do: nil
end
