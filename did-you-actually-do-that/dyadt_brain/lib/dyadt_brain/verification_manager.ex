# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.VerificationManager do
  @moduledoc """
  Orchestrates the 12-layer verification pipeline.

  For each claim:
  1. Register claim in ClaimRegistry
  2. Send to Rust verifier via Port (Layers 1-4, 6-8)
  3. Run Elixir-side checks (Layers 5, 10)
  4. Collect results from all pillars
  5. Feed to VerdictEngine for final verdict
  6. Store report in ClaimRegistry
  """

  use GenServer

  alias DyadtBrain.{ClaimRegistry, Ports.RustPort, SurveillanceSupervisor, VerdictEngine}
  alias DyadtBrain.VeriSimDB.ClaimStore

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Verify a single claim through the full pipeline."
  def verify(claim) when is_map(claim) do
    GenServer.call(__MODULE__, {:verify, claim}, :timer.seconds(30))
  end

  @doc "Verify a batch of claims."
  def verify_batch(claims) when is_list(claims) do
    GenServer.call(__MODULE__, {:verify_batch, claims}, :timer.seconds(60))
  end

  # Server callbacks

  @impl true
  def init(_opts) do
    {:ok, %{active_verifications: %{}}}
  end

  @impl true
  def handle_call({:verify, claim}, _from, state) do
    result = do_verify(claim)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:verify_batch, claims}, _from, state) do
    results = Enum.map(claims, &do_verify/1)
    {:reply, {:ok, results}, state}
  end

  defp do_verify(claim) do
    # Step 1: Register
    {:ok, claim_id} = ClaimRegistry.register(claim)
    ClaimRegistry.update(claim_id, :verifying)

    # Step 2: Send to Rust verifier for Layers 1-4, 6-8
    rust_result = RustPort.verify_claim(claim)

    # Step 3: Aggregate results (for now, just Rust layers)
    layer_results =
      case rust_result do
        {:ok, enriched} ->
          Map.get(enriched, "layer_results", [])

        {:error, reason} ->
          [%{"layer" => "Error", "error" => reason}]
      end

    # Step 4: Compute final verdict
    report =
      case rust_result do
        {:ok, enriched} ->
          Map.get(enriched, "report", %{})

        {:error, reason} ->
          %{
            "overall_verdict" => "Unverifiable",
            "error" => reason,
            "claim" => claim
          }
      end

    final_verdict = VerdictEngine.aggregate(layer_results)

    # Step 5: Run SLM consensus (Layer 11) if applicable
    slm_result = run_slm_evaluation(claim, layer_results)

    full_report = Map.merge(report, %{
      "final_verdict" => final_verdict,
      "layer_results" => layer_results,
      "slm_result" => slm_result,
      "verified_at" => DateTime.utc_now() |> DateTime.to_iso8601()
    })

    # Step 6: Store locally
    ClaimRegistry.update(claim_id, :verified, full_report)

    # Step 7: Persist to VeriSimDB (best-effort, don't fail verification)
    persist_to_verisimdb(claim_id, claim, layer_results, slm_result)

    {:ok, full_report}
  end

  defp run_slm_evaluation(claim, layer_results) do
    evidence_summary =
      layer_results
      |> Enum.map(fn lr ->
        verdict = get_in(lr, ["evidence_result", "verdict"]) || "?"
        layer = lr["layer"] || "?"
        "#{layer}: #{verdict}"
      end)
      |> Enum.join(", ")

    context = %{
      "claim_text" => claim["description"] || "",
      "evidence_summary" => evidence_summary,
      "artifact_sample" => nil,
      "diff_sample" => nil
    }

    case SurveillanceSupervisor.evaluate(context) do
      {:ok, result} -> result
      {:error, _reason} -> nil
    end
  end

  defp persist_to_verisimdb(claim_id, claim, layer_results, slm_result) do
    # Best-effort: log but don't crash on failure
    Task.start(fn ->
      ClaimStore.store_claim(claim_id, claim)

      Enum.each(layer_results, fn lr ->
        ClaimStore.store_result(claim_id, lr)
      end)

      if slm_result do
        ClaimStore.store_votes(claim_id, slm_result)
      end
    end)
  end
end
