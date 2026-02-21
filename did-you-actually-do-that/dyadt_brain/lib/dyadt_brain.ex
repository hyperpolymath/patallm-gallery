# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain do
  @moduledoc """
  Did You Actually Do That? — Elixir Brain

  Central coordination layer for the multi-language distributed verification
  platform. Orchestrates claims through the 12-layer verification pipeline
  using Rust (core verifier) and Elixir (brain, SLM, pattern learning)
  via Erlang Ports.

  All public functions delegate to `DyadtBrain.Verifications`, the single
  business logic context module (following the stapeln `Stacks` pattern).

  ## Quick Start

      # Submit a claim for verification
      {:ok, report} = DyadtBrain.verify_claim(%{
        description: "Created configuration file",
        evidence: [%{type: "FileExists", spec: %{path: "/tmp/config.json"}}]
      })

      # Batch verify multiple claims
      {:ok, reports} = DyadtBrain.verify_batch(claims)

      # Check system health
      DyadtBrain.health_check()

  ## Architecture

  The brain coordinates a 12-layer pipeline:

  1. **File Existence** (Rust) — Verify claimed files exist
  2. **Content Hash** (Rust) — SHA-256 verification
  3. **Syntactic Validity** (Rust) — Parse claimed code
  4. **Semantic Integrity** (Rust) — Content/env checks
  5. **Test Execution** (Elixir) — Run project tests
  6. **Diff Coherence** (Rust) — Git state verification
  7. **Dependency Resolution** (Rust) — Dep graph analysis
  8. **Cross-Reference** (Rust) — Multi-claim consistency graph
  9. **Completeness Audit** (Rust) — Gap detection
  10. **Regression Guard** (Elixir) — Baseline comparison
  11. **SLM Consensus** (Rust+Elixir) — Cognitive-gating ensemble
  12. **Pattern Learning** (Elixir) — Violation frequency tracking
  """

  defdelegate verify_claim(claim), to: DyadtBrain.Verifications
  defdelegate verify_batch(claims), to: DyadtBrain.Verifications
  defdelegate get_report(claim_id), to: DyadtBrain.Verifications
  defdelegate list_claims(opts \\ []), to: DyadtBrain.Verifications
  defdelegate get_stats(), to: DyadtBrain.Verifications
  defdelegate evaluate_slm(context), to: DyadtBrain.Verifications
  defdelegate health_check(), to: DyadtBrain.Verifications
end
