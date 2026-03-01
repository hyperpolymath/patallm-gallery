# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.Attestation do
  @moduledoc """
  Attestation verification for panic-attack scan results.

  Delegates to the Rust verifier via the Port protocol to verify that a
  panic-attack `.attestation.json` sidecar file cryptographically proves
  the scan was genuinely performed. The three-phase attestation chain
  (intent → evidence → seal) is checked for:

  - Hash chain integrity (recomputed SHA-256 hashes)
  - Report hash matches the actual report file
  - Nonce consistency across all three phases
  - Temporal ordering (intent before seal)
  - Plausibility metrics (files > 0, bytes > 0, time > 0)
  - Optional Ed25519 signature verification

  ## Usage

      iex> DyadtBrain.Attestation.verify("report.attestation.json", "report.json")
      {:ok, %{"verdict" => "Confirmed", ...}}
  """

  alias DyadtBrain.Ports.RustPort

  @doc """
  Verify a panic-attack attestation chain.

  Reads the attestation sidecar and report file, sends them to the Rust
  verifier, and returns the attestation verdict.

  ## Parameters

    - `attestation_path` — Path to the `.attestation.json` sidecar file
    - `report_path` — Path to the corresponding scan report JSON
    - `opts` — Keyword list of options:
      - `:public_key` — Hex-encoded Ed25519 public key for signature verification

  ## Returns

    - `{:ok, verdict}` where `verdict` is a map with per-check booleans
    - `{:error, reason}` on failure
  """
  @spec verify(String.t(), String.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def verify(attestation_path, report_path, opts \\ []) do
    public_key = Keyword.get(opts, :public_key)

    params = %{
      "attestation_path" => attestation_path,
      "report_path" => report_path
    }

    params =
      if public_key do
        Map.put(params, "public_key", public_key)
      else
        params
      end

    RustPort.call("verify_attestation", params)
  end

  @doc """
  Check whether an attestation sidecar exists for the given report path.

  Looks for a `.attestation.json` file alongside the report. For example,
  if `report_path` is `reports/scan.json`, checks for
  `reports/scan.attestation.json`.
  """
  @spec has_attestation?(String.t()) :: boolean()
  def has_attestation?(report_path) do
    attestation_path = derive_attestation_path(report_path)
    File.exists?(attestation_path)
  end

  @doc """
  Derive the attestation sidecar path from a report path.

  Replaces the extension with `.attestation.json`. For example:
  - `report.json` → `report.attestation.json`
  - `scan-20260301.json` → `scan-20260301.attestation.json`
  """
  @spec derive_attestation_path(String.t()) :: String.t()
  def derive_attestation_path(report_path) do
    base = Path.rootname(report_path)
    "#{base}.attestation.json"
  end
end
