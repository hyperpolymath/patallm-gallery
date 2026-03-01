K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# K9 Yard-level contractile: panic-attack attestation structure validation
# Security Level: Yard (Nickel evaluation with contracts)
# Signature recommended but not required
#
# Validates the structure and cross-field consistency of a panic-attack
# attestation envelope (.attestation.json). Uses Nickel contracts to
# enforce:
#   - SHA-256 hash format (64 hex chars)
#   - Nonce format (64 hex chars)
#   - Nonce consistency across all three phases
#   - Temporal ordering (intent < checkpoints < seal)
#   - Plausibility (files > 0, bytes > 0, wall_clock > 0)
#
# Does NOT perform cryptographic verification — that requires subprocess
# access (Hunt level). This contractile catches structural/logical errors
# that would indicate fabrication or corruption.

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "attestation-schema",
    security = {
      leash = 'Yard,
      trust_level = "validated-config",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = {
      name = "panic-attack-attested",
      version = "1.0.0",
      description = "Attestation structure validation for panic-attack scan results. Validates hash formats, nonce consistency, temporal ordering, and plausibility without cryptographic verification.",
      author = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>",
    },
  },

  # Contract definitions for attestation structure
  contracts = {
    # SHA-256 hash: exactly 64 lowercase hex characters
    Sha256Hash =
      std.contract.from_predicate (fun s =>
        std.is_string s
        && std.string.length s == 64
        && std.string.is_match "^[0-9a-f]{64}$" s
      ),

    # Session nonce: exactly 64 lowercase hex characters (32 bytes)
    SessionNonce =
      std.contract.from_predicate (fun s =>
        std.is_string s
        && std.string.length s == 64
        && std.string.is_match "^[0-9a-f]{64}$" s
      ),

    # ISO 8601 timestamp (basic format check)
    Iso8601Timestamp =
      std.contract.from_predicate (fun s =>
        std.is_string s
        && std.string.length s >= 20
        && std.string.is_match "^[0-9]{4}-[0-9]{2}-[0-9]{2}T" s
      ),

    # Envelope type must be "trustfile"
    TrustfileType =
      std.contract.from_predicate (fun s =>
        s == "trustfile"
      ),

    # Evidence checkpoint
    EvidenceCheckpoint = {
      files_at_checkpoint | Number,
      rolling_hash | String,
      timestamp | String,
    },

    # Pre-execution intent
    ExecutionIntent = {
      session_nonce | String,
      target_hash | String,
      target_path | String,
      cli_args | Array String,
      tool_version | String,
      tool_binary_hash | String,
      timestamp | String,
      commitment_hash | String,
    },

    # Execution evidence
    ExecutionEvidence = {
      session_nonce | String,
      files_read | Number,
      bytes_read | Number,
      directories_traversed | Number,
      rolling_content_hash | String,
      wall_clock_ms | Number,
      cpu_time_ms | Number,
      peak_rss | Number,
      checkpoints | Array EvidenceCheckpoint,
      languages_detected | Array String,
      evidence_hash | String,
    },

    # Post-execution seal
    ReportSeal = {
      session_nonce | String,
      report_hash | String,
      intent_hash | String,
      evidence_hash | String,
      chain_hash | String,
      sealed_at | String,
      ..
    },

    # The attestation chain
    AttestationChain = {
      intent | ExecutionIntent,
      evidence | ExecutionEvidence,
      seal | ReportSeal,
    },

    # The A2ML envelope
    A2mlEnvelope = {
      a2ml_version | String,
      envelope_type | String,
      issuer | String,
      issued_at | String,
      decision_hash | String,
      attestation | AttestationChain,
    },
  },

  # Cross-field validation rules
  validation = {
    # All three phases must share the same session nonce
    nonces_match =
      std.contract.from_predicate (fun envelope =>
        let intent_nonce = envelope.attestation.intent.session_nonce in
        let evidence_nonce = envelope.attestation.evidence.session_nonce in
        let seal_nonce = envelope.attestation.seal.session_nonce in
        intent_nonce == evidence_nonce && evidence_nonce == seal_nonce
      ),

    # Envelope type must be "trustfile"
    envelope_type_valid =
      std.contract.from_predicate (fun envelope =>
        envelope.envelope_type == "trustfile"
      ),

    # decision_hash must equal seal.report_hash
    decision_hash_matches =
      std.contract.from_predicate (fun envelope =>
        envelope.decision_hash == envelope.attestation.seal.report_hash
      ),

    # Plausibility: files_read > 0
    files_positive =
      std.contract.from_predicate (fun envelope =>
        envelope.attestation.evidence.files_read > 0
      ),

    # Plausibility: bytes_read > 0
    bytes_positive =
      std.contract.from_predicate (fun envelope =>
        envelope.attestation.evidence.bytes_read > 0
      ),

    # Plausibility: wall_clock_ms > 0
    wall_clock_positive =
      std.contract.from_predicate (fun envelope =>
        envelope.attestation.evidence.wall_clock_ms > 0
      ),

    # Checkpoints must be ordered by files_at_checkpoint
    checkpoints_ordered =
      std.contract.from_predicate (fun envelope =>
        let cps = envelope.attestation.evidence.checkpoints in
        if std.array.length cps <= 1 then
          true
        else
          # Check each consecutive pair is ordered
          let indices = std.array.range 0 (std.array.length cps - 1) in
          std.array.all
            (fun i =>
              (std.array.at i cps).files_at_checkpoint
              < (std.array.at (i + 1) cps).files_at_checkpoint
            )
            indices
      ),

    # Temporal ordering: intent.timestamp < seal.sealed_at
    temporal_ordering =
      std.contract.from_predicate (fun envelope =>
        # String comparison works for ISO 8601 timestamps
        envelope.attestation.intent.timestamp
        < envelope.attestation.seal.sealed_at
      ),
  },
}
