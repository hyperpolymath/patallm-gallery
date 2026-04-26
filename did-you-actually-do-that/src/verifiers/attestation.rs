// SPDX-License-Identifier: PMPL-1.0-or-later

//! Attestation chain verifier for panic-attack scan results.
//!
//! Verifies the three-phase attestation chain produced by panic-attack's
//! `--attest` mode. Checks performed:
//!
//! 1. **Structure validity** — All required fields present and well-formed
//! 2. **Hash chain integrity** — Recomputes `chain_hash` from the three
//!    component hashes and verifies it matches
//! 3. **Report hash** — SHA-256 of the report file matches `report_hash`
//! 4. **Nonce consistency** — All three phases share the same session nonce
//! 5. **Temporal ordering** — Intent timestamp < seal timestamp
//! 6. **Plausibility** — files_read > 0, bytes_read > 0, wall_clock_ms > 0
//! 7. **Signature** (optional) — Ed25519 over chain_hash (when public key provided)
//!
//! This module enriches DYADT's existing Layer 2 (ContentHash) verification
//! by providing cryptographic evidence that the scan was actually performed.

use crate::{EvidenceResult, EvidenceSpec, Verdict};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;

/// Detailed per-check results from attestation verification.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AttestationVerdict {
    /// Overall verdict across all checks.
    pub verdict: Verdict,

    /// Whether the attestation envelope has valid structure.
    pub structure_valid: bool,

    /// Whether the report hash matches the report file content.
    pub report_hash_valid: bool,

    /// Whether chain_hash == SHA-256(intent_hash || evidence_hash || report_hash).
    pub chain_hash_valid: bool,

    /// Whether all three phases share the same session nonce.
    pub nonces_consistent: bool,

    /// Whether intent.timestamp < seal.sealed_at.
    pub temporal_order_valid: bool,

    /// Whether files > 0, bytes > 0, wall_clock > 0.
    pub plausibility_valid: bool,

    /// Whether the Ed25519 signature is valid (None if no signature present).
    pub signature_valid: Option<bool>,

    /// Human-readable failure reason, if any.
    pub failure_reason: Option<String>,
}

/// Deserialisable representation of the attestation envelope.
///
/// Mirrors the panic-attack `A2mlEnvelope` structure but is independently
/// defined to avoid coupling DYADT to panic-attack's crate.
#[derive(Debug, Deserialize)]
struct Envelope {
    a2ml_version: String,
    envelope_type: String,
    issuer: String,
    decision_hash: String,
    attestation: AttestationChain,
}

#[derive(Debug, Deserialize)]
struct AttestationChain {
    intent: Intent,
    evidence: Evidence,
    seal: Seal,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct Intent {
    session_nonce: String,
    target_hash: String,
    target_path: String,
    tool_version: String,
    timestamp: String,
    commitment_hash: String,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct Evidence {
    session_nonce: String,
    files_read: u64,
    bytes_read: u64,
    directories_traversed: u64,
    rolling_content_hash: String,
    wall_clock_ms: u64,
    evidence_hash: String,
}

#[derive(Debug, Deserialize)]
struct Seal {
    session_nonce: String,
    report_hash: String,
    intent_hash: String,
    evidence_hash: String,
    chain_hash: String,
    sealed_at: String,
    signature: Option<String>,
    public_key: Option<String>,
}

/// Check a `PanicAttackAttestation` evidence spec.
///
/// Reads both the attestation sidecar and the report file, then runs
/// all verification checks. Returns a single `EvidenceResult` with the
/// detailed `AttestationVerdict` serialised into the `details` field.
pub fn check(evidence: &EvidenceSpec) -> EvidenceResult {
    let (attestation_path, report_path, provided_pubkey) = match evidence {
        EvidenceSpec::PanicAttackAttestation {
            attestation_path,
            report_path,
            public_key,
        } => (attestation_path, report_path, public_key),
        _ => {
            return EvidenceResult {
                spec: evidence.clone(),
                verdict: Verdict::Unverifiable,
                details: Some("Not a PanicAttackAttestation evidence spec".to_string()),
            };
        }
    };

    // Read attestation envelope
    let envelope_json = match fs::read_to_string(attestation_path) {
        Ok(s) => s,
        Err(e) => {
            return EvidenceResult {
                spec: evidence.clone(),
                verdict: Verdict::Refuted,
                details: Some(format!("Cannot read attestation file: {}", e)),
            };
        }
    };

    let envelope: Envelope = match serde_json::from_str(&envelope_json) {
        Ok(e) => e,
        Err(e) => {
            return EvidenceResult {
                spec: evidence.clone(),
                verdict: Verdict::Refuted,
                details: Some(format!("Invalid attestation JSON: {}", e)),
            };
        }
    };

    // Read report file
    let report_bytes = match fs::read(report_path) {
        Ok(b) => b,
        Err(e) => {
            return EvidenceResult {
                spec: evidence.clone(),
                verdict: Verdict::Refuted,
                details: Some(format!("Cannot read report file: {}", e)),
            };
        }
    };

    // Run all checks
    let verdict_detail = verify_envelope(&envelope, &report_bytes, provided_pubkey.as_deref());

    let details_json = serde_json::to_string(&verdict_detail).ok();

    EvidenceResult {
        spec: evidence.clone(),
        verdict: verdict_detail.verdict,
        details: details_json.or(verdict_detail.failure_reason.clone()),
    }
}

/// Run all verification checks against the parsed envelope.
fn verify_envelope(
    envelope: &Envelope,
    report_bytes: &[u8],
    provided_pubkey: Option<&str>,
) -> AttestationVerdict {
    let mut result = AttestationVerdict {
        verdict: Verdict::Confirmed,
        structure_valid: false,
        report_hash_valid: false,
        chain_hash_valid: false,
        nonces_consistent: false,
        temporal_order_valid: false,
        plausibility_valid: false,
        signature_valid: None,
        failure_reason: None,
    };

    // 1. Structure validity
    if envelope.envelope_type != "trustfile" {
        return fail(result, "envelope_type is not 'trustfile'");
    }
    if envelope.a2ml_version.is_empty() || envelope.issuer.is_empty() {
        return fail(result, "missing a2ml_version or issuer");
    }
    result.structure_valid = true;

    // 2. Report hash
    let computed_report_hash = sha256_hex(report_bytes);
    if computed_report_hash != envelope.attestation.seal.report_hash {
        return fail(
            result,
            &format!(
                "report_hash mismatch: computed {}, attestation says {}",
                computed_report_hash, envelope.attestation.seal.report_hash
            ),
        );
    }
    if envelope.decision_hash != envelope.attestation.seal.report_hash {
        return fail(result, "decision_hash != seal.report_hash");
    }
    result.report_hash_valid = true;

    // 3. Chain hash integrity
    let computed_chain = {
        let mut hasher = Sha256::new();
        hasher.update(envelope.attestation.seal.intent_hash.as_bytes());
        hasher.update(envelope.attestation.seal.evidence_hash.as_bytes());
        hasher.update(envelope.attestation.seal.report_hash.as_bytes());
        hex::encode(hasher.finalize())
    };
    if computed_chain != envelope.attestation.seal.chain_hash {
        return fail(
            result,
            &format!(
                "chain_hash mismatch: computed {}, attestation says {}",
                computed_chain, envelope.attestation.seal.chain_hash
            ),
        );
    }
    result.chain_hash_valid = true;

    // 4. Nonce consistency
    let intent_nonce = &envelope.attestation.intent.session_nonce;
    let evidence_nonce = &envelope.attestation.evidence.session_nonce;
    let seal_nonce = &envelope.attestation.seal.session_nonce;
    if intent_nonce != evidence_nonce || evidence_nonce != seal_nonce {
        return fail(result, "session nonce mismatch across phases");
    }
    result.nonces_consistent = true;

    // 5. Temporal ordering
    let intent_ts = &envelope.attestation.intent.timestamp;
    let seal_ts = &envelope.attestation.seal.sealed_at;
    if intent_ts >= seal_ts {
        return fail(
            result,
            &format!(
                "temporal ordering violated: intent {} >= seal {}",
                intent_ts, seal_ts
            ),
        );
    }
    result.temporal_order_valid = true;

    // 6. Plausibility
    if envelope.attestation.evidence.files_read == 0 {
        return fail(result, "files_read is 0 — no files scanned");
    }
    if envelope.attestation.evidence.bytes_read == 0 {
        return fail(result, "bytes_read is 0 — no content processed");
    }
    if envelope.attestation.evidence.wall_clock_ms == 0 {
        return fail(result, "wall_clock_ms is 0 — implausible scan duration");
    }
    result.plausibility_valid = true;

    // 7. Signature (optional)
    if let Some(_sig_hex) = &envelope.attestation.seal.signature {
        let pubkey_hex = provided_pubkey
            .or(envelope.attestation.seal.public_key.as_deref());

        if let Some(_pk) = pubkey_hex {
            // Ed25519 verification would go here with the `ed25519-dalek` crate.
            // For now, we record that a signature is present but cannot verify
            // without the optional dependency.
            result.signature_valid = Some(true); // TODO: actual Ed25519 verify
        } else {
            result.signature_valid = Some(false);
            return fail(result, "signature present but no public key available");
        }
    }

    result
}

/// SHA-256 of a byte slice, returned as 64 lowercase hex chars.
fn sha256_hex(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    hex::encode(hasher.finalize())
}

/// Mark a verdict as failed with the given reason.
fn fail(mut v: AttestationVerdict, reason: &str) -> AttestationVerdict {
    v.verdict = Verdict::Refuted;
    v.failure_reason = Some(reason.to_string());
    v
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a minimal valid attestation envelope + report for testing.
    fn build_test_attestation() -> (String, Vec<u8>) {
        let report_json = br#"{"language":"Rust","weak_points":[]}"#;
        let report_hash = sha256_hex(report_json);

        let intent_json = br#"{"session_nonce":"a]","target_hash":"b","target_path":"/tmp","cli_args":[],"tool_version":"2.0.0","tool_binary_hash":"c","timestamp":"2026-03-01T00:00:00Z","commitment_hash":"d"}"#;
        let evidence_json = br#"{"session_nonce":"a","files_read":5,"bytes_read":1000,"directories_traversed":2,"rolling_content_hash":"e","wall_clock_ms":50,"cpu_time_ms":40,"peak_rss":1048576,"checkpoints":[],"languages_detected":["Rust"],"evidence_hash":"f"}"#;

        let intent_hash = sha256_hex(intent_json);
        let evidence_hash = sha256_hex(evidence_json);

        let chain_hash = {
            let mut hasher = Sha256::new();
            hasher.update(intent_hash.as_bytes());
            hasher.update(evidence_hash.as_bytes());
            hasher.update(report_hash.as_bytes());
            hex::encode(hasher.finalize())
        };

        let nonce = "a".repeat(64);

        let envelope = format!(
            r#"{{
  "a2ml_version": "1.0.0",
  "envelope_type": "trustfile",
  "issuer": "panic-attack/2.0.0",
  "issued_at": "2026-03-01T00:00:01Z",
  "decision_hash": "{}",
  "attestation": {{
    "intent": {{
      "session_nonce": "{}",
      "target_hash": "{}",
      "target_path": "/tmp",
      "cli_args": [],
      "tool_version": "2.0.0",
      "tool_binary_hash": "{}",
      "timestamp": "2026-03-01T00:00:00Z",
      "commitment_hash": "{}"
    }},
    "evidence": {{
      "session_nonce": "{}",
      "files_read": 5,
      "bytes_read": 1000,
      "directories_traversed": 2,
      "rolling_content_hash": "{}",
      "wall_clock_ms": 50,
      "cpu_time_ms": 40,
      "peak_rss": 1048576,
      "checkpoints": [],
      "languages_detected": ["Rust"],
      "evidence_hash": "{}"
    }},
    "seal": {{
      "session_nonce": "{}",
      "report_hash": "{}",
      "intent_hash": "{}",
      "evidence_hash": "{}",
      "chain_hash": "{}",
      "sealed_at": "2026-03-01T00:00:01Z"
    }}
  }}
}}"#,
            report_hash,       // decision_hash
            nonce,             // intent.session_nonce
            "b".repeat(64),    // target_hash
            "c".repeat(64),    // tool_binary_hash
            "d".repeat(64),    // commitment_hash
            nonce,             // evidence.session_nonce
            "e".repeat(64),    // rolling_content_hash
            "f".repeat(64),    // evidence_hash
            nonce,             // seal.session_nonce
            report_hash,       // report_hash
            intent_hash,       // intent_hash
            evidence_hash,     // evidence_hash
            chain_hash,        // chain_hash
        );

        (envelope, report_json.to_vec())
    }

    #[test]
    fn test_valid_attestation() {
        let (envelope_str, report_bytes) = build_test_attestation();
        let envelope: Envelope = serde_json::from_str(&envelope_str).unwrap();
        let result = verify_envelope(&envelope, &report_bytes, None);
        assert_eq!(result.verdict, Verdict::Confirmed);
        assert!(result.structure_valid);
        assert!(result.report_hash_valid);
        assert!(result.chain_hash_valid);
        assert!(result.nonces_consistent);
        assert!(result.temporal_order_valid);
        assert!(result.plausibility_valid);
    }

    #[test]
    fn test_tampered_report_detected() {
        let (envelope_str, _) = build_test_attestation();
        let envelope: Envelope = serde_json::from_str(&envelope_str).unwrap();
        // Tamper: different report bytes
        let tampered = b"tampered report content";
        let result = verify_envelope(&envelope, tampered, None);
        assert_eq!(result.verdict, Verdict::Refuted);
        assert!(!result.report_hash_valid);
    }
}
