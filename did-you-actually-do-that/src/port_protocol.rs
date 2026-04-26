// SPDX-License-Identifier: PMPL-1.0-or-later
//! Erlang Port binary protocol for communication with the Elixir brain.
//!
//! The Port protocol uses length-prefixed JSON messages over stdin/stdout.
//! Each message is prefixed with a 4-byte big-endian length, followed by
//! the JSON payload.
//!
//! ## Protocol
//!
//! ```text
//! [4 bytes: length BE] [N bytes: JSON payload]
//! ```
//!
//! ## Request Format
//!
//! ```json
//! {
//!   "id": "request-uuid",
//!   "method": "verify_claim" | "verify_batch" | "check_evidence" | "extract_claims",
//!   "params": { ... }
//! }
//! ```
//!
//! ## Response Format
//!
//! ```json
//! {
//!   "id": "request-uuid",
//!   "result": { ... }
//! }
//! ```
//!
//! Or on error:
//!
//! ```json
//! {
//!   "id": "request-uuid",
//!   "error": { "code": -1, "message": "description" }
//! }
//! ```

use crate::{Claim, Verifier, VerificationReport};
use crate::claim_extractor::{ClaimExtractor, ExtractionContext};
use crate::slm::ensemble::Ensemble;
use crate::slm::EvaluationContext;
use crate::verifiers::{self, LayerResult};
use serde::{Deserialize, Serialize};
use std::io::{self, Read, Write};

/// A request from the Elixir brain.
#[derive(Debug, Deserialize)]
pub struct PortRequest {
    pub id: String,
    pub method: String,
    pub params: serde_json::Value,
}

/// A successful response to the brain.
#[derive(Debug, Serialize)]
pub struct PortResponse {
    pub id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<PortError>,
}

/// An error in a response.
#[derive(Debug, Serialize)]
pub struct PortError {
    pub code: i32,
    pub message: String,
}

impl PortResponse {
    pub fn ok(id: String, result: serde_json::Value) -> Self {
        Self {
            id,
            result: Some(result),
            error: None,
        }
    }

    pub fn err(id: String, code: i32, message: String) -> Self {
        Self {
            id,
            result: None,
            error: Some(PortError { code, message }),
        }
    }
}

/// Read a length-prefixed message from stdin.
fn read_message(reader: &mut impl Read) -> io::Result<Option<Vec<u8>>> {
    let mut len_buf = [0u8; 4];
    match reader.read_exact(&mut len_buf) {
        Ok(()) => {}
        Err(ref e) if e.kind() == io::ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e),
    }

    let len = u32::from_be_bytes(len_buf) as usize;
    if len == 0 {
        return Ok(Some(Vec::new()));
    }

    let mut buf = vec![0u8; len];
    reader.read_exact(&mut buf)?;
    Ok(Some(buf))
}

/// Write a length-prefixed message to stdout.
fn write_message(writer: &mut impl Write, data: &[u8]) -> io::Result<()> {
    let len = data.len() as u32;
    writer.write_all(&len.to_be_bytes())?;
    writer.write_all(data)?;
    writer.flush()?;
    Ok(())
}

/// Handle a single request and produce a response.
fn handle_request(
    verifier: &Verifier,
    ensemble: &Ensemble,
    request: &PortRequest,
) -> PortResponse {
    match request.method.as_str() {
        "verify_claim" => handle_verify_claim(verifier, request),
        "verify_batch" => handle_verify_batch(verifier, request),
        "check_evidence" => handle_check_evidence(request),
        "extract_claims" => handle_extract_claims(request),
        "evaluate_slm" => handle_evaluate_slm(ensemble, request),
        "evaluate_got" => handle_evaluate_got(request),
        "evaluate_moe" => handle_evaluate_moe(request),
        "verify_attestation" => handle_verify_attestation(request),
        "ping" => PortResponse::ok(request.id.clone(), serde_json::json!({"pong": true})),
        _ => PortResponse::err(
            request.id.clone(),
            -32601,
            format!("Unknown method: {}", request.method),
        ),
    }
}

fn handle_verify_claim(verifier: &Verifier, request: &PortRequest) -> PortResponse {
    match serde_json::from_value::<Claim>(request.params.clone()) {
        Ok(claim) => {
            let report = verifier.verify(&claim);

            // Enrich with layer information
            let layer_results: Vec<LayerResult> = claim
                .evidence
                .iter()
                .map(|ev| {
                    let (layer, result) = verifiers::check_evidence_by_layer(ev);
                    LayerResult {
                        layer,
                        evidence_result: result,
                    }
                })
                .collect();

            match serde_json::to_value(&EnrichedReport {
                report,
                layer_results,
            }) {
                Ok(val) => PortResponse::ok(request.id.clone(), val),
                Err(e) => PortResponse::err(request.id.clone(), -1, e.to_string()),
            }
        }
        Err(e) => PortResponse::err(
            request.id.clone(),
            -32602,
            format!("Invalid claim: {}", e),
        ),
    }
}

fn handle_verify_batch(verifier: &Verifier, request: &PortRequest) -> PortResponse {
    match serde_json::from_value::<Vec<Claim>>(request.params.clone()) {
        Ok(claims) => {
            let reports: Vec<VerificationReport> =
                claims.iter().map(|c| verifier.verify(c)).collect();
            match serde_json::to_value(&reports) {
                Ok(val) => PortResponse::ok(request.id.clone(), val),
                Err(e) => PortResponse::err(request.id.clone(), -1, e.to_string()),
            }
        }
        Err(e) => PortResponse::err(
            request.id.clone(),
            -32602,
            format!("Invalid claims array: {}", e),
        ),
    }
}

fn handle_check_evidence(request: &PortRequest) -> PortResponse {
    match serde_json::from_value::<crate::EvidenceSpec>(request.params.clone()) {
        Ok(evidence) => {
            let (layer, result) = verifiers::check_evidence_by_layer(&evidence);
            let layer_result = LayerResult {
                layer,
                evidence_result: result,
            };
            match serde_json::to_value(&layer_result) {
                Ok(val) => PortResponse::ok(request.id.clone(), val),
                Err(e) => PortResponse::err(request.id.clone(), -1, e.to_string()),
            }
        }
        Err(e) => PortResponse::err(
            request.id.clone(),
            -32602,
            format!("Invalid evidence spec: {}", e),
        ),
    }
}

fn handle_extract_claims(request: &PortRequest) -> PortResponse {
    #[derive(Deserialize)]
    struct ExtractParams {
        text: String,
        #[serde(default)]
        source: Option<String>,
        #[serde(default)]
        working_directory: Option<String>,
    }

    match serde_json::from_value::<ExtractParams>(request.params.clone()) {
        Ok(params) => {
            let extractor = ClaimExtractor::new();
            let context = ExtractionContext {
                source: params.source.unwrap_or_else(|| "port-client".to_string()),
                working_directory: params.working_directory,
                conversation_id: None,
                session_start: None,
            };
            let claims = extractor.extract_from_text(&params.text, &context);
            match serde_json::to_value(&claims) {
                Ok(val) => PortResponse::ok(request.id.clone(), val),
                Err(e) => PortResponse::err(request.id.clone(), -1, e.to_string()),
            }
        }
        Err(e) => PortResponse::err(
            request.id.clone(),
            -32602,
            format!("Invalid extract params: {}", e),
        ),
    }
}

fn handle_evaluate_slm(ensemble: &Ensemble, request: &PortRequest) -> PortResponse {
    match serde_json::from_value::<EvaluationContext>(request.params.clone()) {
        Ok(context) => {
            let result = ensemble.evaluate(&context);
            match serde_json::to_value(&result) {
                Ok(val) => PortResponse::ok(request.id.clone(), val),
                Err(e) => PortResponse::err(request.id.clone(), -1, e.to_string()),
            }
        }
        Err(e) => PortResponse::err(
            request.id.clone(),
            -32602,
            format!("Invalid evaluation context: {}", e),
        ),
    }
}

fn handle_evaluate_got(request: &PortRequest) -> PortResponse {
    use crate::slm::graph_of_thought::GraphOfThought;

    match serde_json::from_value::<EvaluationContext>(request.params.clone()) {
        Ok(context) => {
            let got = GraphOfThought::new();
            let result = got.evaluate_with_traces(&context);
            match serde_json::to_value(&result) {
                Ok(val) => PortResponse::ok(request.id.clone(), val),
                Err(e) => PortResponse::err(request.id.clone(), -1, e.to_string()),
            }
        }
        Err(e) => PortResponse::err(
            request.id.clone(),
            -32602,
            format!("Invalid evaluation context: {}", e),
        ),
    }
}

fn handle_evaluate_moe(request: &PortRequest) -> PortResponse {
    use crate::slm::mixture_of_experts::MixtureOfExperts;

    match serde_json::from_value::<EvaluationContext>(request.params.clone()) {
        Ok(context) => {
            let moe = MixtureOfExperts::new();
            let result = moe.evaluate_with_experts(&context);
            match serde_json::to_value(&result) {
                Ok(val) => PortResponse::ok(request.id.clone(), val),
                Err(e) => PortResponse::err(request.id.clone(), -1, e.to_string()),
            }
        }
        Err(e) => PortResponse::err(
            request.id.clone(),
            -32602,
            format!("Invalid evaluation context: {}", e),
        ),
    }
}

/// Handle a `verify_attestation` request from the Elixir brain.
///
/// Expects params: `{ attestation_path, report_path, public_key? }`.
/// Returns the `AttestationVerdict` from the attestation verifier.
fn handle_verify_attestation(request: &PortRequest) -> PortResponse {
    #[derive(Deserialize)]
    struct AttestParams {
        attestation_path: String,
        report_path: String,
        #[serde(default)]
        public_key: Option<String>,
    }

    match serde_json::from_value::<AttestParams>(request.params.clone()) {
        Ok(params) => {
            let evidence = crate::EvidenceSpec::PanicAttackAttestation {
                attestation_path: params.attestation_path,
                report_path: params.report_path,
                public_key: params.public_key,
            };
            let (layer, result) = verifiers::check_evidence_by_layer(&evidence);
            let layer_result = LayerResult {
                layer,
                evidence_result: result,
            };
            match serde_json::to_value(&layer_result) {
                Ok(val) => PortResponse::ok(request.id.clone(), val),
                Err(e) => PortResponse::err(request.id.clone(), -1, e.to_string()),
            }
        }
        Err(e) => PortResponse::err(
            request.id.clone(),
            -32602,
            format!("Invalid attestation params: {}", e),
        ),
    }
}

/// Enriched verification report with layer metadata.
#[derive(Debug, Serialize, Deserialize)]
pub struct EnrichedReport {
    pub report: VerificationReport,
    pub layer_results: Vec<LayerResult>,
}

/// Run the Port protocol server, reading from stdin and writing to stdout.
///
/// This function runs forever (until stdin is closed), processing one
/// request at a time. The Elixir brain starts this as a Port process.
pub fn serve() -> io::Result<()> {
    let verifier = Verifier::new();
    let ensemble = Ensemble::full();
    // Future: load SLM backends from config and add to ensemble
    // ensemble.add_backend(Box::new(LlamaCppBackend::load("path/to/model.gguf")?));

    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();

    // Send a ready signal
    let ready = serde_json::json!({
        "status": "ready",
        "version": env!("CARGO_PKG_VERSION"),
        "slm_backends": ensemble.backend_count(),
    });
    let ready_bytes = serde_json::to_vec(&ready)?;
    write_message(&mut stdout, &ready_bytes)?;

    loop {
        match read_message(&mut stdin)? {
            Some(data) => {
                if data.is_empty() {
                    continue;
                }

                let response = match serde_json::from_slice::<PortRequest>(&data) {
                    Ok(request) => handle_request(&verifier, &ensemble, &request),
                    Err(e) => PortResponse::err(
                        "unknown".to_string(),
                        -32700,
                        format!("Parse error: {}", e),
                    ),
                };

                let response_bytes = serde_json::to_vec(&response)?;
                write_message(&mut stdout, &response_bytes)?;
            }
            None => {
                // stdin closed — brain process ended
                break;
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_port_response_ok() {
        let resp = PortResponse::ok("test-1".to_string(), serde_json::json!({"pong": true}));
        assert!(resp.result.is_some());
        assert!(resp.error.is_none());
    }

    #[test]
    fn test_port_response_err() {
        let resp = PortResponse::err("test-2".to_string(), -1, "oops".to_string());
        assert!(resp.result.is_none());
        assert!(resp.error.is_some());
        assert_eq!(resp.error.unwrap().code, -1);
    }

    #[test]
    fn test_handle_ping() {
        let verifier = Verifier::new();
        let ensemble = Ensemble::full();
        let request = PortRequest {
            id: "ping-1".to_string(),
            method: "ping".to_string(),
            params: serde_json::json!({}),
        };
        let response = handle_request(&verifier, &ensemble, &request);
        assert!(response.result.is_some());
        assert_eq!(response.result.unwrap()["pong"], true);
    }

    #[test]
    fn test_handle_unknown_method() {
        let verifier = Verifier::new();
        let ensemble = Ensemble::full();
        let request = PortRequest {
            id: "unk-1".to_string(),
            method: "nonexistent".to_string(),
            params: serde_json::json!({}),
        };
        let response = handle_request(&verifier, &ensemble, &request);
        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, -32601);
    }

    #[test]
    fn test_handle_evaluate_slm() {
        let verifier = Verifier::new();
        let ensemble = Ensemble::full();
        let request = PortRequest {
            id: "slm-1".to_string(),
            method: "evaluate_slm".to_string(),
            params: serde_json::json!({
                "claim_text": "Added authentication module",
                "evidence_summary": "FileExists: Confirmed",
                "artifact_sample": "pub fn login() -> Result<Token, Error> { db.verify(user, hash) }\n",
            }),
        };
        let response = handle_request(&verifier, &ensemble, &request);
        assert!(response.result.is_some());
        let result = response.result.unwrap();
        assert!(result["votes"].is_array());
        assert!(result["suggested_verdict"].is_string());
    }

    #[test]
    fn test_read_write_message_roundtrip() {
        let data = b"hello world";
        let mut buf = Vec::new();
        write_message(&mut buf, data).unwrap();

        let mut cursor = io::Cursor::new(buf);
        let result = read_message(&mut cursor).unwrap().unwrap();
        assert_eq!(result, data);
    }
}
