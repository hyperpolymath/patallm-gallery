// SPDX-License-Identifier: MPL-2.0
//! MCP (Model Context Protocol) server for AI self-verification
//!
//! This module provides an MCP server that allows AI systems to verify their
//! claimed actions against reality. It exposes verification tools through
//! the standard MCP JSON-RPC interface.
//!
//! ## Features
//!
//! - `verify_claim` tool - Verify a single claim with evidence
//! - `quick_verify` tool - Quick file/directory existence check
//! - `compute_hash` tool - Compute SHA-256 hash for evidence specs
//! - `verify_batch` tool - Verify multiple claims at once
//!
//! ## Usage
//!
//! Run as a standalone MCP server:
//! ```bash
//! dyadt mcp-server
//! ```
//!
//! Or use in Claude Code:
//! ```json
//! {
//!   "mcpServers": {
//!     "dyadt": {
//!       "command": "dyadt",
//!       "args": ["mcp-server"]
//!     }
//!   }
//! }
//! ```

use crate::{Claim, EvidenceSpec, Verdict, VerificationReport, Verifier};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use sha2::{Digest, Sha256};
use std::io::{self, BufRead, Write};

/// MCP Server version
const MCP_VERSION: &str = "2024-11-05";

/// Tool definition
#[derive(Debug, Serialize)]
struct ToolDef {
    name: String,
    description: String,
    #[serde(rename = "inputSchema")]
    input_schema: Value,
}

/// JSON-RPC request
#[derive(Debug, Deserialize)]
struct JsonRpcRequest {
    #[allow(dead_code)]
    jsonrpc: String,
    id: Option<Value>,
    method: String,
    params: Option<Value>,
}

/// JSON-RPC response
#[derive(Debug, Serialize)]
struct JsonRpcResponse {
    jsonrpc: String,
    id: Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<JsonRpcError>,
}

/// JSON-RPC error
#[derive(Debug, Serialize)]
struct JsonRpcError {
    code: i32,
    message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    data: Option<Value>,
}

/// MCP Server
pub struct McpServer {
    verifier: Verifier,
}

impl Default for McpServer {
    fn default() -> Self {
        Self::new()
    }
}

impl McpServer {
    /// Create a new MCP server
    pub fn new() -> Self {
        Self {
            verifier: Verifier::new(),
        }
    }

    /// Run the MCP server on stdio
    pub fn run(&self) -> io::Result<()> {
        let stdin = io::stdin();
        let mut stdout = io::stdout();

        for line in stdin.lock().lines() {
            let line = line?;
            if line.is_empty() {
                continue;
            }

            let request: JsonRpcRequest = match serde_json::from_str(&line) {
                Ok(req) => req,
                Err(e) => {
                    let error_response = JsonRpcResponse {
                        jsonrpc: "2.0".to_string(),
                        id: Value::Null,
                        result: None,
                        error: Some(JsonRpcError {
                            code: -32700,
                            message: format!("Parse error: {}", e),
                            data: None,
                        }),
                    };
                    writeln!(stdout, "{}", serde_json::to_string(&error_response)?)?;
                    stdout.flush()?;
                    continue;
                }
            };

            let response = self.handle_request(&request);
            writeln!(stdout, "{}", serde_json::to_string(&response)?)?;
            stdout.flush()?;
        }

        Ok(())
    }

    /// Handle a single JSON-RPC request
    fn handle_request(&self, request: &JsonRpcRequest) -> JsonRpcResponse {
        let id = request.id.clone().unwrap_or(Value::Null);

        match request.method.as_str() {
            "initialize" => self.handle_initialize(id),
            "initialized" => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id,
                result: Some(Value::Null),
                error: None,
            },
            "tools/list" => self.handle_tools_list(id),
            "tools/call" => self.handle_tools_call(id, request.params.as_ref()),
            _ => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id,
                result: None,
                error: Some(JsonRpcError {
                    code: -32601,
                    message: format!("Method not found: {}", request.method),
                    data: None,
                }),
            },
        }
    }

    /// Handle initialize request
    fn handle_initialize(&self, id: Value) -> JsonRpcResponse {
        JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(json!({
                "protocolVersion": MCP_VERSION,
                "capabilities": {
                    "tools": {
                        "listChanged": false
                    }
                },
                "serverInfo": {
                    "name": "did-you-actually-do-that",
                    "version": env!("CARGO_PKG_VERSION")
                }
            })),
            error: None,
        }
    }

    /// Handle tools/list request
    fn handle_tools_list(&self, id: Value) -> JsonRpcResponse {
        let tools = vec![
            ToolDef {
                name: "verify_claim".to_string(),
                description:
                    "Verify a claim against evidence. Returns a detailed verification report."
                        .to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "description": {
                            "type": "string",
                            "description": "Human-readable description of what was claimed"
                        },
                        "evidence": {
                            "type": "array",
                            "description": "Array of evidence specifications to check",
                            "items": {
                                "type": "object",
                                "properties": {
                                    "type": {
                                        "type": "string",
                                        "enum": ["FileExists", "FileWithHash", "FileContains", "FileMatchesRegex",
                                                 "FileJsonPath", "DirectoryExists", "CommandSucceeds", "GitClean",
                                                 "GitCommitExists", "GitBranchExists", "FileModifiedAfter", "EnvVar"]
                                    },
                                    "spec": {
                                        "type": "object",
                                        "description": "Evidence-specific parameters"
                                    }
                                },
                                "required": ["type", "spec"]
                            }
                        },
                        "source": {
                            "type": "string",
                            "description": "Optional source identifier (e.g., 'claude-code')"
                        }
                    },
                    "required": ["description", "evidence"]
                }),
            },
            ToolDef {
                name: "quick_verify".to_string(),
                description: "Quick check if a file or directory exists.".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "path": {
                            "type": "string",
                            "description": "Path to the file or directory to check"
                        }
                    },
                    "required": ["path"]
                }),
            },
            ToolDef {
                name: "compute_hash".to_string(),
                description: "Compute SHA-256 hash of a file for use in FileWithHash evidence."
                    .to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "path": {
                            "type": "string",
                            "description": "Path to the file to hash"
                        }
                    },
                    "required": ["path"]
                }),
            },
            ToolDef {
                name: "verify_batch".to_string(),
                description: "Verify multiple claims at once. Returns aggregated results."
                    .to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "claims": {
                            "type": "array",
                            "description": "Array of claims to verify",
                            "items": {
                                "type": "object",
                                "properties": {
                                    "description": {"type": "string"},
                                    "evidence": {"type": "array"},
                                    "source": {"type": "string"}
                                },
                                "required": ["description", "evidence"]
                            }
                        }
                    },
                    "required": ["claims"]
                }),
            },
        ];

        JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(json!({ "tools": tools })),
            error: None,
        }
    }

    /// Handle tools/call request
    fn handle_tools_call(&self, id: Value, params: Option<&Value>) -> JsonRpcResponse {
        let params = match params {
            Some(p) => p,
            None => {
                return JsonRpcResponse {
                    jsonrpc: "2.0".to_string(),
                    id,
                    result: None,
                    error: Some(JsonRpcError {
                        code: -32602,
                        message: "Missing params".to_string(),
                        data: None,
                    }),
                };
            }
        };

        let tool_name = params.get("name").and_then(|v| v.as_str()).unwrap_or("");
        let arguments = params.get("arguments").cloned().unwrap_or(json!({}));

        let result = match tool_name {
            "verify_claim" => self.tool_verify_claim(&arguments),
            "quick_verify" => self.tool_quick_verify(&arguments),
            "compute_hash" => self.tool_compute_hash(&arguments),
            "verify_batch" => self.tool_verify_batch(&arguments),
            _ => Err(format!("Unknown tool: {}", tool_name)),
        };

        match result {
            Ok(text) => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id,
                result: Some(json!({
                    "content": [{
                        "type": "text",
                        "text": text
                    }],
                    "isError": false
                })),
                error: None,
            },
            Err(e) => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id,
                result: Some(json!({
                    "content": [{
                        "type": "text",
                        "text": e
                    }],
                    "isError": true
                })),
                error: None,
            },
        }
    }

    /// Verify a single claim
    fn tool_verify_claim(&self, args: &Value) -> Result<String, String> {
        let description = args
            .get("description")
            .and_then(|v| v.as_str())
            .ok_or("Missing 'description' field")?;

        let evidence_array = args
            .get("evidence")
            .and_then(|v| v.as_array())
            .ok_or("Missing 'evidence' array")?;

        let source = args.get("source").and_then(|v| v.as_str());

        // Parse evidence specifications
        let mut evidence_specs = Vec::new();
        for ev in evidence_array {
            let spec: EvidenceSpec = serde_json::from_value(ev.clone())
                .map_err(|e| format!("Invalid evidence: {}", e))?;
            evidence_specs.push(spec);
        }

        // Build claim
        let mut claim = Claim::new(description);
        for spec in evidence_specs {
            claim = claim.with_evidence(spec);
        }
        if let Some(s) = source {
            claim = claim.with_source(s);
        }

        // Verify
        let report = self.verifier.verify(&claim);

        // Format result
        Ok(format_report(&report))
    }

    /// Quick file/directory existence check
    fn tool_quick_verify(&self, args: &Value) -> Result<String, String> {
        let path = args
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or("Missing 'path' field")?;

        let claim = Claim::new(format!("Path exists: {}", path))
            .with_evidence(EvidenceSpec::FileExists {
                path: path.to_string(),
            })
            .with_source("mcp-server");

        let report = self.verifier.verify(&claim);
        Ok(format_report(&report))
    }

    /// Compute file hash
    fn tool_compute_hash(&self, args: &Value) -> Result<String, String> {
        let path = args
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or("Missing 'path' field")?;

        let contents = std::fs::read(path).map_err(|e| format!("Cannot read file: {}", e))?;

        let mut hasher = Sha256::new();
        hasher.update(&contents);
        let hash = hex::encode(hasher.finalize());

        Ok(json!({
            "path": path,
            "sha256": hash,
            "evidence_spec": {
                "type": "FileWithHash",
                "spec": {
                    "path": path,
                    "sha256": hash
                }
            }
        })
        .to_string())
    }

    /// Verify multiple claims
    fn tool_verify_batch(&self, args: &Value) -> Result<String, String> {
        let claims_array = args
            .get("claims")
            .and_then(|v| v.as_array())
            .ok_or("Missing 'claims' array")?;

        let mut reports = Vec::new();
        let mut worst_verdict = Verdict::Confirmed;

        for claim_val in claims_array {
            let claim: Claim = serde_json::from_value(claim_val.clone())
                .map_err(|e| format!("Invalid claim: {}", e))?;

            let report = self.verifier.verify(&claim);

            worst_verdict = match (worst_verdict, report.overall_verdict) {
                (_, Verdict::Refuted) | (Verdict::Refuted, _) => Verdict::Refuted,
                (_, Verdict::Inconclusive) | (Verdict::Inconclusive, _) => Verdict::Inconclusive,
                (_, Verdict::Unverifiable) | (Verdict::Unverifiable, _) => Verdict::Unverifiable,
                (Verdict::Confirmed, Verdict::Confirmed) => Verdict::Confirmed,
            };

            reports.push(report);
        }

        let summary = json!({
            "total": reports.len(),
            "confirmed": reports.iter().filter(|r| r.overall_verdict == Verdict::Confirmed).count(),
            "refuted": reports.iter().filter(|r| r.overall_verdict == Verdict::Refuted).count(),
            "inconclusive": reports.iter().filter(|r| r.overall_verdict == Verdict::Inconclusive).count(),
            "unverifiable": reports.iter().filter(|r| r.overall_verdict == Verdict::Unverifiable).count(),
            "overall_verdict": format!("{:?}", worst_verdict),
            "reports": reports.iter().map(|r| {
                json!({
                    "claim": r.claim.description,
                    "verdict": format!("{:?}", r.overall_verdict),
                    "evidence_count": r.evidence_results.len()
                })
            }).collect::<Vec<_>>()
        });

        Ok(serde_json::to_string_pretty(&summary).unwrap())
    }
}

/// Format a verification report for human/AI consumption
fn format_report(report: &VerificationReport) -> String {
    let mut output = String::new();

    let emoji = match report.overall_verdict {
        Verdict::Confirmed => "✓",
        Verdict::Refuted => "✗",
        Verdict::Inconclusive => "?",
        Verdict::Unverifiable => "⊘",
    };

    output.push_str(&format!(
        "[{}] {} - {:?}\n",
        emoji, report.claim.description, report.overall_verdict
    ));

    if let Some(ref source) = report.claim.source {
        output.push_str(&format!("Source: {}\n", source));
    }

    for result in &report.evidence_results {
        let icon = match result.verdict {
            Verdict::Confirmed => "  ✓",
            Verdict::Refuted => "  ✗",
            Verdict::Inconclusive => "  ?",
            Verdict::Unverifiable => "  ⊘",
        };

        let evidence_desc = format_evidence(&result.spec);
        output.push_str(&format!("{} {}\n", icon, evidence_desc));

        if let Some(ref details) = result.details {
            output.push_str(&format!("      {}\n", details));
        }
    }

    output
}

/// Format evidence spec for display
fn format_evidence(spec: &EvidenceSpec) -> String {
    match spec {
        EvidenceSpec::FileExists { path } => format!("File exists: {}", path),
        EvidenceSpec::FileWithHash { path, .. } => format!("File hash: {}", path),
        EvidenceSpec::FileContains { path, substring } => {
            format!("File contains '{}': {}", substring, path)
        }
        EvidenceSpec::FileMatchesRegex { path, pattern } => {
            format!("File matches regex '{}': {}", pattern, path)
        }
        EvidenceSpec::FileJsonPath {
            path, json_path, ..
        } => {
            format!("JSON path '{}': {}", json_path, path)
        }
        EvidenceSpec::DirectoryExists { path } => format!("Directory exists: {}", path),
        EvidenceSpec::CommandSucceeds { command, .. } => format!("Command succeeds: {}", command),
        EvidenceSpec::GitClean { repo_path } => {
            format!("Git clean: {}", repo_path.as_deref().unwrap_or("."))
        }
        EvidenceSpec::GitCommitExists { commit, .. } => format!("Git commit exists: {}", commit),
        EvidenceSpec::GitBranchExists { branch, .. } => format!("Git branch exists: {}", branch),
        EvidenceSpec::FileModifiedAfter { path, after } => {
            format!("File modified after {}: {}", after, path)
        }
        EvidenceSpec::EnvVar { name, expected } => format!("Env {}={}", name, expected),
        EvidenceSpec::Custom { name, .. } => format!("Custom check: {}", name),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_handle_initialize() {
        let server = McpServer::new();
        let response = server.handle_initialize(json!(1));

        assert!(response.result.is_some());
        let result = response.result.unwrap();
        assert_eq!(result["protocolVersion"], MCP_VERSION);
        assert!(result["serverInfo"]["name"].is_string());
    }

    #[test]
    fn test_handle_tools_list() {
        let server = McpServer::new();
        let response = server.handle_tools_list(json!(1));

        assert!(response.result.is_some());
        let result = response.result.unwrap();
        let tools = result["tools"].as_array().unwrap();
        assert_eq!(tools.len(), 4);

        let tool_names: Vec<&str> = tools.iter().map(|t| t["name"].as_str().unwrap()).collect();
        assert!(tool_names.contains(&"verify_claim"));
        assert!(tool_names.contains(&"quick_verify"));
        assert!(tool_names.contains(&"compute_hash"));
        assert!(tool_names.contains(&"verify_batch"));
    }

    #[test]
    fn test_quick_verify_nonexistent() {
        let server = McpServer::new();
        let result = server.tool_quick_verify(&json!({
            "path": "/nonexistent/path/file.txt"
        }));

        assert!(result.is_ok());
        let text = result.unwrap();
        assert!(text.contains("Refuted"));
    }

    #[test]
    fn test_verify_claim() {
        let server = McpServer::new();
        let result = server.tool_verify_claim(&json!({
            "description": "Test claim",
            "evidence": [
                {
                    "type": "FileExists",
                    "spec": { "path": "/nonexistent/file.txt" }
                }
            ],
            "source": "test"
        }));

        assert!(result.is_ok());
        let text = result.unwrap();
        assert!(text.contains("Test claim"));
        assert!(text.contains("Refuted"));
    }
}
