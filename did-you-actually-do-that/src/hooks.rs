// SPDX-License-Identifier: MPL-2.0
//! Claude Code hook integration
//!
//! This module provides hooks for integrating with Claude Code's hook system.
//! Hooks allow automatic verification of AI claims after tool executions.
//!
//! ## Setup
//!
//! Add to your `~/.claude/settings.json`:
//!
//! ```json
//! {
//!   "hooks": {
//!     "PostToolExecution": [
//!       {
//!         "command": "dyadt",
//!         "args": ["hook", "--tool", "$TOOL_NAME", "--result", "$TOOL_RESULT"],
//!         "timeout": 5000
//!       }
//!     ]
//!   }
//! }
//! ```
//!
//! Or use the MCP server for direct integration.
//!
//! ## Hook Types
//!
//! - `PostToolExecution` - Verify file/directory changes after Write/Edit/Bash tools
//! - `PreCommit` - Verify all claimed changes before git commit
//! - `SessionEnd` - Generate verification report at end of session

use crate::claim_extractor::{ClaimExtractor, ExtractionContext, ToolCall};
use crate::{Verdict, VerificationReport, Verifier};
use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Hook event types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum HookEvent {
    /// After a tool is executed
    PostToolExecution,
    /// Before a git commit
    PreCommit,
    /// At the end of a session
    SessionEnd,
}

/// Input for a hook invocation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HookInput {
    /// The event that triggered the hook
    pub event: HookEvent,
    /// Name of the tool that was executed (for PostToolExecution)
    pub tool_name: Option<String>,
    /// Arguments passed to the tool
    pub tool_args: Option<HashMap<String, serde_json::Value>>,
    /// Result/output of the tool
    pub tool_result: Option<String>,
    /// Working directory
    pub working_directory: Option<String>,
    /// Session ID for tracking
    pub session_id: Option<String>,
}

/// Output from a hook invocation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HookOutput {
    /// Whether verification passed
    pub success: bool,
    /// Overall verdict
    pub verdict: String,
    /// Number of claims verified
    pub claims_verified: usize,
    /// Number of claims confirmed
    pub claims_confirmed: usize,
    /// Number of claims refuted
    pub claims_refuted: usize,
    /// Detailed reports (if requested)
    pub reports: Option<Vec<VerificationReport>>,
    /// Human-readable summary
    pub summary: String,
}

/// Hook handler for Claude Code integration
pub struct HookHandler {
    extractor: ClaimExtractor,
    verifier: Verifier,
    session_start: String,
}

impl Default for HookHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl HookHandler {
    /// Create a new hook handler
    pub fn new() -> Self {
        Self {
            extractor: ClaimExtractor::new(),
            verifier: Verifier::new(),
            session_start: Utc::now().to_rfc3339(),
        }
    }

    /// Handle a hook event
    pub fn handle(&self, input: &HookInput) -> HookOutput {
        match input.event {
            HookEvent::PostToolExecution => self.handle_post_tool_execution(input),
            HookEvent::PreCommit => self.handle_pre_commit(input),
            HookEvent::SessionEnd => self.handle_session_end(input),
        }
    }

    /// Handle post tool execution event
    fn handle_post_tool_execution(&self, input: &HookInput) -> HookOutput {
        let tool_name = input.tool_name.as_deref().unwrap_or("unknown");

        // Only verify for tools that make changes
        if !matches!(tool_name, "Write" | "Edit" | "Bash" | "NotebookEdit") {
            return HookOutput {
                success: true,
                verdict: "Skipped".to_string(),
                claims_verified: 0,
                claims_confirmed: 0,
                claims_refuted: 0,
                reports: None,
                summary: format!("Tool '{}' does not require verification", tool_name),
            };
        }

        let context = ExtractionContext {
            source: "claude-code-hook".to_string(),
            working_directory: input.working_directory.clone(),
            conversation_id: input.session_id.clone(),
            session_start: Some(self.session_start.clone()),
        };

        // Build tool call from input
        let mut tool_call = ToolCall::new(tool_name);
        if let Some(ref args) = input.tool_args {
            for (key, value) in args {
                tool_call = tool_call.with_arg(key, value.clone());
            }
        }

        // Extract claims from tool call
        let claims = self
            .extractor
            .extract_from_tool_calls(&[tool_call], &context);

        if claims.is_empty() {
            return HookOutput {
                success: true,
                verdict: "NoClaimsToVerify".to_string(),
                claims_verified: 0,
                claims_confirmed: 0,
                claims_refuted: 0,
                reports: None,
                summary: "No verifiable claims extracted from tool execution".to_string(),
            };
        }

        // Verify all claims
        let reports: Vec<VerificationReport> = claims
            .iter()
            .map(|claim| self.verifier.verify(claim))
            .collect();

        let confirmed = reports
            .iter()
            .filter(|r| r.overall_verdict == Verdict::Confirmed)
            .count();
        let refuted = reports
            .iter()
            .filter(|r| r.overall_verdict == Verdict::Refuted)
            .count();

        let overall_verdict = if refuted > 0 {
            Verdict::Refuted
        } else if confirmed == reports.len() {
            Verdict::Confirmed
        } else {
            Verdict::Inconclusive
        };

        HookOutput {
            success: refuted == 0,
            verdict: format!("{:?}", overall_verdict),
            claims_verified: reports.len(),
            claims_confirmed: confirmed,
            claims_refuted: refuted,
            reports: Some(reports),
            summary: format!(
                "Verified {} claims: {} confirmed, {} refuted",
                claims.len(),
                confirmed,
                refuted
            ),
        }
    }

    /// Handle pre-commit event
    fn handle_pre_commit(&self, input: &HookInput) -> HookOutput {
        let context = ExtractionContext {
            source: "claude-code-hook".to_string(),
            working_directory: input.working_directory.clone(),
            conversation_id: input.session_id.clone(),
            session_start: Some(self.session_start.clone()),
        };

        // If tool result contains text describing changes, extract from it
        let claims = if let Some(ref result_text) = input.tool_result {
            self.extractor.extract_from_text(result_text, &context)
        } else {
            Vec::new()
        };

        if claims.is_empty() {
            return HookOutput {
                success: true,
                verdict: "NoClaimsToVerify".to_string(),
                claims_verified: 0,
                claims_confirmed: 0,
                claims_refuted: 0,
                reports: None,
                summary: "No claims to verify before commit".to_string(),
            };
        }

        let reports: Vec<VerificationReport> = claims
            .iter()
            .map(|claim| self.verifier.verify(claim))
            .collect();

        let confirmed = reports
            .iter()
            .filter(|r| r.overall_verdict == Verdict::Confirmed)
            .count();
        let refuted = reports
            .iter()
            .filter(|r| r.overall_verdict == Verdict::Refuted)
            .count();

        let total = reports.len();
        let all_confirmed = refuted == 0 && confirmed == total;

        HookOutput {
            success: all_confirmed,
            verdict: if all_confirmed {
                "Confirmed".to_string()
            } else if refuted > 0 {
                "Refuted".to_string()
            } else {
                "Inconclusive".to_string()
            },
            claims_verified: total,
            claims_confirmed: confirmed,
            claims_refuted: refuted,
            reports: Some(reports),
            summary: if all_confirmed {
                format!("All {} claims verified - safe to commit", total)
            } else {
                format!(
                    "Pre-commit check: {} confirmed, {} refuted - review before committing",
                    confirmed, refuted
                )
            },
        }
    }

    /// Handle session end event
    fn handle_session_end(&self, input: &HookInput) -> HookOutput {
        // At session end, we could aggregate all verification results
        // For now, just provide a summary
        let context = ExtractionContext {
            source: "claude-code-hook".to_string(),
            working_directory: input.working_directory.clone(),
            conversation_id: input.session_id.clone(),
            session_start: Some(self.session_start.clone()),
        };

        // Extract claims from any summary text
        let claims = if let Some(ref result_text) = input.tool_result {
            self.extractor.extract_from_text(result_text, &context)
        } else {
            Vec::new()
        };

        if claims.is_empty() {
            return HookOutput {
                success: true,
                verdict: "NoClaimsToVerify".to_string(),
                claims_verified: 0,
                claims_confirmed: 0,
                claims_refuted: 0,
                reports: None,
                summary: "Session ended with no claims to verify".to_string(),
            };
        }

        let reports: Vec<VerificationReport> = claims
            .iter()
            .map(|claim| self.verifier.verify(claim))
            .collect();

        let total = reports.len();
        let confirmed = reports
            .iter()
            .filter(|r| r.overall_verdict == Verdict::Confirmed)
            .count();
        let refuted = reports
            .iter()
            .filter(|r| r.overall_verdict == Verdict::Refuted)
            .count();

        HookOutput {
            success: refuted == 0,
            verdict: if refuted > 0 {
                "Refuted".to_string()
            } else if confirmed == total {
                "Confirmed".to_string()
            } else {
                "Inconclusive".to_string()
            },
            claims_verified: total,
            claims_confirmed: confirmed,
            claims_refuted: refuted,
            reports: Some(reports),
            summary: format!(
                "Session verification: {} claims, {} confirmed, {} refuted",
                total, confirmed, refuted
            ),
        }
    }
}

/// Parse hook input from JSON string
pub fn parse_hook_input(json_str: &str) -> Result<HookInput, serde_json::Error> {
    serde_json::from_str(json_str)
}

/// Format hook output as JSON string
pub fn format_hook_output(output: &HookOutput) -> String {
    serde_json::to_string_pretty(output).unwrap_or_else(|_| "{}".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hook_handler_creation() {
        let handler = HookHandler::new();
        assert!(!handler.session_start.is_empty());
    }

    #[test]
    fn test_skip_non_modifying_tools() {
        let handler = HookHandler::new();
        let input = HookInput {
            event: HookEvent::PostToolExecution,
            tool_name: Some("Read".to_string()),
            tool_args: None,
            tool_result: None,
            working_directory: None,
            session_id: None,
        };

        let output = handler.handle(&input);
        assert!(output.success);
        assert_eq!(output.verdict, "Skipped");
    }

    #[test]
    fn test_write_tool_verification() {
        let handler = HookHandler::new();
        let mut args = HashMap::new();
        args.insert(
            "file_path".to_string(),
            serde_json::json!("/nonexistent/path/test.txt"),
        );

        let input = HookInput {
            event: HookEvent::PostToolExecution,
            tool_name: Some("Write".to_string()),
            tool_args: Some(args),
            tool_result: None,
            working_directory: None,
            session_id: None,
        };

        let output = handler.handle(&input);
        // File doesn't exist, so should be refuted
        assert!(!output.success);
        assert!(output.claims_refuted > 0);
    }

    #[test]
    fn test_parse_hook_input() {
        let json = r#"{
            "event": "PostToolExecution",
            "tool_name": "Write",
            "tool_args": {"file_path": "/tmp/test.txt"},
            "working_directory": "/home/user"
        }"#;

        let input = parse_hook_input(json).unwrap();
        assert_eq!(input.event, HookEvent::PostToolExecution);
        assert_eq!(input.tool_name, Some("Write".to_string()));
    }
}
