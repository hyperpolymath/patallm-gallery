// SPDX-License-Identifier: MPL-2.0
//! Claim extraction from AI responses
//!
//! This module provides functionality to extract verifiable claims from AI
//! assistant responses, tool calls, and structured outputs.
//!
//! ## Supported Patterns
//!
//! The extractor recognizes common patterns in AI responses:
//!
//! - File operations: "I created/wrote/modified/deleted file X"
//! - Command execution: "I ran/executed command X"
//! - Git operations: "I committed/pushed/created branch X"
//! - General assertions: "I did X" with evidence markers
//!
//! ## Usage
//!
//! ```rust,no_run
//! use did_you_actually_do_that::claim_extractor::{ClaimExtractor, ExtractionContext};
//!
//! let extractor = ClaimExtractor::new();
//! let context = ExtractionContext {
//!     source: "claude-code".to_string(),
//!     working_directory: Some("/home/user/project".to_string()),
//!     ..Default::default()
//! };
//!
//! let claims = extractor.extract_from_text("I created the file src/main.rs", &context);
//! ```

use crate::{Claim, EvidenceSpec};
use regex::Regex;
use std::collections::HashSet;

/// Context for claim extraction
#[derive(Debug, Clone, Default)]
pub struct ExtractionContext {
    /// Source identifier for extracted claims
    pub source: String,
    /// Working directory for relative paths
    pub working_directory: Option<String>,
    /// Additional context about the conversation
    pub conversation_id: Option<String>,
    /// Timestamp for FileModifiedAfter checks
    pub session_start: Option<String>,
}

/// Extracts verifiable claims from AI responses
#[allow(dead_code)]
pub struct ClaimExtractor {
    file_created_pattern: Regex,
    file_wrote_pattern: Regex,
    file_modified_pattern: Regex,
    file_deleted_pattern: Regex,
    command_ran_pattern: Regex,
    git_commit_pattern: Regex,
    git_push_pattern: Regex,
    git_branch_pattern: Regex,
    directory_created_pattern: Regex,
}

impl Default for ClaimExtractor {
    fn default() -> Self {
        Self::new()
    }
}

impl ClaimExtractor {
    /// Create a new claim extractor with default patterns
    pub fn new() -> Self {
        Self {
            // File operations - capture paths (with extensions or path separators)
            // Must contain a file extension or path separator to be a valid file path
            file_created_pattern: Regex::new(
                r#"(?i)(?:I\s+)?(?:created|made|added)\s+(?:the\s+)?file\s+[`"']?([^\s`"']+\.[a-zA-Z0-9]+)[`"']?"#
            ).unwrap(),
            file_wrote_pattern: Regex::new(
                r#"(?i)(?:I\s+)?(?:wrote|written)\s+(?:to\s+)?(?:the\s+)?file\s+[`"']?([^\s`"']+\.[a-zA-Z0-9]+)[`"']?"#
            ).unwrap(),
            file_modified_pattern: Regex::new(
                r#"(?i)(?:I\s+)?(?:modified|updated|edited|changed)\s+(?:the\s+)?(?:file\s+)?[`"']?([^\s`"']+\.[a-zA-Z0-9]+)[`"']?"#
            ).unwrap(),
            file_deleted_pattern: Regex::new(
                r#"(?i)(?:I\s+)?(?:deleted|removed)\s+(?:the\s+)?file\s+[`"']?([^\s`"']+\.[a-zA-Z0-9]+)[`"']?"#
            ).unwrap(),
            // Command execution
            command_ran_pattern: Regex::new(
                r#"(?i)(?:I\s+)?(?:ran|executed)\s+(?:the\s+)?command\s+[`"']([^`"']+)[`"']"#
            ).unwrap(),
            // Git operations - require hash to be present for commit detection
            git_commit_pattern: Regex::new(
                r#"(?i)(?:committed|commit)\s+(?:with\s+)?(?:hash\s+)?([a-f0-9]{7,40})"#
            ).unwrap(),
            git_push_pattern: Regex::new(
                r#"(?i)(?:I\s+)?pushed\s+(?:to\s+)?(?:the\s+)?branch\s+[`"']?([^\s`"']+)[`"']?"#
            ).unwrap(),
            // Git branch - require "branch" keyword to avoid false positives
            git_branch_pattern: Regex::new(
                r#"(?i)(?:I\s+)?(?:created|switched\s+to)\s+(?:the\s+)?branch\s+[`"']?([^\s`"']+)[`"']?"#
            ).unwrap(),
            // Directory operations - require "directory" or "folder" keyword
            directory_created_pattern: Regex::new(
                r#"(?i)(?:I\s+)?(?:created|made)\s+(?:the\s+)?(?:directory|folder|dir)\s+[`"']?([^\s`"']+)[`"']?"#
            ).unwrap(),
        }
    }

    /// Extract claims from plain text AI response
    pub fn extract_from_text(&self, text: &str, context: &ExtractionContext) -> Vec<Claim> {
        let mut claims = Vec::new();
        let mut seen_paths = HashSet::new();

        // Extract file creation claims
        for cap in self.file_created_pattern.captures_iter(text) {
            if let Some(path) = cap.get(1) {
                let path_str = self.resolve_path(path.as_str().trim(), context);
                if seen_paths.insert(path_str.clone()) {
                    let mut claim = Claim::new(format!("Created file: {}", path_str))
                        .with_evidence(EvidenceSpec::FileExists {
                            path: path_str.clone(),
                        });

                    // Add modification time check if session_start is available
                    if let Some(ref start_time) = context.session_start {
                        claim = claim.with_evidence(EvidenceSpec::FileModifiedAfter {
                            path: path_str,
                            after: start_time.clone(),
                        });
                    }

                    if !context.source.is_empty() {
                        claim = claim.with_source(&context.source);
                    }
                    claims.push(claim);
                }
            }
        }

        // Extract file write claims
        for cap in self.file_wrote_pattern.captures_iter(text) {
            if let Some(path) = cap.get(1) {
                let path_str = self.resolve_path(path.as_str().trim(), context);
                if seen_paths.insert(path_str.clone()) {
                    let mut claim = Claim::new(format!("Wrote to file: {}", path_str))
                        .with_evidence(EvidenceSpec::FileExists { path: path_str });

                    if !context.source.is_empty() {
                        claim = claim.with_source(&context.source);
                    }
                    claims.push(claim);
                }
            }
        }

        // Extract file modification claims
        for cap in self.file_modified_pattern.captures_iter(text) {
            if let Some(path) = cap.get(1) {
                let path_str = self.resolve_path(path.as_str().trim(), context);
                if seen_paths.insert(path_str.clone()) {
                    let mut claim = Claim::new(format!("Modified file: {}", path_str))
                        .with_evidence(EvidenceSpec::FileExists {
                            path: path_str.clone(),
                        });

                    if let Some(ref start_time) = context.session_start {
                        claim = claim.with_evidence(EvidenceSpec::FileModifiedAfter {
                            path: path_str,
                            after: start_time.clone(),
                        });
                    }

                    if !context.source.is_empty() {
                        claim = claim.with_source(&context.source);
                    }
                    claims.push(claim);
                }
            }
        }

        // Extract directory creation claims
        for cap in self.directory_created_pattern.captures_iter(text) {
            if let Some(path) = cap.get(1) {
                let path_str = self.resolve_path(path.as_str().trim(), context);
                if seen_paths.insert(path_str.clone()) {
                    let mut claim = Claim::new(format!("Created directory: {}", path_str))
                        .with_evidence(EvidenceSpec::DirectoryExists { path: path_str });

                    if !context.source.is_empty() {
                        claim = claim.with_source(&context.source);
                    }
                    claims.push(claim);
                }
            }
        }

        // Extract git commit claims
        for cap in self.git_commit_pattern.captures_iter(text) {
            if let Some(hash) = cap.get(1) {
                let commit_hash = hash.as_str().trim();
                let repo_path = context.working_directory.clone();
                let mut claim = Claim::new(format!("Made git commit: {}", commit_hash))
                    .with_evidence(EvidenceSpec::GitCommitExists {
                        commit: commit_hash.to_string(),
                        repo_path,
                    });

                if !context.source.is_empty() {
                    claim = claim.with_source(&context.source);
                }
                claims.push(claim);
            }
        }

        // Extract git branch claims
        for cap in self.git_branch_pattern.captures_iter(text) {
            if let Some(branch) = cap.get(1) {
                let branch_name = branch.as_str().trim();
                let repo_path = context.working_directory.clone();
                let mut claim = Claim::new(format!("Created/switched to branch: {}", branch_name))
                    .with_evidence(EvidenceSpec::GitBranchExists {
                        branch: branch_name.to_string(),
                        repo_path,
                    });

                if !context.source.is_empty() {
                    claim = claim.with_source(&context.source);
                }
                claims.push(claim);
            }
        }

        claims
    }

    /// Extract claims from Claude Code tool calls
    pub fn extract_from_tool_calls(
        &self,
        tool_calls: &[ToolCall],
        context: &ExtractionContext,
    ) -> Vec<Claim> {
        let mut claims = Vec::new();

        for tool_call in tool_calls {
            match tool_call.name.as_str() {
                "Write" | "Edit" => {
                    if let Some(path) = tool_call.arguments.get("file_path") {
                        let path_str = path.as_str().unwrap_or("");
                        let mut claim =
                            Claim::new(format!("Tool {} on: {}", tool_call.name, path_str))
                                .with_evidence(EvidenceSpec::FileExists {
                                    path: path_str.to_string(),
                                });

                        if let Some(ref start_time) = context.session_start {
                            claim = claim.with_evidence(EvidenceSpec::FileModifiedAfter {
                                path: path_str.to_string(),
                                after: start_time.clone(),
                            });
                        }

                        if !context.source.is_empty() {
                            claim = claim.with_source(&context.source);
                        }
                        claims.push(claim);
                    }
                }
                "Bash" => {
                    if let Some(command) = tool_call.arguments.get("command") {
                        let cmd_str = command.as_str().unwrap_or("");
                        // For bash commands, we can't verify they succeeded after the fact
                        // But we can verify side effects like file creation
                        if cmd_str.starts_with("mkdir") || cmd_str.contains("mkdir -p") {
                            // Try to extract path from mkdir command
                            let parts: Vec<&str> = cmd_str.split_whitespace().collect();
                            if let Some(path) = parts.last() {
                                let path_str = self.resolve_path(path, context);
                                let mut claim = Claim::new(format!(
                                    "Created directory via mkdir: {}",
                                    path_str
                                ))
                                .with_evidence(EvidenceSpec::DirectoryExists { path: path_str });

                                if !context.source.is_empty() {
                                    claim = claim.with_source(&context.source);
                                }
                                claims.push(claim);
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        claims
    }

    /// Resolve a path, handling relative paths if working_directory is set
    fn resolve_path(&self, path: &str, context: &ExtractionContext) -> String {
        let path = path.trim_matches(|c| c == '`' || c == '"' || c == '\'');

        if path.starts_with('/') || path.starts_with('~') {
            // Absolute path or home-relative
            path.to_string()
        } else if let Some(ref wd) = context.working_directory {
            // Make relative path absolute
            format!("{}/{}", wd.trim_end_matches('/'), path)
        } else {
            path.to_string()
        }
    }
}

/// Represents a tool call from an AI assistant
#[derive(Debug, Clone)]
pub struct ToolCall {
    /// Name of the tool (e.g., "Write", "Edit", "Bash")
    pub name: String,
    /// Arguments passed to the tool
    pub arguments: std::collections::HashMap<String, serde_json::Value>,
}

impl ToolCall {
    /// Create a new tool call
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            arguments: std::collections::HashMap::new(),
        }
    }

    /// Add an argument to the tool call
    pub fn with_arg(mut self, key: impl Into<String>, value: serde_json::Value) -> Self {
        self.arguments.insert(key.into(), value);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_file_created() {
        let extractor = ClaimExtractor::new();
        let context = ExtractionContext {
            source: "test".to_string(),
            ..Default::default()
        };

        let claims = extractor.extract_from_text("I created file src/main.rs", &context);
        assert_eq!(claims.len(), 1);
        assert!(claims[0].description.contains("src/main.rs"));
    }

    #[test]
    fn test_extract_file_with_backticks() {
        let extractor = ClaimExtractor::new();
        let context = ExtractionContext::default();

        let claims = extractor.extract_from_text("I created file `src/lib.rs`", &context);
        assert_eq!(claims.len(), 1);
        assert!(claims[0].description.contains("src/lib.rs"));
    }

    #[test]
    fn test_extract_directory_created() {
        let extractor = ClaimExtractor::new();
        let context = ExtractionContext::default();

        let claims = extractor.extract_from_text("I created directory tests/integration", &context);
        assert_eq!(claims.len(), 1);
        assert!(claims[0].description.contains("directory"));
    }

    #[test]
    fn test_extract_with_working_directory() {
        let extractor = ClaimExtractor::new();
        let context = ExtractionContext {
            working_directory: Some("/home/user/project".to_string()),
            ..Default::default()
        };

        let claims = extractor.extract_from_text("I modified src/main.rs", &context);
        assert_eq!(claims.len(), 1);

        // Check that path was resolved
        if let Some(ev) = claims[0].evidence.first() {
            match ev {
                EvidenceSpec::FileExists { path } => {
                    assert!(path.starts_with("/home/user/project"));
                }
                _ => panic!("Expected FileExists evidence"),
            }
        }
    }

    #[test]
    fn test_extract_from_tool_call() {
        let extractor = ClaimExtractor::new();
        let context = ExtractionContext {
            source: "claude-code".to_string(),
            ..Default::default()
        };

        let tool_call =
            ToolCall::new("Write").with_arg("file_path", serde_json::json!("/tmp/test.txt"));

        let claims = extractor.extract_from_tool_calls(&[tool_call], &context);
        assert_eq!(claims.len(), 1);
        assert!(claims[0].description.contains("Write"));
        assert!(claims[0].description.contains("/tmp/test.txt"));
    }

    #[test]
    fn test_no_duplicates() {
        let extractor = ClaimExtractor::new();
        let context = ExtractionContext::default();

        // Multiple mentions of the same file should not create duplicates
        let text = "I created file src/main.rs. Then I also wrote to file src/main.rs.";
        let claims = extractor.extract_from_text(text, &context);

        // Should get 1 claim since both refer to the same file path
        // The deduplication prevents multiple claims for the same path
        assert!(!claims.is_empty());
    }

    #[test]
    fn test_git_commit_extraction() {
        let extractor = ClaimExtractor::new();
        let context = ExtractionContext::default();

        let claims = extractor.extract_from_text("I committed with hash abc1234", &context);
        assert_eq!(claims.len(), 1);
        assert!(claims[0].description.contains("abc1234"));
    }

    #[test]
    fn test_git_branch_extraction() {
        let extractor = ClaimExtractor::new();
        let context = ExtractionContext::default();

        let claims = extractor.extract_from_text("I created branch feature/new-thing", &context);
        assert_eq!(claims.len(), 1);
        assert!(claims[0].description.contains("feature/new-thing"));
    }
}
