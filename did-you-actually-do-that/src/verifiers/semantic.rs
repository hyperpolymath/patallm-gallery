// SPDX-License-Identifier: PMPL-1.0-or-later
//! Layer 4: Semantic Integrity
//!
//! Verifies that code does what claims say: file content matching, regex patterns,
//! JSON path validation, environment variables, modification timestamps, and
//! command execution.

use crate::{EvidenceResult, EvidenceSpec, Verdict};
use chrono::{DateTime, Utc};
use regex::Regex;
use std::process::Command;

/// Check semantic integrity evidence.
pub fn check(evidence: &EvidenceSpec) -> EvidenceResult {
    let (verdict, details) = match evidence {
        EvidenceSpec::FileContains { path, substring } => match std::fs::read_to_string(path) {
            Ok(contents) => {
                if contents.contains(substring) {
                    (Verdict::Confirmed, Some("Substring found".to_string()))
                } else {
                    (Verdict::Refuted, Some("Substring not found".to_string()))
                }
            }
            Err(e) => (Verdict::Refuted, Some(format!("Cannot read file: {}", e))),
        },

        EvidenceSpec::FileMatchesRegex { path, pattern } => match Regex::new(pattern) {
            Ok(re) => match std::fs::read_to_string(path) {
                Ok(contents) => {
                    if re.is_match(&contents) {
                        (Verdict::Confirmed, Some("Pattern matched".to_string()))
                    } else {
                        (Verdict::Refuted, Some("Pattern not matched".to_string()))
                    }
                }
                Err(e) => (Verdict::Refuted, Some(format!("Cannot read file: {}", e))),
            },
            Err(e) => (
                Verdict::Unverifiable,
                Some(format!("Invalid regex pattern: {}", e)),
            ),
        },

        EvidenceSpec::FileJsonPath {
            path,
            json_path,
            expected,
        } => match std::fs::read_to_string(path) {
            Ok(contents) => match serde_json::from_str::<serde_json::Value>(&contents) {
                Ok(json) => match extract_json_path(&json, json_path) {
                    Some(actual) => {
                        if actual == expected {
                            (Verdict::Confirmed, Some("JSON path matches".to_string()))
                        } else {
                            (
                                Verdict::Refuted,
                                Some(format!(
                                    "JSON path mismatch: expected {:?}, got {:?}",
                                    expected, actual
                                )),
                            )
                        }
                    }
                    None => (
                        Verdict::Refuted,
                        Some(format!("JSON path not found: {}", json_path)),
                    ),
                },
                Err(e) => (Verdict::Refuted, Some(format!("Invalid JSON: {}", e))),
            },
            Err(e) => (Verdict::Refuted, Some(format!("Cannot read file: {}", e))),
        },

        EvidenceSpec::FileModifiedAfter { path, after } => {
            match chrono::DateTime::parse_from_rfc3339(after) {
                Ok(threshold) => match std::fs::metadata(path) {
                    Ok(meta) => match meta.modified() {
                        Ok(modified) => {
                            let modified_dt: DateTime<Utc> = modified.into();
                            if modified_dt > threshold.with_timezone(&Utc) {
                                (
                                    Verdict::Confirmed,
                                    Some(format!("File modified at {}", modified_dt)),
                                )
                            } else {
                                (
                                    Verdict::Refuted,
                                    Some(format!(
                                        "File last modified {} (before {})",
                                        modified_dt, after
                                    )),
                                )
                            }
                        }
                        Err(e) => (
                            Verdict::Unverifiable,
                            Some(format!("Cannot get modification time: {}", e)),
                        ),
                    },
                    Err(e) => (Verdict::Refuted, Some(format!("Cannot stat file: {}", e))),
                },
                Err(e) => (
                    Verdict::Unverifiable,
                    Some(format!("Invalid timestamp '{}': {}", after, e)),
                ),
            }
        }

        EvidenceSpec::EnvVar { name, expected } => match std::env::var(name) {
            Ok(actual) => {
                if actual == *expected {
                    (Verdict::Confirmed, Some(format!("{}={}", name, expected)))
                } else {
                    (
                        Verdict::Refuted,
                        Some(format!("{}={} (expected {})", name, actual, expected)),
                    )
                }
            }
            Err(_) => (
                Verdict::Refuted,
                Some(format!("Environment variable {} not set", name)),
            ),
        },

        EvidenceSpec::CommandSucceeds { command, args } => {
            match Command::new(command).args(args).output() {
                Ok(output) => {
                    if output.status.success() {
                        (Verdict::Confirmed, Some("Command succeeded".to_string()))
                    } else {
                        (
                            Verdict::Refuted,
                            Some(format!(
                                "Command failed with exit code: {:?}",
                                output.status.code()
                            )),
                        )
                    }
                }
                Err(e) => (Verdict::Refuted, Some(format!("Command error: {}", e))),
            }
        }

        _ => (
            Verdict::Unverifiable,
            Some("Not a semantic integrity check".to_string()),
        ),
    };

    EvidenceResult {
        spec: evidence.clone(),
        verdict,
        details,
    }
}

/// Extract a value from JSON using a simple path notation.
/// Supports paths like ".field", ".nested.field", "[0]", ".array[0].field"
pub fn extract_json_path<'a>(
    json: &'a serde_json::Value,
    path: &str,
) -> Option<&'a serde_json::Value> {
    let mut current = json;

    for segment in path.split('.').filter(|s| !s.is_empty()) {
        if let Some(bracket_pos) = segment.find('[') {
            let field_name = &segment[..bracket_pos];
            if !field_name.is_empty() {
                current = current.get(field_name)?;
            }

            let end_bracket = segment.find(']')?;
            let index_str = &segment[bracket_pos + 1..end_bracket];
            let index: usize = index_str.parse().ok()?;
            current = current.get(index)?;
        } else {
            current = current.get(segment)?;
        }
    }

    Some(current)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_path_simple() {
        let json: serde_json::Value = serde_json::json!({"name": "test", "value": 42});
        assert_eq!(
            extract_json_path(&json, ".name"),
            Some(&serde_json::json!("test"))
        );
    }

    #[test]
    fn test_json_path_nested() {
        let json: serde_json::Value =
            serde_json::json!({"outer": {"inner": "deep"}});
        assert_eq!(
            extract_json_path(&json, ".outer.inner"),
            Some(&serde_json::json!("deep"))
        );
    }

    #[test]
    fn test_json_path_array() {
        let json: serde_json::Value =
            serde_json::json!({"items": [1, 2, 3]});
        assert_eq!(
            extract_json_path(&json, ".items[1]"),
            Some(&serde_json::json!(2))
        );
    }

    #[test]
    fn test_command_succeeds() {
        let evidence = EvidenceSpec::CommandSucceeds {
            command: "true".to_string(),
            args: vec![],
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Confirmed);
    }

    #[test]
    fn test_command_fails() {
        let evidence = EvidenceSpec::CommandSucceeds {
            command: "false".to_string(),
            args: vec![],
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Refuted);
    }
}
