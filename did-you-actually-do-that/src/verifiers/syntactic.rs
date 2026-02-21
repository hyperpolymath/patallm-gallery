// SPDX-License-Identifier: PMPL-1.0-or-later
//! Layer 3: Syntactic Validity
//!
//! Checks that created/modified files parse without errors. Currently supports:
//! - JSON files (via serde_json)
//! - TOML files (via toml crate)
//! - Basic text readability
//!
//! Future: tree-sitter integration for language-specific parsing.

use crate::{EvidenceResult, EvidenceSpec, Verdict};
use std::path::Path;

/// Check syntactic validity of a file.
///
/// Dispatches to format-specific parsers based on file extension.
pub fn check(evidence: &EvidenceSpec) -> EvidenceResult {
    let (verdict, details) = match evidence {
        EvidenceSpec::FileExists { path } => check_syntax(path),
        _ => (
            Verdict::Unverifiable,
            Some("Syntactic check not applicable to this evidence type".to_string()),
        ),
    };

    EvidenceResult {
        spec: evidence.clone(),
        verdict,
        details,
    }
}

fn check_syntax(path: &str) -> (Verdict, Option<String>) {
    let p = Path::new(path);
    if !p.exists() {
        return (
            Verdict::Refuted,
            Some(format!("File not found: {}", path)),
        );
    }

    let ext = p
        .extension()
        .map(|e| e.to_string_lossy().to_lowercase())
        .unwrap_or_default();

    match ext.as_str() {
        "json" => validate_json(path),
        "toml" => validate_toml(path),
        "rs" | "ex" | "exs" | "jl" | "chpl" | "oz" | "ml" | "mli" | "hs" | "rb" | "py"
        | "js" | "mjs" | "res" | "resi" | "ncl" | "scm" | "lisp" | "el" => {
            validate_text_readable(path, &ext)
        }
        "md" | "adoc" | "txt" | "csv" | "yml" | "yaml" => validate_text_readable(path, &ext),
        _ => validate_text_readable(path, &ext),
    }
}

/// Validate JSON syntax.
fn validate_json(path: &str) -> (Verdict, Option<String>) {
    match std::fs::read_to_string(path) {
        Ok(contents) => {
            if contents.trim().is_empty() {
                return (
                    Verdict::Inconclusive,
                    Some("File is empty".to_string()),
                );
            }
            match serde_json::from_str::<serde_json::Value>(&contents) {
                Ok(_) => (
                    Verdict::Confirmed,
                    Some("Valid JSON syntax".to_string()),
                ),
                Err(e) => (
                    Verdict::Refuted,
                    Some(format!("JSON syntax error at line {}: {}", e.line(), e)),
                ),
            }
        }
        Err(e) => (
            Verdict::Refuted,
            Some(format!("Cannot read file: {}", e)),
        ),
    }
}

/// Validate TOML syntax.
fn validate_toml(path: &str) -> (Verdict, Option<String>) {
    match std::fs::read_to_string(path) {
        Ok(contents) => {
            if contents.trim().is_empty() {
                return (
                    Verdict::Inconclusive,
                    Some("File is empty".to_string()),
                );
            }
            match contents.parse::<toml::Table>() {
                Ok(_) => (
                    Verdict::Confirmed,
                    Some("Valid TOML syntax".to_string()),
                ),
                Err(e) => (
                    Verdict::Refuted,
                    Some(format!("TOML syntax error: {}", e)),
                ),
            }
        }
        Err(e) => (
            Verdict::Refuted,
            Some(format!("Cannot read file: {}", e)),
        ),
    }
}

/// Basic text readability check for source code files.
/// Confirms the file is valid UTF-8 and non-empty.
fn validate_text_readable(path: &str, ext: &str) -> (Verdict, Option<String>) {
    match std::fs::read_to_string(path) {
        Ok(contents) => {
            if contents.is_empty() {
                (
                    Verdict::Inconclusive,
                    Some("File is empty — cannot validate syntax".to_string()),
                )
            } else {
                (
                    Verdict::Confirmed,
                    Some(format!(
                        "File is valid UTF-8 ({} lines, .{} file)",
                        contents.lines().count(),
                        ext
                    )),
                )
            }
        }
        Err(e) => {
            if e.kind() == std::io::ErrorKind::InvalidData {
                (
                    Verdict::Inconclusive,
                    Some("Binary file — syntax check not applicable".to_string()),
                )
            } else {
                (
                    Verdict::Refuted,
                    Some(format!("Cannot read file: {}", e)),
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_valid_json() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("test.json");
        fs::write(&path, r#"{"key": "value", "num": 42}"#).unwrap();

        let evidence = EvidenceSpec::FileExists {
            path: path.to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Confirmed);
    }

    #[test]
    fn test_invalid_json() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("bad.json");
        fs::write(&path, r#"{"key": value}"#).unwrap();

        let evidence = EvidenceSpec::FileExists {
            path: path.to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Refuted);
        assert!(result.details.unwrap().contains("JSON syntax error"));
    }

    #[test]
    fn test_valid_toml() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("test.toml");
        fs::write(&path, "[package]\nname = \"test\"\n").unwrap();

        let evidence = EvidenceSpec::FileExists {
            path: path.to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Confirmed);
    }

    #[test]
    fn test_invalid_toml() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("bad.toml");
        fs::write(&path, "[package\nname = broken").unwrap();

        let evidence = EvidenceSpec::FileExists {
            path: path.to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Refuted);
        assert!(result.details.unwrap().contains("TOML syntax error"));
    }

    #[test]
    fn test_source_file_readable() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("main.rs");
        fs::write(&path, "fn main() {\n    println!(\"hello\");\n}\n").unwrap();

        let evidence = EvidenceSpec::FileExists {
            path: path.to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Confirmed);
        assert!(result.details.unwrap().contains("3 lines"));
    }

    #[test]
    fn test_empty_file() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("empty.json");
        fs::write(&path, "").unwrap();

        let evidence = EvidenceSpec::FileExists {
            path: path.to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Inconclusive);
    }
}
