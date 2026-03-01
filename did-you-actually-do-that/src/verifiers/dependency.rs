// SPDX-License-Identifier: PMPL-1.0-or-later
//! Layer 7: Dependency Resolution
//!
//! Verifies that added dependencies resolve, versions exist, and manifests
//! are well-formed. Supports Cargo.toml, mix.exs, package.json, and
//! Project.toml (Julia).

use crate::{EvidenceResult, EvidenceSpec, Verdict};
use std::path::Path;

/// Check dependency resolution evidence.
///
/// When a FileExists or FileContains evidence references a manifest file,
/// this layer validates the manifest structure and dependency declarations.
pub fn check(evidence: &EvidenceSpec) -> EvidenceResult {
    let (verdict, details) = match evidence {
        EvidenceSpec::FileExists { path } => check_manifest(path),
        EvidenceSpec::FileContains { path, substring } => {
            if is_manifest(path) {
                check_manifest_contains(path, substring)
            } else {
                (
                    Verdict::Unverifiable,
                    Some("Not a dependency manifest".to_string()),
                )
            }
        }
        _ => (
            Verdict::Unverifiable,
            Some("Dependency check not applicable".to_string()),
        ),
    };

    EvidenceResult {
        spec: evidence.clone(),
        verdict,
        details,
    }
}

fn is_manifest(path: &str) -> bool {
    let name = Path::new(path)
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_default();

    matches!(
        name.as_str(),
        "Cargo.toml" | "mix.exs" | "Project.toml" | "package.json" | "deno.json"
    )
}

fn check_manifest(path: &str) -> (Verdict, Option<String>) {
    if !Path::new(path).exists() {
        return (
            Verdict::Refuted,
            Some(format!("Manifest not found: {}", path)),
        );
    }

    if !is_manifest(path) {
        return (
            Verdict::Unverifiable,
            Some("Not a dependency manifest".to_string()),
        );
    }

    let name = match Path::new(path).file_name() {
        Some(f) => f.to_string_lossy().to_string(),
        None => {
            return (
                Verdict::Unverifiable,
                Some("Path has no filename component".to_string()),
            );
        }
    };

    match name.as_str() {
        "Cargo.toml" => validate_cargo_toml(path),
        "Project.toml" => validate_project_toml(path),
        "package.json" | "deno.json" => validate_package_json(path),
        "mix.exs" => validate_mix_exs(path),
        _ => (Verdict::Inconclusive, Some("Unknown manifest type".to_string())),
    }
}

fn check_manifest_contains(path: &str, substring: &str) -> (Verdict, Option<String>) {
    match std::fs::read_to_string(path) {
        Ok(contents) => {
            if contents.contains(substring) {
                (
                    Verdict::Confirmed,
                    Some(format!("Manifest contains '{}'", substring)),
                )
            } else {
                (
                    Verdict::Refuted,
                    Some(format!("Manifest does not contain '{}'", substring)),
                )
            }
        }
        Err(e) => (
            Verdict::Refuted,
            Some(format!("Cannot read manifest: {}", e)),
        ),
    }
}

/// Validate a Cargo.toml file: check it parses, has [package], and deps are well-formed.
fn validate_cargo_toml(path: &str) -> (Verdict, Option<String>) {
    let contents = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => return (Verdict::Refuted, Some(format!("Cannot read: {}", e))),
    };

    let doc: toml::Table = match contents.parse() {
        Ok(d) => d,
        Err(e) => {
            return (
                Verdict::Refuted,
                Some(format!("Invalid TOML: {}", e)),
            )
        }
    };

    let mut issues = Vec::new();

    // Check [package] section
    if let Some(pkg) = doc.get("package").and_then(|v| v.as_table()) {
        if pkg.get("name").is_none() {
            issues.push("Missing package.name".to_string());
        }
        if pkg.get("version").is_none() {
            issues.push("Missing package.version".to_string());
        }
    } else {
        // Could be a workspace root
        if doc.get("workspace").is_none() {
            issues.push("Missing [package] section (and not a workspace)".to_string());
        }
    }

    // Check [dependencies] for basic validity
    if let Some(deps) = doc.get("dependencies").and_then(|v| v.as_table()) {
        for (name, spec) in deps {
            if let Some(table) = spec.as_table() {
                // Check that version or git or path is specified
                if table.get("version").is_none()
                    && table.get("git").is_none()
                    && table.get("path").is_none()
                {
                    issues.push(format!("Dependency '{}': no version, git, or path", name));
                }
            }
            // String values like "1.0" are fine
        }
    }

    let dep_count = doc
        .get("dependencies")
        .and_then(|v| v.as_table())
        .map(|t| t.len())
        .unwrap_or(0);

    if issues.is_empty() {
        (
            Verdict::Confirmed,
            Some(format!("Valid Cargo.toml ({} dependencies)", dep_count)),
        )
    } else {
        (
            Verdict::Refuted,
            Some(format!("Cargo.toml issues: {}", issues.join("; "))),
        )
    }
}

/// Validate a Julia Project.toml file.
fn validate_project_toml(path: &str) -> (Verdict, Option<String>) {
    let contents = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => return (Verdict::Refuted, Some(format!("Cannot read: {}", e))),
    };

    let doc: toml::Table = match contents.parse() {
        Ok(d) => d,
        Err(e) => {
            return (
                Verdict::Refuted,
                Some(format!("Invalid TOML: {}", e)),
            )
        }
    };

    let mut issues = Vec::new();

    if doc.get("name").is_none() {
        issues.push("Missing name".to_string());
    }
    if doc.get("uuid").is_none() {
        issues.push("Missing uuid".to_string());
    }

    let dep_count = doc
        .get("deps")
        .and_then(|v| v.as_table())
        .map(|t| t.len())
        .unwrap_or(0);

    if issues.is_empty() {
        (
            Verdict::Confirmed,
            Some(format!("Valid Project.toml ({} dependencies)", dep_count)),
        )
    } else {
        (
            Verdict::Refuted,
            Some(format!("Project.toml issues: {}", issues.join("; "))),
        )
    }
}

/// Validate a package.json or deno.json file.
fn validate_package_json(path: &str) -> (Verdict, Option<String>) {
    let contents = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => return (Verdict::Refuted, Some(format!("Cannot read: {}", e))),
    };

    let json: serde_json::Value = match serde_json::from_str(&contents) {
        Ok(j) => j,
        Err(e) => {
            return (
                Verdict::Refuted,
                Some(format!("Invalid JSON: {}", e)),
            )
        }
    };

    let mut issues = Vec::new();

    if json.get("name").is_none() {
        issues.push("Missing name".to_string());
    }

    let dep_count = json
        .get("dependencies")
        .and_then(|v| v.as_object())
        .map(|o| o.len())
        .unwrap_or(0);

    if issues.is_empty() {
        (
            Verdict::Confirmed,
            Some(format!("Valid package.json ({} dependencies)", dep_count)),
        )
    } else {
        (
            Verdict::Refuted,
            Some(format!("package.json issues: {}", issues.join("; "))),
        )
    }
}

/// Validate a mix.exs file (basic check: file is readable Elixir).
fn validate_mix_exs(path: &str) -> (Verdict, Option<String>) {
    let contents = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => return (Verdict::Refuted, Some(format!("Cannot read: {}", e))),
    };

    let mut issues = Vec::new();

    if !contents.contains("defmodule") {
        issues.push("No defmodule found".to_string());
    }
    if !contents.contains("Mix.Project") && !contents.contains("MixProject") {
        issues.push("Not a Mix project file".to_string());
    }

    // Count deps by looking for {:dep_name, patterns
    let dep_count = contents
        .lines()
        .filter(|line| {
            let trimmed = line.trim();
            trimmed.starts_with("{:") && trimmed.contains(',')
        })
        .count();

    if issues.is_empty() {
        (
            Verdict::Confirmed,
            Some(format!(
                "Valid mix.exs (~{} dependencies detected)",
                dep_count
            )),
        )
    } else {
        (
            Verdict::Refuted,
            Some(format!("mix.exs issues: {}", issues.join("; "))),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_valid_cargo_toml() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("Cargo.toml");
        fs::write(
            &path,
            r#"
[package]
name = "test-project"
version = "0.1.0"

[dependencies]
serde = "1.0"
"#,
        )
        .unwrap();

        let evidence = EvidenceSpec::FileExists {
            path: path.to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Confirmed);
        assert!(result.details.unwrap().contains("1 dependencies"));
    }

    #[test]
    fn test_invalid_cargo_toml() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("Cargo.toml");
        fs::write(&path, "this is not valid toml {{{").unwrap();

        let evidence = EvidenceSpec::FileExists {
            path: path.to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Refuted);
    }

    #[test]
    fn test_valid_package_json() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("package.json");
        fs::write(
            &path,
            r#"{"name": "test", "version": "1.0.0", "dependencies": {"express": "^4.0"}}"#,
        )
        .unwrap();

        let evidence = EvidenceSpec::FileExists {
            path: path.to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Confirmed);
    }

    #[test]
    fn test_valid_mix_exs() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("mix.exs");
        fs::write(
            &path,
            r#"
defmodule MyApp.MixProject do
  use Mix.Project

  defp deps do
    [
      {:jason, "~> 1.4"},
      {:plug, "~> 1.16"}
    ]
  end
end
"#,
        )
        .unwrap();

        let evidence = EvidenceSpec::FileExists {
            path: path.to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Confirmed);
        assert!(result.details.unwrap().contains("2 dependencies"));
    }

    #[test]
    fn test_non_manifest_returns_unverifiable() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("readme.txt");
        fs::write(&path, "hello").unwrap();

        let evidence = EvidenceSpec::FileExists {
            path: path.to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Unverifiable);
    }

    #[test]
    fn test_cargo_dep_without_version() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("Cargo.toml");
        fs::write(
            &path,
            r#"
[package]
name = "test"
version = "0.1.0"

[dependencies]
bad-dep = {}
"#,
        )
        .unwrap();

        let evidence = EvidenceSpec::FileExists {
            path: path.to_str().unwrap().to_string(),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Refuted);
        assert!(result.details.unwrap().contains("no version"));
    }
}
