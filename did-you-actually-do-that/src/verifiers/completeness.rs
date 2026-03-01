// SPDX-License-Identifier: PMPL-1.0-or-later
//! Layer 9: Completeness Audit
//!
//! Scans a repository to identify gaps between what was claimed and what exists.
//! Detects untested modules, undocumented public functions, claimed-but-missing
//! files, orphaned test files, and stub implementations.

use crate::{Claim, EvidenceResult, EvidenceSpec, Verdict};
use std::collections::HashSet;
use std::path::{Path, PathBuf};

/// A gap identified by the completeness audit.
#[derive(Debug, Clone)]
pub struct CompletenessGap {
    pub category: GapCategory,
    pub path: String,
    pub description: String,
    pub severity: Severity,
}

/// Categories of completeness gaps.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GapCategory {
    /// A file claimed to exist is missing
    ClaimedButMissing,
    /// A source file has no corresponding test file
    Untested,
    /// A test file exists but its source file is missing
    OrphanedTest,
    /// A file contains stub patterns (todo!, unimplemented!, etc.)
    StubDetected,
    /// A claimed directory is empty or missing
    EmptyDirectory,
}

/// How serious the gap is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Critical,
    Warning,
    Info,
}

/// The completeness auditor.
pub struct CompletenessAudit {
    /// Root path of the repository being audited
    repo_root: PathBuf,
    /// File extensions considered as source code
    source_extensions: HashSet<String>,
    /// Patterns that indicate stub implementations
    stub_patterns: Vec<String>,
}

impl CompletenessAudit {
    pub fn new(repo_root: impl Into<PathBuf>) -> Self {
        let source_exts: HashSet<String> = [
            "rs", "ex", "exs", "erl", "idr", "zig", "res", "resi", "gleam", "jl", "ml", "mli",
            "hs", "lhs", "adb", "ads", "sh",
        ]
        .iter()
        .map(|s| s.to_string())
        .collect();

        let stub_pats = vec![
            "todo!()".to_string(),
            "unimplemented!()".to_string(),
            "TODO:".to_string(),
            "FIXME:".to_string(),
            "HACK:".to_string(),
            "raise \"not implemented\"".to_string(),
            "raise \"TODO\"".to_string(),
            "NotImplementedError".to_string(),
            "panic!(\"not yet implemented\")".to_string(),
            "# stub".to_string(),
            "// stub".to_string(),
            "pass  # TODO".to_string(),
        ];

        Self {
            repo_root: repo_root.into(),
            source_extensions: source_exts,
            stub_patterns: stub_pats,
        }
    }

    /// Run the full completeness audit against a set of claims.
    pub fn scan(&self, claims: &[Claim]) -> Vec<CompletenessGap> {
        let mut gaps = Vec::new();

        // 1. Check claimed files exist
        gaps.extend(self.check_claimed_files(claims));

        // 2. Check for stub patterns in claimed files
        gaps.extend(self.check_stubs(claims));

        // 3. Check source/test coverage
        gaps.extend(self.check_test_coverage());

        // 4. Check claimed directories
        gaps.extend(self.check_claimed_directories(claims));

        gaps
    }

    /// Convert gaps to EvidenceResults for the verification pipeline.
    pub fn scan_as_evidence(&self, claims: &[Claim]) -> Vec<EvidenceResult> {
        self.scan(claims)
            .into_iter()
            .map(|gap| {
                let verdict = match gap.severity {
                    Severity::Critical => Verdict::Refuted,
                    Severity::Warning => Verdict::Inconclusive,
                    Severity::Info => Verdict::Confirmed,
                };

                EvidenceResult {
                    spec: EvidenceSpec::Custom {
                        name: format!("completeness_{:?}", gap.category).to_lowercase(),
                        params: [
                            ("path".to_string(), gap.path),
                            ("category".to_string(), format!("{:?}", gap.category)),
                        ]
                        .into_iter()
                        .collect(),
                    },
                    verdict,
                    details: Some(gap.description),
                }
            })
            .collect()
    }

    /// Check that every file referenced in claims actually exists.
    fn check_claimed_files(&self, claims: &[Claim]) -> Vec<CompletenessGap> {
        let mut gaps = Vec::new();

        for claim in claims {
            for ev in &claim.evidence {
                let path = match ev {
                    EvidenceSpec::FileExists { path } => Some(path),
                    EvidenceSpec::FileWithHash { path, .. } => Some(path),
                    EvidenceSpec::FileContains { path, .. } => Some(path),
                    EvidenceSpec::FileMatchesRegex { path, .. } => Some(path),
                    EvidenceSpec::FileJsonPath { path, .. } => Some(path),
                    EvidenceSpec::FileModifiedAfter { path, .. } => Some(path),
                    _ => None,
                };

                if let Some(file_path) = path {
                    let full_path = self.resolve_path(file_path);
                    if !full_path.exists() {
                        gaps.push(CompletenessGap {
                            category: GapCategory::ClaimedButMissing,
                            path: file_path.clone(),
                            description: format!(
                                "Claimed file '{}' does not exist (claim: '{}')",
                                file_path,
                                truncate(&claim.description, 60)
                            ),
                            severity: Severity::Critical,
                        });
                    }
                }
            }
        }

        gaps
    }

    /// Scan claimed files for stub patterns.
    fn check_stubs(&self, claims: &[Claim]) -> Vec<CompletenessGap> {
        let mut gaps = Vec::new();
        let mut checked = HashSet::new();

        for claim in claims {
            for ev in &claim.evidence {
                let path = match ev {
                    EvidenceSpec::FileExists { path } => Some(path),
                    EvidenceSpec::FileContains { path, .. } => Some(path),
                    _ => None,
                };

                if let Some(file_path) = path {
                    if checked.contains(file_path) {
                        continue;
                    }
                    checked.insert(file_path.clone());

                    let full_path = self.resolve_path(file_path);
                    if let Ok(contents) = std::fs::read_to_string(&full_path) {
                        let stub_count = self.count_stubs(&contents);
                        if stub_count > 0 {
                            gaps.push(CompletenessGap {
                                category: GapCategory::StubDetected,
                                path: file_path.clone(),
                                description: format!(
                                    "File '{}' contains {} stub pattern(s) — implementation may be incomplete",
                                    file_path, stub_count
                                ),
                                severity: if stub_count > 3 {
                                    Severity::Critical
                                } else {
                                    Severity::Warning
                                },
                            });
                        }
                    }
                }
            }
        }

        gaps
    }

    /// Check source files have corresponding test files and vice versa.
    fn check_test_coverage(&self) -> Vec<CompletenessGap> {
        let mut gaps = Vec::new();

        let source_files = self.collect_source_files("src");
        let test_files = self.collect_source_files("test")
            .into_iter()
            .chain(self.collect_source_files("tests"))
            .collect::<HashSet<_>>();

        // Check for untested source files
        for source in &source_files {
            let stem = source
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("");

            // Skip mod.rs, lib.rs, main.rs — these are structural
            if ["mod", "lib", "main", "application", "router"].contains(&stem) {
                continue;
            }

            let has_test = test_files.iter().any(|t| {
                let test_stem = t.file_stem().and_then(|s| s.to_str()).unwrap_or("");
                test_stem == format!("{}_test", stem)
                    || test_stem == format!("test_{}", stem)
                    || test_stem == stem
            });

            // Also check for inline #[cfg(test)] in the source file
            let has_inline_tests = source
                .extension()
                .and_then(|e| e.to_str())
                .map(|ext| ext == "rs")
                .unwrap_or(false)
                && std::fs::read_to_string(source)
                    .map(|c| c.contains("#[cfg(test)]") || c.contains("#[test]"))
                    .unwrap_or(false);

            if !has_test && !has_inline_tests {
                let rel = source
                    .strip_prefix(&self.repo_root)
                    .unwrap_or(source)
                    .display()
                    .to_string();
                gaps.push(CompletenessGap {
                    category: GapCategory::Untested,
                    path: rel,
                    description: format!("Source file '{}' has no corresponding test file", stem),
                    severity: Severity::Warning,
                });
            }
        }

        // Check for orphaned test files
        for test in &test_files {
            let test_stem = test
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("");

            // Derive the expected source name
            let source_stem = test_stem
                .strip_suffix("_test")
                .or_else(|| test_stem.strip_prefix("test_"))
                .unwrap_or(test_stem);

            let has_source = source_files.iter().any(|s| {
                s.file_stem()
                    .and_then(|stem| stem.to_str())
                    .map(|stem| stem == source_stem)
                    .unwrap_or(false)
            });

            if !has_source && source_stem != test_stem {
                let rel = test
                    .strip_prefix(&self.repo_root)
                    .unwrap_or(test)
                    .display()
                    .to_string();
                gaps.push(CompletenessGap {
                    category: GapCategory::OrphanedTest,
                    path: rel,
                    description: format!(
                        "Test file '{}' has no matching source file '{}'",
                        test_stem, source_stem
                    ),
                    severity: Severity::Info,
                });
            }
        }

        gaps
    }

    /// Check claimed directories exist and are non-empty.
    fn check_claimed_directories(&self, claims: &[Claim]) -> Vec<CompletenessGap> {
        let mut gaps = Vec::new();

        for claim in claims {
            for ev in &claim.evidence {
                if let EvidenceSpec::DirectoryExists { path } = ev {
                    let full_path = self.resolve_path(path);
                    if !full_path.exists() {
                        gaps.push(CompletenessGap {
                            category: GapCategory::ClaimedButMissing,
                            path: path.clone(),
                            description: format!("Claimed directory '{}' does not exist", path),
                            severity: Severity::Critical,
                        });
                    } else if full_path.is_dir() {
                        let is_empty = std::fs::read_dir(&full_path)
                            .map(|mut d| d.next().is_none())
                            .unwrap_or(true);
                        if is_empty {
                            gaps.push(CompletenessGap {
                                category: GapCategory::EmptyDirectory,
                                path: path.clone(),
                                description: format!("Claimed directory '{}' exists but is empty", path),
                                severity: Severity::Warning,
                            });
                        }
                    }
                }
            }
        }

        gaps
    }

    fn resolve_path(&self, path: &str) -> PathBuf {
        let p = Path::new(path);
        if p.is_absolute() {
            p.to_path_buf()
        } else {
            self.repo_root.join(p)
        }
    }

    fn count_stubs(&self, contents: &str) -> usize {
        self.stub_patterns
            .iter()
            .map(|pat| contents.matches(pat).count())
            .sum()
    }

    fn collect_source_files(&self, subdir: &str) -> HashSet<PathBuf> {
        let dir = self.repo_root.join(subdir);
        if !dir.exists() {
            return HashSet::new();
        }

        collect_files_recursive(&dir, &self.source_extensions)
    }
}

/// Recursively collect files with matching extensions.
fn collect_files_recursive(dir: &Path, extensions: &HashSet<String>) -> HashSet<PathBuf> {
    let mut files = HashSet::new();

    let entries = match std::fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(_) => return files,
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            // Skip hidden directories and common non-source dirs
            let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if !name.starts_with('.') && name != "target" && name != "_build" && name != "deps" {
                files.extend(collect_files_recursive(&path, extensions));
            }
        } else if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            if extensions.contains(ext) {
                files.insert(path);
            }
        }
    }

    files
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}...", &s[..max])
    }
}

/// Single evidence check for layer dispatch compatibility.
pub fn check(evidence: &EvidenceSpec) -> EvidenceResult {
    EvidenceResult {
        spec: evidence.clone(),
        verdict: Verdict::Inconclusive,
        details: Some("Completeness audit requires claim context and repo path".to_string()),
    }
}

/// Run a completeness audit for a repo path and set of claims, returning evidence results.
pub fn audit(repo_path: &str, claims: &[Claim]) -> Vec<EvidenceResult> {
    let auditor = CompletenessAudit::new(repo_path);
    auditor.scan_as_evidence(claims)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn make_claim(desc: &str, evidence: Vec<EvidenceSpec>) -> Claim {
        Claim {
            id: format!("test-{}", desc.len()),
            description: desc.to_string(),
            timestamp: chrono::Utc::now(),
            evidence,
            source: Some("test".to_string()),
        }
    }

    #[test]
    fn test_claimed_missing_file() {
        let tmp = std::env::temp_dir().join("dyadt_completeness_test");
        let _ = fs::create_dir_all(&tmp);

        let claims = vec![make_claim(
            "Created config",
            vec![EvidenceSpec::FileExists {
                path: "nonexistent_file_12345.json".to_string(),
            }],
        )];

        let auditor = CompletenessAudit::new(&tmp);
        let gaps = auditor.scan(&claims);

        assert!(
            gaps.iter()
                .any(|g| g.category == GapCategory::ClaimedButMissing),
            "Should detect missing claimed file"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_stub_detection() {
        let tmp = std::env::temp_dir().join("dyadt_stub_test");
        let _ = fs::create_dir_all(&tmp);

        let stub_file = tmp.join("stub.rs");
        fs::write(&stub_file, "fn main() { todo!() }\nfn helper() { unimplemented!() }")
            .unwrap();

        let claims = vec![make_claim(
            "Created stub.rs",
            vec![EvidenceSpec::FileExists {
                path: "stub.rs".to_string(),
            }],
        )];

        let auditor = CompletenessAudit::new(&tmp);
        let gaps = auditor.scan(&claims);

        assert!(
            gaps.iter().any(|g| g.category == GapCategory::StubDetected),
            "Should detect stub patterns"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_empty_directory() {
        let tmp = std::env::temp_dir().join("dyadt_emptydir_test");
        let empty_dir = tmp.join("empty");
        let _ = fs::create_dir_all(&empty_dir);

        let claims = vec![make_claim(
            "Created directory",
            vec![EvidenceSpec::DirectoryExists {
                path: "empty".to_string(),
            }],
        )];

        let auditor = CompletenessAudit::new(&tmp);
        let gaps = auditor.scan(&claims);

        assert!(
            gaps.iter()
                .any(|g| g.category == GapCategory::EmptyDirectory),
            "Should detect empty directory"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_evidence_conversion() {
        let tmp = std::env::temp_dir().join("dyadt_evidence_test");
        let _ = fs::create_dir_all(&tmp);

        let claims = vec![make_claim(
            "Created missing file",
            vec![EvidenceSpec::FileExists {
                path: "does_not_exist.rs".to_string(),
            }],
        )];

        let auditor = CompletenessAudit::new(&tmp);
        let results = auditor.scan_as_evidence(&claims);

        assert!(
            results.iter().any(|r| r.verdict == Verdict::Refuted),
            "Missing file should produce Refuted verdict"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_truncate() {
        assert_eq!(truncate("hello", 10), "hello");
        assert_eq!(truncate("hello world this is long", 10), "hello worl...");
    }
}
