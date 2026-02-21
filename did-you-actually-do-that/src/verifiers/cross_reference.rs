// SPDX-License-Identifier: PMPL-1.0-or-later
//! Layer 8: Cross-Reference Analysis
//!
//! Validates consistency across multiple claims by building a reference graph.
//! Detects contradictions, temporal inconsistencies, orphaned references,
//! and import/export dependency violations.

use crate::{Claim, EvidenceResult, EvidenceSpec, Verdict};
use std::collections::{HashMap, HashSet};

/// A directed reference graph tracking relationships between claimed artifacts.
#[derive(Debug, Default)]
pub struct ReferenceGraph {
    /// Nodes: artifact path -> set of claims referencing it
    nodes: HashMap<String, Vec<ClaimRef>>,
    /// Edges: source path -> set of paths it imports/references
    edges: HashMap<String, HashSet<String>>,
}

/// A reference from a claim to an artifact, with action semantics.
#[derive(Debug, Clone)]
struct ClaimRef {
    claim_index: usize,
    action: ArtifactAction,
    timestamp: i64,
}

/// What a claim says happened to an artifact.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ArtifactAction {
    Created,
    Modified,
    Deleted,
    Referenced,
}

impl ReferenceGraph {
    pub fn new() -> Self {
        Self::default()
    }

    /// Build the reference graph from a set of claims.
    pub fn build(claims: &[Claim]) -> Self {
        let mut graph = Self::new();

        for (idx, claim) in claims.iter().enumerate() {
            let desc_lower = claim.description.to_lowercase();
            let action = Self::infer_action(&desc_lower);
            let timestamp = claim.timestamp.timestamp();

            for ev in &claim.evidence {
                if let Some(path) = Self::extract_path(ev) {
                    graph
                        .nodes
                        .entry(path.clone())
                        .or_default()
                        .push(ClaimRef {
                            claim_index: idx,
                            action,
                            timestamp,
                        });
                }
            }

            // Parse import/export references from description
            let imports = Self::extract_imports(&claim.description);
            let exports = Self::extract_exports(&claim.description);

            // Build edges: imported modules are dependencies
            for imp in &imports {
                for exp in &exports {
                    graph
                        .edges
                        .entry(exp.clone())
                        .or_default()
                        .insert(imp.clone());
                }
            }

            // Also build edges from evidence paths
            let paths: Vec<String> = claim
                .evidence
                .iter()
                .filter_map(Self::extract_path)
                .collect();
            if paths.len() > 1 {
                let primary = &paths[0];
                for dep in &paths[1..] {
                    graph
                        .edges
                        .entry(primary.clone())
                        .or_default()
                        .insert(dep.clone());
                }
            }
        }

        graph
    }

    fn infer_action(description: &str) -> ArtifactAction {
        if description.contains("created")
            || description.contains("added")
            || description.contains("wrote")
            || description.contains("generated")
        {
            ArtifactAction::Created
        } else if description.contains("deleted")
            || description.contains("removed")
            || description.contains("dropped")
        {
            ArtifactAction::Deleted
        } else if description.contains("modified")
            || description.contains("updated")
            || description.contains("changed")
            || description.contains("edited")
            || description.contains("fixed")
        {
            ArtifactAction::Modified
        } else {
            ArtifactAction::Referenced
        }
    }

    fn extract_path(evidence: &EvidenceSpec) -> Option<String> {
        match evidence {
            EvidenceSpec::FileExists { path } => Some(path.clone()),
            EvidenceSpec::FileWithHash { path, .. } => Some(path.clone()),
            EvidenceSpec::FileContains { path, .. } => Some(path.clone()),
            EvidenceSpec::FileMatchesRegex { path, .. } => Some(path.clone()),
            EvidenceSpec::FileJsonPath { path, .. } => Some(path.clone()),
            EvidenceSpec::DirectoryExists { path } => Some(path.clone()),
            EvidenceSpec::FileModifiedAfter { path, .. } => Some(path.clone()),
            _ => None,
        }
    }

    /// Extract import-like references from claim text.
    fn extract_imports(description: &str) -> Vec<String> {
        let mut imports = Vec::new();
        let patterns = [
            "imports ", "import ", "use ", "require ", "include ",
            "depends on ", "from ",
        ];
        let desc_lower = description.to_lowercase();
        for pat in &patterns {
            if let Some(pos) = desc_lower.find(pat) {
                let rest = &description[pos + pat.len()..];
                if let Some(module) = rest.split_whitespace().next() {
                    let clean = module.trim_matches(|c: char| !c.is_alphanumeric() && c != '/' && c != '.' && c != '_' && c != ':');
                    if !clean.is_empty() {
                        imports.push(clean.to_string());
                    }
                }
            }
        }
        imports
    }

    /// Extract export-like references from claim text.
    fn extract_exports(description: &str) -> Vec<String> {
        let mut exports = Vec::new();
        let patterns = ["exports ", "provides ", "defines ", "exposes "];
        let desc_lower = description.to_lowercase();
        for pat in &patterns {
            if let Some(pos) = desc_lower.find(pat) {
                let rest = &description[pos + pat.len()..];
                if let Some(module) = rest.split_whitespace().next() {
                    let clean = module.trim_matches(|c: char| !c.is_alphanumeric() && c != '/' && c != '.' && c != '_' && c != ':');
                    if !clean.is_empty() {
                        exports.push(clean.to_string());
                    }
                }
            }
        }
        exports
    }
}

/// Check cross-reference consistency for a set of claims.
///
/// Performs four analyses:
/// 1. **Contradiction detection**: Same artifact claimed as both created and deleted
/// 2. **Temporal consistency**: Timestamps must be monotonically ordered per artifact
/// 3. **Orphaned reference detection**: References to artifacts with no creation claim
/// 4. **Circular dependency detection**: Import cycles in the reference graph
pub fn check_consistency(claims: &[Claim]) -> Vec<EvidenceResult> {
    if claims.len() < 2 {
        return vec![];
    }

    let graph = ReferenceGraph::build(claims);
    let mut results = Vec::new();

    // 1. Contradiction detection
    results.extend(detect_contradictions(&graph));

    // 2. Temporal consistency
    results.extend(detect_temporal_violations(&graph));

    // 3. Orphaned references
    results.extend(detect_orphaned_references(&graph, claims));

    // 4. Circular dependencies
    results.extend(detect_circular_deps(&graph));

    results
}

/// Detect artifacts claimed as both created and deleted.
fn detect_contradictions(graph: &ReferenceGraph) -> Vec<EvidenceResult> {
    let mut results = Vec::new();

    for (path, refs) in &graph.nodes {
        let has_create = refs.iter().any(|r| r.action == ArtifactAction::Created);
        let has_delete = refs.iter().any(|r| r.action == ArtifactAction::Deleted);

        if has_create && has_delete {
            results.push(EvidenceResult {
                spec: EvidenceSpec::Custom {
                    name: "cross_ref_contradiction".to_string(),
                    params: [("path".to_string(), path.clone())]
                        .into_iter()
                        .collect(),
                },
                verdict: Verdict::Refuted,
                details: Some(format!(
                    "Contradiction: '{}' claimed as both created and deleted in the same claim set",
                    path
                )),
            });
        }
    }

    results
}

/// Detect temporal ordering violations (later claims referencing earlier timestamps).
fn detect_temporal_violations(graph: &ReferenceGraph) -> Vec<EvidenceResult> {
    let mut results = Vec::new();

    for (path, refs) in &graph.nodes {
        if refs.len() < 2 {
            continue;
        }

        // Sort by claim index (submission order) and check timestamps
        let mut sorted_refs: Vec<&ClaimRef> = refs.iter().collect();
        sorted_refs.sort_by_key(|r| r.claim_index);

        for window in sorted_refs.windows(2) {
            let earlier = window[0];
            let later = window[1];

            // If a later-submitted claim has an earlier timestamp, that's suspicious
            if later.timestamp < earlier.timestamp - 1 {
                results.push(EvidenceResult {
                    spec: EvidenceSpec::Custom {
                        name: "cross_ref_temporal".to_string(),
                        params: [("path".to_string(), path.clone())]
                            .into_iter()
                            .collect(),
                    },
                    verdict: Verdict::Inconclusive,
                    details: Some(format!(
                        "Temporal anomaly: later claim for '{}' has earlier timestamp (delta: {}s)",
                        path,
                        earlier.timestamp - later.timestamp
                    )),
                });
            }
        }
    }

    results
}

/// Detect references to artifacts that no claim says were created.
fn detect_orphaned_references(graph: &ReferenceGraph, claims: &[Claim]) -> Vec<EvidenceResult> {
    let mut results = Vec::new();

    // Collect all referenced paths that are only referenced (not created)
    let created_paths: HashSet<&String> = graph
        .nodes
        .iter()
        .filter(|(_, refs)| refs.iter().any(|r| r.action == ArtifactAction::Created))
        .map(|(path, _)| path)
        .collect();

    // Check edges for references to non-created paths
    for (source, deps) in &graph.edges {
        for dep in deps {
            // Only flag if the dependency path looks like a project file (not stdlib)
            if !created_paths.contains(dep)
                && !dep.starts_with("std")
                && !dep.starts_with("core")
                && dep.contains('/')
            {
                // Verify it's not just an external dependency
                let is_internal = claims.iter().any(|c| {
                    c.evidence
                        .iter()
                        .any(|e| ReferenceGraph::extract_path(e).as_ref() == Some(dep))
                });

                if is_internal {
                    results.push(EvidenceResult {
                        spec: EvidenceSpec::Custom {
                            name: "cross_ref_orphan".to_string(),
                            params: [
                                ("source".to_string(), source.clone()),
                                ("dependency".to_string(), dep.clone()),
                            ]
                            .into_iter()
                            .collect(),
                        },
                        verdict: Verdict::Inconclusive,
                        details: Some(format!(
                            "Orphaned reference: '{}' depends on '{}' but no claim creates it",
                            source, dep
                        )),
                    });
                }
            }
        }
    }

    results
}

/// Detect circular dependencies in the reference graph using DFS.
fn detect_circular_deps(graph: &ReferenceGraph) -> Vec<EvidenceResult> {
    let mut results = Vec::new();
    let mut visited = HashSet::new();
    let mut in_stack = HashSet::new();

    for node in graph.edges.keys() {
        if !visited.contains(node) {
            let mut path = Vec::new();
            if has_cycle(node, graph, &mut visited, &mut in_stack, &mut path) {
                results.push(EvidenceResult {
                    spec: EvidenceSpec::Custom {
                        name: "cross_ref_cycle".to_string(),
                        params: [("cycle".to_string(), path.join(" -> "))]
                            .into_iter()
                            .collect(),
                    },
                    verdict: Verdict::Refuted,
                    details: Some(format!(
                        "Circular dependency detected: {}",
                        path.join(" -> ")
                    )),
                });
            }
        }
    }

    results
}

fn has_cycle(
    node: &str,
    graph: &ReferenceGraph,
    visited: &mut HashSet<String>,
    in_stack: &mut HashSet<String>,
    path: &mut Vec<String>,
) -> bool {
    visited.insert(node.to_string());
    in_stack.insert(node.to_string());
    path.push(node.to_string());

    if let Some(deps) = graph.edges.get(node) {
        for dep in deps {
            if !visited.contains(dep.as_str()) {
                if has_cycle(dep, graph, visited, in_stack, path) {
                    return true;
                }
            } else if in_stack.contains(dep.as_str()) {
                path.push(dep.clone());
                return true;
            }
        }
    }

    in_stack.remove(node);
    path.pop();
    false
}

/// Single evidence check (for compatibility with layer dispatch).
pub fn check(evidence: &EvidenceSpec) -> EvidenceResult {
    EvidenceResult {
        spec: evidence.clone(),
        verdict: Verdict::Inconclusive,
        details: Some("Cross-reference analysis requires multiple claims".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;

    fn make_claim(desc: &str, paths: &[&str]) -> Claim {
        let mut claim = Claim {
            id: format!("test-{}", desc.len()),
            description: desc.to_string(),
            timestamp: Utc::now(),
            evidence: Vec::new(),
            source: Some("test".to_string()),
        };
        for path in paths {
            claim.evidence.push(EvidenceSpec::FileExists {
                path: path.to_string(),
            });
        }
        claim
    }

    #[test]
    fn test_no_claims() {
        let results = check_consistency(&[]);
        assert!(results.is_empty());
    }

    #[test]
    fn test_single_claim() {
        let claims = vec![make_claim("Created foo.rs", &["src/foo.rs"])];
        let results = check_consistency(&claims);
        assert!(results.is_empty());
    }

    #[test]
    fn test_contradiction_detected() {
        let claims = vec![
            make_claim("Created the config file", &["config.json"]),
            make_claim("Deleted the config file", &["config.json"]),
        ];
        let results = check_consistency(&claims);
        assert!(
            results.iter().any(|r| r.verdict == Verdict::Refuted),
            "Should detect create+delete contradiction"
        );
    }

    #[test]
    fn test_no_contradiction_different_paths() {
        let claims = vec![
            make_claim("Created the config file", &["config.json"]),
            make_claim("Deleted the old config", &["old_config.json"]),
        ];
        let results = check_consistency(&claims);
        let contradictions: Vec<_> = results
            .iter()
            .filter(|r| r.verdict == Verdict::Refuted)
            .collect();
        assert!(contradictions.is_empty());
    }

    #[test]
    fn test_consistent_claims() {
        let claims = vec![
            make_claim("Created module A", &["src/a.rs"]),
            make_claim("Created module B that imports A", &["src/b.rs"]),
        ];
        let results = check_consistency(&claims);
        // No contradictions expected
        assert!(results.iter().all(|r| r.verdict != Verdict::Refuted));
    }

    #[test]
    fn test_reference_graph_building() {
        let claims = vec![
            make_claim("Created the helper", &["src/helper.rs"]),
            make_claim("Modified main to use helper", &["src/main.rs", "src/helper.rs"]),
        ];
        let graph = ReferenceGraph::build(&claims);
        assert!(graph.nodes.contains_key("src/helper.rs"));
        assert!(graph.nodes.contains_key("src/main.rs"));
        assert_eq!(graph.nodes["src/helper.rs"].len(), 2);
    }

    #[test]
    fn test_action_inference() {
        assert_eq!(
            ReferenceGraph::infer_action("created a new module"),
            ArtifactAction::Created
        );
        assert_eq!(
            ReferenceGraph::infer_action("deleted old test file"),
            ArtifactAction::Deleted
        );
        assert_eq!(
            ReferenceGraph::infer_action("modified the configuration"),
            ArtifactAction::Modified
        );
        assert_eq!(
            ReferenceGraph::infer_action("ran the tests"),
            ArtifactAction::Referenced
        );
    }
}
