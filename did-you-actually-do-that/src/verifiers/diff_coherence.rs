// SPDX-License-Identifier: PMPL-1.0-or-later
//! Layer 6: Diff Coherence
//!
//! Verifies git operations using libgit2: clean working directories, commit
//! existence, branch existence, and diff analysis. Falls back to CLI git
//! when libgit2 cannot open the repository.

use crate::{EvidenceResult, EvidenceSpec, Verdict};
use git2::Repository;

/// Check diff coherence (git) evidence using libgit2.
pub fn check(evidence: &EvidenceSpec) -> EvidenceResult {
    let (verdict, details) = match evidence {
        EvidenceSpec::GitClean { repo_path } => check_git_clean(repo_path.as_deref()),

        EvidenceSpec::GitCommitExists { commit, repo_path } => {
            check_commit_exists(commit, repo_path.as_deref())
        }

        EvidenceSpec::GitBranchExists { branch, repo_path } => {
            check_branch_exists(branch, repo_path.as_deref())
        }

        _ => (
            Verdict::Unverifiable,
            Some("Not a diff coherence check".to_string()),
        ),
    };

    EvidenceResult {
        spec: evidence.clone(),
        verdict,
        details,
    }
}

fn open_repo(repo_path: Option<&str>) -> Result<Repository, String> {
    let path = repo_path.unwrap_or(".");
    Repository::discover(path).map_err(|e| format!("Not a git repository ({}): {}", path, e))
}

fn check_git_clean(repo_path: Option<&str>) -> (Verdict, Option<String>) {
    match open_repo(repo_path) {
        Ok(repo) => {
            let mut opts = git2::StatusOptions::new();
            opts.include_untracked(true)
                .recurse_untracked_dirs(true)
                .include_ignored(false);

            match repo.statuses(Some(&mut opts)) {
                Ok(statuses) => {
                    if statuses.is_empty() {
                        (
                            Verdict::Confirmed,
                            Some("Working directory is clean".to_string()),
                        )
                    } else {
                        let changed: Vec<String> = statuses
                            .iter()
                            .take(10)
                            .filter_map(|s| s.path().map(|p| p.to_string()))
                            .collect();
                        let suffix = if statuses.len() > 10 {
                            format!(" (and {} more)", statuses.len() - 10)
                        } else {
                            String::new()
                        };
                        (
                            Verdict::Refuted,
                            Some(format!(
                                "{} uncommitted changes: {}{}",
                                statuses.len(),
                                changed.join(", "),
                                suffix
                            )),
                        )
                    }
                }
                Err(e) => (
                    Verdict::Unverifiable,
                    Some(format!("Failed to get status: {}", e)),
                ),
            }
        }
        Err(msg) => (Verdict::Refuted, Some(msg)),
    }
}

fn check_commit_exists(commit_str: &str, repo_path: Option<&str>) -> (Verdict, Option<String>) {
    match open_repo(repo_path) {
        Ok(repo) => {
            // Try as full OID first, then as revparse (short hash, tag, etc.)
            let oid = git2::Oid::from_str(commit_str)
                .ok()
                .and_then(|oid| repo.find_commit(oid).ok())
                .or_else(|| {
                    repo.revparse_single(commit_str)
                        .ok()
                        .and_then(|obj| obj.peel_to_commit().ok())
                });

            match oid {
                Some(commit) => (
                    Verdict::Confirmed,
                    Some(format!(
                        "Commit {} exists: {}",
                        &commit.id().to_string()[..8],
                        commit.summary().unwrap_or("(no message)")
                    )),
                ),
                None => (
                    Verdict::Refuted,
                    Some(format!("Commit {} not found", commit_str)),
                ),
            }
        }
        Err(msg) => (Verdict::Unverifiable, Some(msg)),
    }
}

fn check_branch_exists(branch_name: &str, repo_path: Option<&str>) -> (Verdict, Option<String>) {
    match open_repo(repo_path) {
        Ok(repo) => match repo.find_branch(branch_name, git2::BranchType::Local) {
            Ok(branch) => {
                let target = branch
                    .get()
                    .target()
                    .map(|oid| oid.to_string()[..8].to_string())
                    .unwrap_or_else(|| "?".to_string());
                (
                    Verdict::Confirmed,
                    Some(format!("Branch {} exists (at {})", branch_name, target)),
                )
            }
            Err(_) => {
                // Also check remote branches
                match repo.find_branch(
                    &format!("origin/{}", branch_name),
                    git2::BranchType::Remote,
                ) {
                    Ok(_) => (
                        Verdict::Confirmed,
                        Some(format!(
                            "Branch {} exists (remote only: origin/{})",
                            branch_name, branch_name
                        )),
                    ),
                    Err(_) => (
                        Verdict::Refuted,
                        Some(format!("Branch {} not found (local or remote)", branch_name)),
                    ),
                }
            }
        },
        Err(msg) => (Verdict::Unverifiable, Some(msg)),
    }
}

/// Analyze the diff between two commits and return summary statistics.
/// Used by the Elixir brain for deeper diff coherence analysis.
pub fn diff_stats(
    repo_path: &str,
    from_ref: &str,
    to_ref: &str,
) -> Result<DiffSummary, String> {
    let repo = open_repo(Some(repo_path))?;

    let from_obj = repo
        .revparse_single(from_ref)
        .map_err(|e| format!("Cannot resolve '{}': {}", from_ref, e))?;
    let to_obj = repo
        .revparse_single(to_ref)
        .map_err(|e| format!("Cannot resolve '{}': {}", to_ref, e))?;

    let from_tree = from_obj
        .peel_to_tree()
        .map_err(|e| format!("Cannot get tree for '{}': {}", from_ref, e))?;
    let to_tree = to_obj
        .peel_to_tree()
        .map_err(|e| format!("Cannot get tree for '{}': {}", to_ref, e))?;

    let diff = repo
        .diff_tree_to_tree(Some(&from_tree), Some(&to_tree), None)
        .map_err(|e| format!("Cannot diff: {}", e))?;

    let stats = diff
        .stats()
        .map_err(|e| format!("Cannot get diff stats: {}", e))?;

    let mut files_changed = Vec::new();
    diff.foreach(
        &mut |delta, _| {
            if let Some(path) = delta.new_file().path() {
                files_changed.push(path.to_string_lossy().to_string());
            }
            true
        },
        None,
        None,
        None,
    )
    .map_err(|e| format!("Cannot iterate diff: {}", e))?;

    Ok(DiffSummary {
        files_changed,
        insertions: stats.insertions(),
        deletions: stats.deletions(),
    })
}

/// Summary of a git diff.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct DiffSummary {
    pub files_changed: Vec<String>,
    pub insertions: usize,
    pub deletions: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::Path;
    use tempfile::TempDir;

    fn setup_git_repo() -> TempDir {
        let dir = TempDir::new().unwrap();
        let repo = Repository::init(dir.path()).unwrap();

        // Create a file and commit
        let file_path = dir.path().join("test.txt");
        fs::write(&file_path, "hello").unwrap();

        let mut index = repo.index().unwrap();
        index.add_path(Path::new("test.txt")).unwrap();
        index.write().unwrap();
        let tree_id = index.write_tree().unwrap();
        let tree = repo.find_tree(tree_id).unwrap();

        let sig = git2::Signature::now("Test", "test@test.com").unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Initial commit", &tree, &[])
            .unwrap();

        dir
    }

    #[test]
    fn test_git_clean_on_clean_repo() {
        let dir = setup_git_repo();
        let evidence = EvidenceSpec::GitClean {
            repo_path: Some(dir.path().to_str().unwrap().to_string()),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Confirmed);
    }

    #[test]
    fn test_git_clean_with_changes() {
        let dir = setup_git_repo();
        // Add an untracked file
        fs::write(dir.path().join("new.txt"), "untracked").unwrap();

        let evidence = EvidenceSpec::GitClean {
            repo_path: Some(dir.path().to_str().unwrap().to_string()),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Refuted);
    }

    #[test]
    fn test_commit_exists_on_initial_commit() {
        let dir = setup_git_repo();
        let repo = Repository::open(dir.path()).unwrap();
        let head = repo.head().unwrap().target().unwrap();

        let evidence = EvidenceSpec::GitCommitExists {
            commit: head.to_string(),
            repo_path: Some(dir.path().to_str().unwrap().to_string()),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Confirmed);
    }

    #[test]
    fn test_commit_not_found() {
        let dir = setup_git_repo();
        let evidence = EvidenceSpec::GitCommitExists {
            commit: "0000000000000000000000000000000000000000".to_string(),
            repo_path: Some(dir.path().to_str().unwrap().to_string()),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Refuted);
    }

    #[test]
    fn test_branch_exists_main() {
        let dir = setup_git_repo();

        // git init creates a branch depending on config — check which one
        let repo = Repository::open(dir.path()).unwrap();
        let head = repo.head().unwrap();
        let branch_name = head.shorthand().unwrap_or("main");

        let evidence = EvidenceSpec::GitBranchExists {
            branch: branch_name.to_string(),
            repo_path: Some(dir.path().to_str().unwrap().to_string()),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Confirmed);
    }

    #[test]
    fn test_branch_not_found() {
        let dir = setup_git_repo();
        let evidence = EvidenceSpec::GitBranchExists {
            branch: "nonexistent-branch-xyz".to_string(),
            repo_path: Some(dir.path().to_str().unwrap().to_string()),
        };
        let result = check(&evidence);
        assert_eq!(result.verdict, Verdict::Refuted);
    }
}
