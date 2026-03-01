// SPDX-License-Identifier: MPL-2.0
//! Watch mode for continuous verification
//!
//! Re-runs verification when watched paths change.

use crate::{Claim, EvidenceSpec, VerificationReport, Verifier};
use notify::{Event, RecommendedWatcher, RecursiveMode, Watcher};
use std::collections::HashSet;
use std::path::Path;
use std::sync::mpsc;
use std::time::Duration;

/// Extract all file paths from a claim's evidence
pub fn extract_watch_paths(claim: &Claim) -> HashSet<String> {
    let mut paths = HashSet::new();

    for evidence in &claim.evidence {
        match evidence {
            EvidenceSpec::FileExists { path }
            | EvidenceSpec::FileWithHash { path, .. }
            | EvidenceSpec::FileContains { path, .. }
            | EvidenceSpec::FileMatchesRegex { path, .. }
            | EvidenceSpec::FileJsonPath { path, .. }
            | EvidenceSpec::FileModifiedAfter { path, .. } => {
                paths.insert(path.clone());
            }
            EvidenceSpec::DirectoryExists { path } => {
                paths.insert(path.clone());
            }
            EvidenceSpec::GitClean { repo_path }
            | EvidenceSpec::GitCommitExists { repo_path, .. }
            | EvidenceSpec::GitBranchExists { repo_path, .. } => {
                if let Some(p) = repo_path {
                    paths.insert(p.clone());
                } else {
                    paths.insert(".".to_string());
                }
            }
            EvidenceSpec::CommandSucceeds { .. }
            | EvidenceSpec::EnvVar { .. }
            | EvidenceSpec::Custom { .. } => {
                // These don't have watchable paths
            }
        }
    }

    paths
}

/// Watch claims and re-verify on changes
pub fn watch_and_verify<F>(
    claims: Vec<Claim>,
    on_report: F,
    debounce_ms: u64,
) -> Result<(), Box<dyn std::error::Error>>
where
    F: Fn(&VerificationReport) + Send + 'static,
{
    let (tx, rx) = mpsc::channel();

    let mut watcher = RecommendedWatcher::new(
        move |res: Result<Event, notify::Error>| {
            if let Ok(event) = res {
                let _ = tx.send(event);
            }
        },
        notify::Config::default().with_poll_interval(Duration::from_millis(debounce_ms)),
    )?;

    // Collect all paths to watch
    let mut watch_paths = HashSet::new();
    for claim in &claims {
        watch_paths.extend(extract_watch_paths(claim));
    }

    // Start watching paths
    for path in &watch_paths {
        let p = Path::new(path);
        if p.exists() {
            // Watch parent directory if it's a file
            let watch_target = if p.is_file() {
                p.parent().unwrap_or(p)
            } else {
                p
            };
            watcher.watch(watch_target, RecursiveMode::NonRecursive)?;
        }
    }

    let verifier = Verifier::new();

    // Initial verification
    println!("Initial verification:");
    for claim in &claims {
        let report = verifier.verify(claim);
        on_report(&report);
    }
    println!("\nWatching for changes... (Ctrl+C to stop)\n");

    // Watch loop
    loop {
        match rx.recv() {
            Ok(event) => {
                // Check if any watched path was affected
                let affected: Vec<_> = event
                    .paths
                    .iter()
                    .filter(|p| {
                        watch_paths.iter().any(|wp| {
                            let wp_path = Path::new(wp);
                            p.starts_with(wp_path) || wp_path.starts_with(p)
                        })
                    })
                    .collect();

                if !affected.is_empty() {
                    println!("Change detected in: {:?}", affected);
                    for claim in &claims {
                        let report = verifier.verify(claim);
                        on_report(&report);
                    }
                    println!();
                }
            }
            Err(e) => {
                eprintln!("Watch error: {}", e);
                break;
            }
        }
    }

    Ok(())
}
