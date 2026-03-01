K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# K9 Kennel-level contractile: panic-attack AssailReport schema
# Security Level: Kennel (data-only, no execution)
# No signature required — pure data validation
#
# Validates the structure of a raw panic-attack AssailReport JSON file.
# Does NOT verify attestation — that is the Yard-level contractile's job.
# This contractile ensures the report has all required fields with correct
# types before any trust-related checks are attempted.

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "report-schema",
    security = {
      leash = 'Kennel,
      trust_level = "data-only",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = {
      name = "panic-attack-report",
      version = "1.0.0",
      description = "Pure data schema for raw panic-attack AssailReport JSON. Validates required fields and types without any attestation checks.",
      author = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>",
    },
  },

  # Contract definitions for AssailReport structure validation
  contracts = {
    # Severity must be one of the defined levels
    Severity = [| 'Critical, 'High, 'Medium, 'Low, 'Info |],

    # A single weak point found by static analysis
    WeakPoint = {
      category | String,
      description | String,
      file | String,
      line | Number,
      severity | String,
      ..
    },

    # Per-file statistics block
    ProgramStatistics = {
      total_lines | Number,
      unsafe_blocks | Number,
      panic_sites | Number,
      unwrap_calls | Number,
      allocation_sites | Number,
      io_operations | Number,
      threading_constructs | Number,
    },

    # Per-file statistics entry
    FileStatistics = {
      file_path | String,
      lines | Number,
      unsafe_blocks | Number,
      panic_sites | Number,
      unwrap_calls | Number,
      allocation_sites | Number,
      io_operations | Number,
      threading_constructs | Number,
    },

    # Overall assessment from an AssaultReport (attack results)
    OverallAssessment = {
      robustness_score | Number,
      critical_issues | Array String,
      recommendations | Array String,
    },

    # The top-level AssailReport schema
    AssailReport = {
      program_path | String,
      language | String,
      weak_points | Array WeakPoint,
      statistics | ProgramStatistics,
      file_statistics | Array FileStatistics,
      recommended_attacks | Array String,
      ..
    },
  },

  # Validation rules for report plausibility
  validation = {
    # program_path must not be empty
    check_program_path =
      std.contract.from_predicate (fun report =>
        std.string.length report.program_path > 0
      ),

    # language must not be empty
    check_language =
      std.contract.from_predicate (fun report =>
        std.string.length report.language > 0
      ),

    # total_lines must be non-negative
    check_total_lines =
      std.contract.from_predicate (fun report =>
        report.statistics.total_lines >= 0
      ),
  },
}
