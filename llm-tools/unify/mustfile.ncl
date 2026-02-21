# SPDX-License-Identifier: AGPL-3.0-or-later
# Mustfile - Project requirements and standards configuration
# Nickel configuration for LLM Unify

{
  project = {
    name = "llm-unify",
    version = "0.1.0",
    license = "AGPL-3.0-or-later",
    rust_edition = "2021",
    min_rust_version = "1.75.0",
  },

  # Required documentation files
  documentation = {
    required = [
      "README.adoc",
      "SECURITY.md",
      "CONTRIBUTING.md",
      "CODE_OF_CONDUCT.md",
      "MAINTAINERS.md",
      "CHANGELOG.md",
      "RSR-COMPLIANCE.md",
      "ROADMAP.adoc",
    ],
    format = "adoc",  # Primary documentation format
  },

  # Well-known resources (RFC compliance)
  well_known = {
    required = [
      ".well-known/security.txt",  # RFC 9116
      ".well-known/ai.txt",
      ".well-known/humans.txt",
    ],
    config_source = ".config/well-known.scm",
  },

  # Code quality standards
  quality = {
    unsafe_blocks = 0,  # Zero unsafe code allowed
    test_coverage_target = 80,  # Percent
    clippy_warnings = "deny",
    format_check = true,
  },

  # RSR Compliance targets
  rsr = {
    current_level = "Silver",
    target_level = "Gold",
    current_points = 51,
    max_points = 55,
    categories = {
      documentation = { score = 5, max = 5 },
      type_safety = { score = 5, max = 5 },
      memory_safety = { score = 5, max = 5 },
      offline_first = { score = 5, max = 5 },
      well_known = { score = 5, max = 5 },
      build_system = { score = 5, max = 5 },
      test_coverage = { score = 3, max = 5 },  # Needs improvement
      tpcf = { score = 5, max = 5 },
      license = { score = 5, max = 5 },
      community = { score = 4, max = 5 },
      security = { score = 4, max = 5 },
    },
  },

  # Supported providers
  providers = {
    implemented = ["chatgpt", "claude"],
    stub = ["gemini", "copilot"],
    planned = [],
  },

  # Crate structure
  crates = [
    "llm-unify-core",
    "llm-unify-storage",
    "llm-unify-parser",
    "llm-unify-search",
    "llm-unify-tui",
    "llm-unify-cli",
  ],

  # CLI commands (for documentation sync)
  cli_commands = [
    "import",
    "list",
    "show",
    "search",
    "delete",
    "export",
    "stats",
    "validate",
    "backup",
    "restore",
    "init",
    "tui",
    "version",
    "schema",
  ],
}
