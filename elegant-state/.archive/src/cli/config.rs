use clap::Subcommand;
use super::VotingStrategyArg;

#[derive(Subcommand)]
pub enum ConfigCommands {
    /// Show current configuration
    Show {
        /// Show only specific section
        #[arg(short, long)]
        section: Option<String>,

        /// Output as JSON
        #[arg(long)]
        json: bool,
    },

    /// Get a specific configuration value
    Get {
        /// Configuration key (dot-separated path)
        key: String,
    },

    /// Set a configuration value
    Set {
        /// Configuration key (dot-separated path)
        key: String,

        /// Value to set
        value: String,
    },

    /// Reset configuration to defaults
    Reset {
        /// Reset only specific section
        #[arg(short, long)]
        section: Option<String>,

        /// Skip confirmation
        #[arg(long)]
        force: bool,
    },

    /// Validate configuration file
    Validate {
        /// Config file to validate
        file: Option<String>,

        /// Show detailed validation errors
        #[arg(long)]
        verbose: bool,
    },

    /// Generate configuration file
    Generate {
        /// Output file
        #[arg(short, long, default_value = "config.ncl")]
        output: String,

        /// Environment preset (dev, staging, prod)
        #[arg(short, long, default_value = "dev")]
        env: String,

        /// Output format (ncl, json, toml, yaml)
        #[arg(short, long, default_value = "ncl")]
        format: String,
    },

    /// Switch between configuration presets
    Preset {
        /// Preset name (dev, staging, prod, minimal, neurophone)
        name: String,
    },

    /// Edit configuration in $EDITOR
    Edit {
        /// Config file to edit
        file: Option<String>,
    },

    /// Show configuration diff between environments
    Diff {
        /// First environment
        from: String,

        /// Second environment
        to: String,
    },

    /// Set voting strategy
    Voting {
        /// Voting strategy
        #[arg(value_enum)]
        strategy: VotingStrategyArg,

        /// Minimum voters required
        #[arg(long)]
        min_voters: Option<usize>,

        /// Timeout in seconds
        #[arg(long)]
        timeout: Option<u64>,
    },
}
