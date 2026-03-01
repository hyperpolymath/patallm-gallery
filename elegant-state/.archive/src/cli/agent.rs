use clap::Subcommand;
use super::CapabilityModeArg;

#[derive(Subcommand)]
pub enum AgentCommands {
    /// List all configured agents
    List {
        /// Show detailed information
        #[arg(short, long)]
        verbose: bool,

        /// Include reputation scores
        #[arg(long)]
        reputation: bool,
    },

    /// Show agent configuration
    Show {
        /// Agent name (user, claude, llama, system, or module:*)
        agent: String,

        /// Show reputation history
        #[arg(long)]
        history: bool,
    },

    /// Set agent capabilities
    Set {
        /// Agent name
        agent: String,

        /// Capability mode
        #[arg(short, long, value_enum)]
        mode: Option<CapabilityModeArg>,

        /// Can vote on proposals
        #[arg(long)]
        can_vote: Option<bool>,

        /// Vote weight (0.0 - 2.0)
        #[arg(long)]
        vote_weight: Option<f32>,
    },

    /// Register a new module agent
    Register {
        /// Module name
        name: String,

        /// Initial capability mode
        #[arg(short, long, value_enum, default_value = "proposal")]
        mode: CapabilityModeArg,

        /// Module description
        #[arg(short, long)]
        description: Option<String>,
    },

    /// Unregister a module agent
    Unregister {
        /// Module name
        name: String,

        /// Skip confirmation
        #[arg(long)]
        force: bool,
    },

    /// Show reputation leaderboard
    Leaderboard {
        /// Number of entries to show
        #[arg(short, long, default_value = "10")]
        limit: usize,

        /// Sort by (score, accuracy, votes)
        #[arg(long, default_value = "score")]
        sort: String,
    },

    /// Reset agent reputation
    ResetReputation {
        /// Agent name (or "all" for all agents)
        agent: String,

        /// Skip confirmation
        #[arg(long)]
        force: bool,
    },

    /// Apply reputation decay
    Decay {
        /// Decay factor (0.0 - 1.0)
        #[arg(long, default_value = "0.99")]
        factor: f32,
    },

    /// Switch current agent identity
    Switch {
        /// Agent to switch to
        agent: String,
    },

    /// Show current agent identity
    Whoami,
}
