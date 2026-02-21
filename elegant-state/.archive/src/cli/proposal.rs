use clap::Subcommand;

#[derive(Subcommand)]
pub enum ProposalCommands {
    /// List proposals
    List {
        /// Show only pending proposals
        #[arg(long)]
        pending: bool,

        /// Show only my proposals
        #[arg(long)]
        mine: bool,

        /// Filter by status (pending, approved, rejected, expired, withdrawn)
        #[arg(short, long)]
        status: Option<String>,

        /// Number of proposals to show
        #[arg(short, long, default_value = "20")]
        limit: usize,

        /// Show detailed information
        #[arg(short, long)]
        verbose: bool,
    },

    /// Show proposal details
    Show {
        /// Proposal ID
        id: String,

        /// Show vote details
        #[arg(long)]
        votes: bool,

        /// Show full payload
        #[arg(long)]
        payload: bool,
    },

    /// Create a new proposal
    Create {
        /// Operation type (create, update, delete, link, unlink)
        operation: String,

        /// Target (node:ID, edge:ID, or new:kind)
        target: String,

        /// Payload as JSON
        #[arg(short, long)]
        payload: String,

        /// Rationale for the proposal
        #[arg(short, long)]
        rationale: Option<String>,
    },

    /// Withdraw a proposal
    Withdraw {
        /// Proposal ID
        id: String,

        /// Reason for withdrawal
        #[arg(short, long)]
        reason: Option<String>,
    },

    /// Approve a proposal (shortcut for vote approve)
    Approve {
        /// Proposal ID
        id: String,

        /// Reason for approval
        #[arg(short, long)]
        reason: Option<String>,
    },

    /// Reject a proposal (shortcut for vote reject)
    Reject {
        /// Proposal ID
        id: String,

        /// Reason for rejection
        #[arg(short, long)]
        reason: Option<String>,
    },

    /// Show votes on a proposal
    Votes {
        /// Proposal ID
        id: String,

        /// Show detailed vote information
        #[arg(short, long)]
        verbose: bool,
    },

    /// Execute an approved proposal
    Execute {
        /// Proposal ID
        id: String,

        /// Skip execution confirmation
        #[arg(long)]
        force: bool,
    },

    /// Expire old pending proposals
    Expire {
        /// Only expire proposals older than (e.g., "1h", "2d")
        #[arg(long)]
        older_than: Option<String>,

        /// Dry run - show what would be expired
        #[arg(long)]
        dry_run: bool,
    },

    /// Clean up resolved proposals
    Cleanup {
        /// Keep proposals newer than (e.g., "7d", "30d")
        #[arg(long, default_value = "7d")]
        keep: String,

        /// Dry run - show what would be removed
        #[arg(long)]
        dry_run: bool,
    },
}
