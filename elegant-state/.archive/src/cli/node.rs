use clap::Subcommand;

#[derive(Subcommand)]
pub enum NodeCommands {
    /// Create a new node
    Create {
        /// Node kind (conversation, project, insight, task, context, module, agent)
        #[arg(short, long)]
        kind: String,

        /// Node content as JSON
        #[arg(short, long)]
        content: String,

        /// Optional metadata as JSON
        #[arg(short, long)]
        metadata: Option<String>,
    },

    /// Get a node by ID
    Get {
        /// Node ID (ULID)
        id: String,
    },

    /// List nodes
    List {
        /// Filter by kind
        #[arg(short, long)]
        kind: Option<String>,

        /// Maximum number of nodes to return
        #[arg(short, long, default_value = "20")]
        limit: usize,
    },

    /// Update a node
    Update {
        /// Node ID
        id: String,

        /// New content as JSON
        #[arg(short, long)]
        content: String,
    },

    /// Delete a node
    Delete {
        /// Node ID
        id: String,

        /// Skip confirmation
        #[arg(short, long)]
        force: bool,
    },
}
