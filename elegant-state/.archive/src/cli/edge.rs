use clap::Subcommand;

#[derive(Subcommand)]
pub enum EdgeCommands {
    /// Create a new edge
    Create {
        /// Source node ID
        #[arg(short, long)]
        from: String,

        /// Target node ID
        #[arg(short, long)]
        to: String,

        /// Edge kind (references, derived_from, related_to, part_of, blocks, enables, supersedes)
        #[arg(short, long)]
        kind: String,

        /// Edge weight (0.0 - 1.0)
        #[arg(short, long)]
        weight: Option<f32>,
    },

    /// List edges from a node
    From {
        /// Source node ID
        id: String,
    },

    /// List edges to a node
    To {
        /// Target node ID
        id: String,
    },

    /// Delete an edge
    Delete {
        /// Edge ID
        id: String,
    },
}
