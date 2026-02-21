mod node;
mod edge;
mod serve;

pub use node::NodeCommands;
pub use edge::EdgeCommands;
pub use serve::ServeCommands;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "state-cli")]
#[command(author = "elegant-STATE")]
#[command(version = "0.1.0")]
#[command(about = "Local-first state graph for multi-agent orchestration")]
pub struct Cli {
    /// Path to the state database
    #[arg(short, long, default_value = "~/.local/share/elegant-state/db")]
    pub db_path: String,

    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Node operations
    Node {
        #[command(subcommand)]
        command: NodeCommands,
    },

    /// Edge operations
    Edge {
        #[command(subcommand)]
        command: EdgeCommands,
    },

    /// Search the state graph
    Search {
        /// Search query
        query: String,

        /// Filter by node kinds (comma-separated)
        #[arg(short, long)]
        kinds: Option<String>,
    },

    /// Show recent events
    Events {
        /// Number of events to show
        #[arg(short, long, default_value = "20")]
        limit: usize,

        /// Filter by agent
        #[arg(short, long)]
        agent: Option<String>,
    },

    /// Export state to JSON
    Export {
        /// Output format
        #[arg(short, long, default_value = "json")]
        format: String,
    },

    /// Import state from JSON
    Import {
        /// Input file
        file: String,
    },

    /// Start GraphQL server
    Serve {
        #[command(subcommand)]
        command: ServeCommands,
    },
}
