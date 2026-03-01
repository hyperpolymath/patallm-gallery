use clap::Subcommand;
use super::{NodeKindArg, SearchAlgorithm};

#[derive(Subcommand)]
pub enum SearchCommands {
    /// Full-text search using tantivy
    Fulltext {
        /// Search query
        query: String,

        /// Filter by node kinds
        #[arg(short, long)]
        kinds: Option<Vec<NodeKindArg>>,

        /// Maximum results
        #[arg(short, long, default_value = "20")]
        limit: usize,

        /// Minimum score threshold (0.0 - 1.0)
        #[arg(long)]
        min_score: Option<f32>,

        /// Show relevance scores
        #[arg(long)]
        scores: bool,

        /// Highlight matches in output
        #[arg(long)]
        highlight: bool,
    },

    /// Fuzzy search (skim/fzf-like)
    Fuzzy {
        /// Search pattern
        pattern: String,

        /// Filter by node kinds
        #[arg(short, long)]
        kinds: Option<Vec<NodeKindArg>>,

        /// Maximum results
        #[arg(short, long, default_value = "20")]
        limit: usize,

        /// Use nucleo algorithm instead of skim
        #[arg(long)]
        nucleo: bool,

        /// Case sensitive matching
        #[arg(long)]
        case_sensitive: bool,
    },

    /// Approximate matching (agrep-like)
    Agrep {
        /// Search pattern
        pattern: String,

        /// Maximum edit distance (errors allowed)
        #[arg(short, long, default_value = "2")]
        max_errors: usize,

        /// Filter by node kinds
        #[arg(short, long)]
        kinds: Option<Vec<NodeKindArg>>,

        /// Maximum results
        #[arg(short, long, default_value = "20")]
        limit: usize,
    },

    /// Exact substring search
    Exact {
        /// Search string
        query: String,

        /// Filter by node kinds
        #[arg(short, long)]
        kinds: Option<Vec<NodeKindArg>>,

        /// Case insensitive
        #[arg(short, long)]
        ignore_case: bool,

        /// Maximum results
        #[arg(short, long, default_value = "20")]
        limit: usize,
    },

    /// Search by metadata field
    Meta {
        /// Field name
        field: String,

        /// Field value (supports wildcards with *)
        value: String,

        /// Filter by node kinds
        #[arg(short, long)]
        kinds: Option<Vec<NodeKindArg>>,
    },

    /// Find nodes by edge relationships
    Related {
        /// Node ID to find relations for
        id: String,

        /// Edge direction (in, out, both)
        #[arg(short, long, default_value = "both")]
        direction: String,

        /// Edge kinds to follow
        #[arg(short, long)]
        edge_kinds: Option<Vec<String>>,

        /// Traversal depth
        #[arg(long, default_value = "1")]
        depth: usize,
    },

    /// Rebuild search index
    Reindex {
        /// Only index nodes of specific kinds
        #[arg(short, long)]
        kinds: Option<Vec<NodeKindArg>>,

        /// Show progress
        #[arg(long)]
        progress: bool,
    },
}
