use clap::Subcommand;

#[derive(Subcommand)]
pub enum GraphqlCommands {
    /// Export GraphQL schema
    Schema {
        /// Output file (- for stdout)
        #[arg(short, long, default_value = "-")]
        output: String,

        /// Include descriptions
        #[arg(long, default_value = "true")]
        descriptions: bool,
    },

    /// Execute a GraphQL query
    Query {
        /// Query string or @file
        query: String,

        /// Variables as JSON
        #[arg(short, long)]
        variables: Option<String>,

        /// Operation name
        #[arg(short, long)]
        operation: Option<String>,

        /// Pretty print output
        #[arg(short, long)]
        pretty: bool,
    },

    /// Execute a GraphQL mutation
    Mutate {
        /// Mutation string or @file
        mutation: String,

        /// Variables as JSON
        #[arg(short, long)]
        variables: Option<String>,

        /// Operation name
        #[arg(short, long)]
        operation: Option<String>,

        /// Dry run - validate but don't execute
        #[arg(long)]
        dry_run: bool,
    },

    /// Introspect the GraphQL API
    Introspect {
        /// Server URL (default: http://127.0.0.1:4000/graphql)
        #[arg(short, long)]
        url: Option<String>,

        /// Output format (json, sdl)
        #[arg(short, long, default_value = "sdl")]
        format: String,
    },

    /// Validate a GraphQL query/mutation
    Validate {
        /// Query/mutation string or @file
        query: String,
    },

    /// Start GraphQL playground
    Playground {
        /// Server URL
        #[arg(short, long, default_value = "http://127.0.0.1:4000/graphql")]
        url: String,

        /// Open in browser
        #[arg(long)]
        open: bool,
    },

    /// Generate TypeScript types from schema
    Codegen {
        /// Output file
        #[arg(short, long, default_value = "types.ts")]
        output: String,

        /// Include resolvers
        #[arg(long)]
        resolvers: bool,
    },

    /// Monitor GraphQL subscriptions
    Subscribe {
        /// Subscription query
        query: String,

        /// Variables as JSON
        #[arg(short, long)]
        variables: Option<String>,

        /// Server URL
        #[arg(short, long, default_value = "ws://127.0.0.1:4000/graphql")]
        url: String,

        /// Output each event on a new line (NDJSON)
        #[arg(long)]
        ndjson: bool,
    },
}
