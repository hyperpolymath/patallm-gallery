use clap::Subcommand;

#[derive(Subcommand)]
pub enum ServeCommands {
    /// Start HTTP server
    Http {
        /// Port to listen on
        #[arg(short, long, default_value = "4000")]
        port: u16,

        /// Host to bind to
        #[arg(short = 'H', long, default_value = "127.0.0.1")]
        host: String,
    },

    // Future: Unix socket support
    // Socket {
    //     #[arg(short, long, default_value = "/tmp/elegant-state.sock")]
    //     path: String,
    // },
}
