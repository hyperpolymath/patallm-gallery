use clap::Subcommand;

#[derive(Subcommand)]
pub enum DbCommands {
    /// Show database statistics
    Stats {
        /// Show detailed statistics
        #[arg(short, long)]
        verbose: bool,

        /// Include index statistics
        #[arg(long)]
        index: bool,
    },

    /// Initialize a new database
    Init {
        /// Database path (uses default if not specified)
        path: Option<String>,

        /// Force initialization even if database exists
        #[arg(long)]
        force: bool,
    },

    /// Backup database
    Backup {
        /// Backup destination (directory or file)
        output: Option<String>,

        /// Backup name (default: timestamp)
        #[arg(short, long)]
        name: Option<String>,

        /// Compress backup
        #[arg(long)]
        compress: bool,

        /// Include full-text index
        #[arg(long)]
        include_index: bool,
    },

    /// Restore database from backup
    Restore {
        /// Backup file or directory
        backup: String,

        /// Skip confirmation
        #[arg(long)]
        force: bool,

        /// Verify backup integrity before restore
        #[arg(long)]
        verify: bool,
    },

    /// List available backups
    Backups {
        /// Backup directory
        #[arg(short, long)]
        dir: Option<String>,
    },

    /// Compact database
    Compact {
        /// Compaction threshold in MB
        #[arg(long)]
        threshold: Option<usize>,

        /// Force compaction regardless of threshold
        #[arg(long)]
        force: bool,
    },

    /// Verify database integrity
    Verify {
        /// Fix issues if possible
        #[arg(long)]
        fix: bool,

        /// Check full-text index
        #[arg(long)]
        index: bool,
    },

    /// Repair database
    Repair {
        /// Skip confirmation
        #[arg(long)]
        force: bool,

        /// Create backup before repair
        #[arg(long, default_value = "true")]
        backup: bool,
    },

    /// Migrate database schema
    Migrate {
        /// Target schema version
        #[arg(long)]
        to_version: Option<String>,

        /// Show pending migrations
        #[arg(long)]
        pending: bool,

        /// Dry run - show what would be migrated
        #[arg(long)]
        dry_run: bool,
    },

    /// Show database path
    Path,

    /// Reset database (destructive!)
    Reset {
        /// Skip confirmation
        #[arg(long)]
        force: bool,

        /// Create backup before reset
        #[arg(long)]
        backup: bool,
    },

    /// Vacuum database (reclaim space)
    Vacuum {
        /// Show progress
        #[arg(long)]
        progress: bool,
    },
}
