//! Storage layer for LLM Unify using SQLite

pub mod backup;
pub mod database;
pub mod error;
pub mod migration;
pub mod repository;

pub use backup::{
    check_integrity, compute_checksum, create_backup, restore_backup, validate_backup,
    BackupMetadata, DatabaseStats, BACKUP_FORMAT_VERSION,
};
pub use database::Database;
pub use error::{Error, Result};
pub use migration::{get_version, needs_migration, run_migrations, CURRENT_VERSION};
pub use repository::{ConversationRepository, MessageRepository};
