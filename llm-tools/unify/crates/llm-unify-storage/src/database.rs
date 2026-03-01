//! Database connection and initialization

use crate::migration;
use crate::Result;
use sqlx::sqlite::{SqliteConnectOptions, SqlitePool, SqlitePoolOptions};
use std::path::Path;
use std::str::FromStr;

/// Database connection pool
pub struct Database {
    pool: SqlitePool,
    path: String,
}

impl Database {
    /// Create a new database connection
    pub async fn new<P: AsRef<Path>>(path: P) -> Result<Self> {
        let path_str = path.as_ref().to_string_lossy().to_string();
        let options = SqliteConnectOptions::from_str(&format!("sqlite:{}", path_str))?
            .create_if_missing(true)
            .journal_mode(sqlx::sqlite::SqliteJournalMode::Wal) // WAL mode for safe backups
            .foreign_keys(true); // Enforce foreign key constraints

        let pool = SqlitePoolOptions::new()
            .max_connections(5)
            .connect_with(options)
            .await?;

        let db = Self {
            pool,
            path: path_str,
        };
        db.initialize().await?;
        Ok(db)
    }

    /// Initialize database schema using migrations
    async fn initialize(&self) -> Result<()> {
        migration::run_migrations(&self.pool).await?;
        Ok(())
    }

    /// Get the connection pool
    pub fn pool(&self) -> &SqlitePool {
        &self.pool
    }

    /// Get the database file path
    pub fn path(&self) -> &str {
        &self.path
    }

    /// Get current schema version
    pub async fn schema_version(&self) -> Result<i32> {
        migration::get_version(&self.pool).await
    }

    /// Check if database needs migration
    pub async fn needs_migration(&self) -> Result<bool> {
        migration::needs_migration(&self.pool).await
    }
}
