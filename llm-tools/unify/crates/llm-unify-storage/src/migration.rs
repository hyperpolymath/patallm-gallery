//! Database migrations and schema versioning

use crate::Result;
use sqlx::SqlitePool;

/// Current schema version
pub const CURRENT_VERSION: i32 = 1;

/// Migration definition
pub struct Migration {
    pub version: i32,
    pub description: &'static str,
    pub up: &'static str,
}

/// All migrations in order
pub const MIGRATIONS: &[Migration] = &[
    Migration {
        version: 1,
        description: "Initial schema with conversations, messages, and FTS5",
        up: r#"
CREATE TABLE IF NOT EXISTS conversations (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    provider TEXT NOT NULL,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    metadata TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS messages (
    id TEXT PRIMARY KEY,
    conversation_id TEXT NOT NULL,
    role TEXT NOT NULL,
    content TEXT NOT NULL,
    timestamp TEXT NOT NULL,
    metadata TEXT NOT NULL,
    FOREIGN KEY (conversation_id) REFERENCES conversations(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_messages_conversation_id ON messages(conversation_id);
CREATE INDEX IF NOT EXISTS idx_messages_timestamp ON messages(timestamp);
CREATE INDEX IF NOT EXISTS idx_conversations_provider ON conversations(provider);
CREATE INDEX IF NOT EXISTS idx_conversations_updated_at ON conversations(updated_at);

CREATE VIRTUAL TABLE IF NOT EXISTS messages_fts USING fts5(
    content,
    content=messages,
    content_rowid=rowid
);

CREATE TRIGGER IF NOT EXISTS messages_ai AFTER INSERT ON messages BEGIN
    INSERT INTO messages_fts(rowid, content) VALUES (new.rowid, new.content);
END;

CREATE TRIGGER IF NOT EXISTS messages_ad AFTER DELETE ON messages BEGIN
    DELETE FROM messages_fts WHERE rowid = old.rowid;
END;

CREATE TRIGGER IF NOT EXISTS messages_au AFTER UPDATE ON messages BEGIN
    DELETE FROM messages_fts WHERE rowid = old.rowid;
    INSERT INTO messages_fts(rowid, content) VALUES (new.rowid, new.content);
END;
"#,
    },
];

/// Schema version table creation
const VERSION_TABLE: &str = r#"
CREATE TABLE IF NOT EXISTS schema_version (
    version INTEGER PRIMARY KEY,
    applied_at TEXT NOT NULL,
    description TEXT NOT NULL
);
"#;

/// Get current schema version from database
pub async fn get_version(pool: &SqlitePool) -> Result<i32> {
    // Check if version table exists
    let table_exists: Option<(i32,)> = sqlx::query_as(
        "SELECT 1 FROM sqlite_master WHERE type='table' AND name='schema_version'",
    )
    .fetch_optional(pool)
    .await?;

    if table_exists.is_none() {
        return Ok(0); // No version table means version 0
    }

    // Get max version - MAX can return NULL if table is empty
    let version: Option<(Option<i32>,)> =
        sqlx::query_as("SELECT MAX(version) FROM schema_version")
            .fetch_optional(pool)
            .await?;

    Ok(version.and_then(|v| v.0).unwrap_or(0))
}

/// Run all pending migrations
pub async fn run_migrations(pool: &SqlitePool) -> Result<Vec<i32>> {
    // Ensure version table exists
    sqlx::query(VERSION_TABLE).execute(pool).await?;

    let current = get_version(pool).await?;
    let mut applied = Vec::new();

    for migration in MIGRATIONS {
        if migration.version > current {
            // Run migration in a transaction
            let mut tx = pool.begin().await?;

            sqlx::query(migration.up).execute(&mut *tx).await?;

            sqlx::query(
                "INSERT INTO schema_version (version, applied_at, description) VALUES (?, datetime('now'), ?)",
            )
            .bind(migration.version)
            .bind(migration.description)
            .execute(&mut *tx)
            .await?;

            tx.commit().await?;
            applied.push(migration.version);
        }
    }

    Ok(applied)
}

/// Check if migrations are needed
pub async fn needs_migration(pool: &SqlitePool) -> Result<bool> {
    let current = get_version(pool).await?;
    Ok(current < CURRENT_VERSION)
}

/// Get migration history
pub async fn get_history(pool: &SqlitePool) -> Result<Vec<(i32, String, String)>> {
    let rows: Vec<(i32, String, String)> = sqlx::query_as(
        "SELECT version, applied_at, description FROM schema_version ORDER BY version",
    )
    .fetch_all(pool)
    .await?;

    Ok(rows)
}
