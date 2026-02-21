//! Database backup and restore with integrity guarantees

use crate::{Error, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use sqlx::SqlitePool;
use std::fs;
use std::io::Read;
use std::path::Path;

/// Backup metadata stored alongside the database backup
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BackupMetadata {
    /// Backup format version
    pub format_version: i32,
    /// Schema version of the backed up database
    pub schema_version: i32,
    /// SHA-256 checksum of the database file
    pub checksum: String,
    /// Timestamp when backup was created (RFC3339)
    pub created_at: String,
    /// Size of the database file in bytes
    pub file_size: u64,
    /// Application version that created the backup
    pub app_version: String,
}

/// Current backup format version
pub const BACKUP_FORMAT_VERSION: i32 = 1;

impl BackupMetadata {
    /// Get metadata file path for a backup
    pub fn metadata_path(backup_path: &Path) -> std::path::PathBuf {
        let mut path = backup_path.to_path_buf();
        let name = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("backup");
        path.set_file_name(format!("{}.meta.json", name));
        path
    }
}

/// Compute SHA-256 checksum of a file
pub fn compute_checksum(path: &Path) -> Result<String> {
    let mut file = fs::File::open(path)
        .map_err(|e| Error::Other(format!("Failed to open file for checksum: {}", e)))?;
    let mut hasher = Sha256::new();
    let mut buffer = [0u8; 8192];

    loop {
        let bytes_read = file
            .read(&mut buffer)
            .map_err(|e| Error::Other(format!("Failed to read file: {}", e)))?;
        if bytes_read == 0 {
            break;
        }
        hasher.update(&buffer[..bytes_read]);
    }

    Ok(format!("{:x}", hasher.finalize()))
}

/// Verify a file matches expected checksum
pub fn verify_checksum(path: &Path, expected: &str) -> Result<bool> {
    let actual = compute_checksum(path)?;
    Ok(actual == expected)
}

/// Create a backup with integrity guarantees
pub async fn create_backup(
    pool: &SqlitePool,
    source_path: &Path,
    backup_path: &Path,
) -> Result<BackupMetadata> {
    // Checkpoint WAL to ensure all data is in main database file
    sqlx::query("PRAGMA wal_checkpoint(TRUNCATE)")
        .execute(pool)
        .await?;

    // Get current schema version
    let schema_version = crate::migration::get_version(pool).await?;

    // Create temp file first for atomic operation
    let temp_path = backup_path.with_extension("tmp");

    // Copy database file
    fs::copy(source_path, &temp_path)
        .map_err(|e| Error::Other(format!("Failed to copy database: {}", e)))?;

    // Compute checksum of backup
    let checksum = compute_checksum(&temp_path)?;

    // Get file size
    let file_size = fs::metadata(&temp_path)
        .map_err(|e| Error::Other(format!("Failed to get file metadata: {}", e)))?
        .len();

    // Create metadata
    let metadata = BackupMetadata {
        format_version: BACKUP_FORMAT_VERSION,
        schema_version,
        checksum,
        created_at: chrono::Utc::now().to_rfc3339(),
        file_size,
        app_version: env!("CARGO_PKG_VERSION").to_string(),
    };

    // Write metadata file
    let meta_path = BackupMetadata::metadata_path(backup_path);
    let meta_json = serde_json::to_string_pretty(&metadata)
        .map_err(|e| Error::Other(format!("Failed to serialize metadata: {}", e)))?;

    fs::write(&meta_path, meta_json)
        .map_err(|e| Error::Other(format!("Failed to write metadata: {}", e)))?;

    // Atomic rename of temp file to final backup
    fs::rename(&temp_path, backup_path)
        .map_err(|e| Error::Other(format!("Failed to finalize backup: {}", e)))?;

    Ok(metadata)
}

/// Validate a backup before restore
pub fn validate_backup(backup_path: &Path) -> Result<BackupMetadata> {
    // Check backup file exists
    if !backup_path.exists() {
        return Err(Error::Other(format!(
            "Backup file not found: {}",
            backup_path.display()
        )));
    }

    // Load metadata
    let meta_path = BackupMetadata::metadata_path(backup_path);
    let metadata = if meta_path.exists() {
        let meta_json = fs::read_to_string(&meta_path)
            .map_err(|e| Error::Other(format!("Failed to read metadata: {}", e)))?;
        serde_json::from_str::<BackupMetadata>(&meta_json)
            .map_err(|e| Error::Other(format!("Invalid metadata format: {}", e)))?
    } else {
        // Legacy backup without metadata - compute checksum only
        let checksum = compute_checksum(backup_path)?;
        let file_size = fs::metadata(backup_path)
            .map_err(|e| Error::Other(format!("Failed to get file size: {}", e)))?
            .len();

        BackupMetadata {
            format_version: 0, // Unknown/legacy
            schema_version: 0, // Unknown
            checksum,
            created_at: "unknown".to_string(),
            file_size,
            app_version: "unknown".to_string(),
        }
    };

    // Verify checksum
    if !verify_checksum(backup_path, &metadata.checksum)? {
        return Err(Error::Other(
            "Backup checksum verification failed - file may be corrupted".to_string(),
        ));
    }

    // Verify file size
    let actual_size = fs::metadata(backup_path)
        .map_err(|e| Error::Other(format!("Failed to get file size: {}", e)))?
        .len();

    if actual_size != metadata.file_size {
        return Err(Error::Other(format!(
            "Backup size mismatch: expected {} bytes, got {} bytes",
            metadata.file_size, actual_size
        )));
    }

    // Basic SQLite file validation
    let mut file = fs::File::open(backup_path)
        .map_err(|e| Error::Other(format!("Failed to open backup: {}", e)))?;
    let mut header = [0u8; 16];
    file.read_exact(&mut header)
        .map_err(|e| Error::Other(format!("Failed to read backup header: {}", e)))?;

    // SQLite magic bytes: "SQLite format 3\0"
    if &header[..16] != b"SQLite format 3\0" {
        return Err(Error::Other(
            "Invalid SQLite file format".to_string(),
        ));
    }

    Ok(metadata)
}

/// Restore from a validated backup
pub fn restore_backup(backup_path: &Path, target_path: &Path, force: bool) -> Result<BackupMetadata> {
    // Validate backup first
    let metadata = validate_backup(backup_path)?;

    // Check schema compatibility
    if metadata.schema_version > crate::migration::CURRENT_VERSION && !force {
        return Err(Error::Other(format!(
            "Backup has newer schema version ({}) than current ({}). Use --force to override.",
            metadata.schema_version,
            crate::migration::CURRENT_VERSION
        )));
    }

    // Create backup of current database if it exists
    if target_path.exists() {
        let old_backup = target_path.with_extension("old");
        fs::rename(target_path, &old_backup)
            .map_err(|e| Error::Other(format!("Failed to backup existing database: {}", e)))?;
    }

    // Copy validated backup to target
    fs::copy(backup_path, target_path)
        .map_err(|e| Error::Other(format!("Failed to restore backup: {}", e)))?;

    // Verify the restored file
    if !verify_checksum(target_path, &metadata.checksum)? {
        return Err(Error::Other(
            "Restored database checksum mismatch - restore may have failed".to_string(),
        ));
    }

    Ok(metadata)
}

/// Validate database integrity using SQLite's integrity_check
pub async fn check_integrity(pool: &SqlitePool) -> Result<Vec<String>> {
    let rows: Vec<(String,)> = sqlx::query_as("PRAGMA integrity_check")
        .fetch_all(pool)
        .await?;

    let issues: Vec<String> = rows
        .into_iter()
        .map(|r| r.0)
        .filter(|s| s != "ok")
        .collect();

    Ok(issues)
}

/// Get database statistics for validation
pub async fn get_stats(pool: &SqlitePool) -> Result<DatabaseStats> {
    let conv_count: (i64,) = sqlx::query_as("SELECT COUNT(*) FROM conversations")
        .fetch_one(pool)
        .await?;

    let msg_count: (i64,) = sqlx::query_as("SELECT COUNT(*) FROM messages")
        .fetch_one(pool)
        .await?;

    let orphan_count: (i64,) = sqlx::query_as(
        "SELECT COUNT(*) FROM messages WHERE conversation_id NOT IN (SELECT id FROM conversations)",
    )
    .fetch_one(pool)
    .await?;

    let fts_count: (i64,) = sqlx::query_as("SELECT COUNT(*) FROM messages_fts")
        .fetch_one(pool)
        .await?;

    Ok(DatabaseStats {
        conversation_count: conv_count.0,
        message_count: msg_count.0,
        orphan_message_count: orphan_count.0,
        fts_entry_count: fts_count.0,
    })
}

/// Database statistics
#[derive(Debug, Clone)]
pub struct DatabaseStats {
    pub conversation_count: i64,
    pub message_count: i64,
    pub orphan_message_count: i64,
    pub fts_entry_count: i64,
}

impl DatabaseStats {
    /// Check if stats indicate a healthy database
    pub fn is_healthy(&self) -> bool {
        self.orphan_message_count == 0 && self.message_count == self.fts_entry_count
    }

    /// Get list of issues
    pub fn issues(&self) -> Vec<String> {
        let mut issues = Vec::new();

        if self.orphan_message_count > 0 {
            issues.push(format!(
                "Found {} orphan messages without parent conversation",
                self.orphan_message_count
            ));
        }

        if self.message_count != self.fts_entry_count {
            issues.push(format!(
                "FTS index out of sync: {} messages but {} FTS entries",
                self.message_count, self.fts_entry_count
            ));
        }

        issues
    }
}
