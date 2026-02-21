// SPDX-License-Identifier: AGPL-3.0-or-later
//! LLM Unify CLI

use anyhow::Result;
use clap::{Parser, Subcommand};
use llm_unify_core::Provider;
use llm_unify_parser::get_parser;
use llm_unify_search::SearchEngine;
use llm_unify_storage::{
    check_integrity, create_backup, restore_backup, validate_backup, BackupMetadata,
    ConversationRepository, Database, BACKUP_FORMAT_VERSION, CURRENT_VERSION,
};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// Export format version for portability
pub const EXPORT_FORMAT_VERSION: i32 = 1;

/// Versioned export wrapper
#[derive(Debug, Serialize, Deserialize)]
pub struct VersionedExport<T> {
    /// Export format version
    pub format_version: i32,
    /// Application version that created the export
    pub app_version: String,
    /// Schema version of source database
    pub schema_version: i32,
    /// Timestamp when export was created (RFC3339)
    pub exported_at: String,
    /// The exported data
    pub data: T,
}

#[derive(Parser)]
#[command(name = "llm-unify")]
#[command(about = "Unified interface for managing LLM conversations", long_about = None)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Database file path
    #[arg(short, long, default_value = "llm-unify.db")]
    database: PathBuf,
}

#[derive(Subcommand)]
enum Commands {
    /// Import conversations from a provider
    Import {
        /// Provider name (chatgpt, claude, gemini, copilot)
        provider: String,

        /// Path to export file
        file: PathBuf,
    },

    /// List all conversations
    List {
        /// Filter by provider
        #[arg(short, long)]
        provider: Option<String>,
    },

    /// Show a conversation
    Show {
        /// Conversation ID
        id: String,
    },

    /// Search conversations
    Search {
        /// Search query
        query: String,

        /// Limit results
        #[arg(short, long, default_value = "10")]
        limit: usize,
    },

    /// Delete a conversation
    Delete {
        /// Conversation ID
        id: String,
    },

    /// Export a conversation
    Export {
        /// Conversation ID
        id: String,

        /// Output file
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Export raw format without version wrapper
        #[arg(long)]
        raw: bool,
    },

    /// Show statistics
    Stats,

    /// Validate database integrity
    Validate,

    /// Backup database with integrity verification
    Backup {
        /// Backup file path
        output: PathBuf,
    },

    /// Restore from backup with validation
    Restore {
        /// Backup file path
        input: PathBuf,

        /// Force restore even if schema version is newer
        #[arg(long)]
        force: bool,
    },

    /// Initialize database
    Init,

    /// Launch TUI
    Tui,

    /// Show version information
    Version,

    /// Show schema and migration info
    Schema,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Handle commands that don't need database connection first
    match &cli.command {
        Commands::Version => {
            println!("llm-unify v{}", env!("CARGO_PKG_VERSION"));
            println!("Schema version: {}", CURRENT_VERSION);
            println!("Backup format: {}", BACKUP_FORMAT_VERSION);
            println!("Export format: {}", EXPORT_FORMAT_VERSION);
            return Ok(());
        }
        Commands::Restore { input, force } => {
            return handle_restore(&cli.database, input, *force);
        }
        _ => {}
    }

    // Open database for all other commands
    let db = Database::new(&cli.database).await?;

    match cli.command {
        Commands::Import { provider, file } => {
            let provider_enum = parse_provider(&provider)?;
            let parser = get_parser(provider_enum);

            let data = std::fs::read(&file)?;
            let conversations = parser.parse(&data)?;
            let count = conversations.len();

            let repo = ConversationRepository::new(&db);
            for conv in conversations {
                repo.save(&conv).await?;
            }

            println!("Imported {} conversations from {}", count, provider);
        }

        Commands::List { provider } => {
            let repo = ConversationRepository::new(&db);
            let conversations = repo.list().await?;

            let filtered: Vec<_> = if let Some(p) = provider {
                let provider_enum = parse_provider(&p)?;
                conversations
                    .into_iter()
                    .filter(|c| c.provider == provider_enum)
                    .collect()
            } else {
                conversations
            };

            for conv in filtered {
                println!(
                    "{} | {} | {} | {} messages",
                    conv.id,
                    conv.provider,
                    conv.title,
                    conv.message_count()
                );
            }
        }

        Commands::Show { id } => {
            let repo = ConversationRepository::new(&db);
            if let Some(conv) = repo.find_by_id(&id).await? {
                println!("Conversation: {}", conv.title);
                println!("Provider: {}", conv.provider);
                println!("Messages: {}", conv.message_count());
                println!();

                for msg in conv.messages {
                    println!("[{}] {}", msg.role, msg.content);
                    println!();
                }
            } else {
                println!("Conversation not found");
            }
        }

        Commands::Search { query, limit } => {
            let search = SearchEngine::new(&db);
            let results = search.search(&query).await?;

            for (i, result) in results.iter().take(limit).enumerate() {
                println!("{}. Conversation: {}", i + 1, result.conversation_id);
                println!("   {}", result.snippet);
                println!();
            }
        }

        Commands::Delete { id } => {
            let repo = ConversationRepository::new(&db);
            repo.delete(&id).await?;
            println!("Deleted conversation: {}", id);
        }

        Commands::Export { id, output, raw } => {
            let repo = ConversationRepository::new(&db);
            if let Some(conv) = repo.find_by_id(&id).await? {
                let json = if raw {
                    serde_json::to_string_pretty(&conv)?
                } else {
                    let schema_version = db.schema_version().await?;
                    let export = VersionedExport {
                        format_version: EXPORT_FORMAT_VERSION,
                        app_version: env!("CARGO_PKG_VERSION").to_string(),
                        schema_version,
                        exported_at: chrono::Utc::now().to_rfc3339(),
                        data: conv,
                    };
                    serde_json::to_string_pretty(&export)?
                };

                if let Some(path) = output {
                    std::fs::write(&path, json)?;
                    println!("Exported to: {}", path.display());
                } else {
                    println!("{}", json);
                }
            } else {
                println!("Conversation not found");
            }
        }

        Commands::Stats => {
            let repo = ConversationRepository::new(&db);
            let conversations = repo.list().await?;

            let total_convs = conversations.len();
            let total_msgs: usize = conversations.iter().map(|c| c.message_count()).sum();

            println!("Total conversations: {}", total_convs);
            println!("Total messages: {}", total_msgs);

            // Count by provider
            let mut provider_counts = std::collections::HashMap::new();
            for conv in conversations {
                *provider_counts.entry(conv.provider).or_insert(0) += 1;
            }

            println!("\nBy provider:");
            for (provider, count) in provider_counts {
                println!("  {}: {}", provider, count);
            }
        }

        Commands::Validate => {
            println!("Validating database integrity...\n");

            // SQLite integrity check
            let issues = check_integrity(db.pool()).await?;
            if issues.is_empty() {
                println!("[OK] SQLite integrity check passed");
            } else {
                println!("[FAIL] SQLite integrity issues:");
                for issue in &issues {
                    println!("  - {}", issue);
                }
            }

            // Schema version check
            let version = db.schema_version().await?;
            println!("[OK] Schema version: {} (current: {})", version, CURRENT_VERSION);

            // Data consistency check
            let stats = llm_unify_storage::backup::get_stats(db.pool()).await?;
            println!(
                "[OK] Data: {} conversations, {} messages",
                stats.conversation_count, stats.message_count
            );

            let stat_issues = stats.issues();
            if stat_issues.is_empty() {
                println!("[OK] Data consistency check passed");
            } else {
                println!("[WARN] Data consistency issues:");
                for issue in &stat_issues {
                    println!("  - {}", issue);
                }
            }

            if issues.is_empty() && stat_issues.is_empty() {
                println!("\nDatabase validation: PASSED");
            } else {
                println!("\nDatabase validation: FAILED");
                std::process::exit(1);
            }
        }

        Commands::Backup { output } => {
            println!("Creating backup...");

            let source_path = Path::new(&cli.database);
            let metadata = create_backup(db.pool(), source_path, &output).await?;

            println!("\nBackup created successfully!");
            println!("  File: {}", output.display());
            println!("  Size: {} bytes", metadata.file_size);
            println!("  Checksum: {}", metadata.checksum);
            println!("  Schema version: {}", metadata.schema_version);
            println!(
                "  Metadata: {}",
                BackupMetadata::metadata_path(&output).display()
            );
        }

        Commands::Init => {
            println!("Database initialized: {}", cli.database.display());
            println!("Schema version: {}", db.schema_version().await?);
        }

        Commands::Tui => {
            llm_unify_tui::run(db).await?;
        }

        Commands::Schema => {
            let version = db.schema_version().await?;
            println!("Database: {}", cli.database.display());
            println!("Current schema version: {}", version);
            println!("Latest schema version: {}", CURRENT_VERSION);

            if version < CURRENT_VERSION {
                println!("\nMigrations pending: {} -> {}", version, CURRENT_VERSION);
            } else {
                println!("\nDatabase is up to date.");
            }

            // Show migration history
            let history = llm_unify_storage::migration::get_history(db.pool()).await?;
            if !history.is_empty() {
                println!("\nMigration history:");
                for (ver, applied_at, desc) in history {
                    println!("  v{}: {} ({})", ver, desc, applied_at);
                }
            }
        }

        // These are handled above before opening the database
        Commands::Version | Commands::Restore { .. } => unreachable!(),
    }

    Ok(())
}

/// Handle restore command separately to avoid opening database first
fn handle_restore(target_path: &Path, input: &Path, force: bool) -> Result<()> {
    println!("Validating backup...");

    // Validate first
    let metadata = validate_backup(input)?;

    println!("\nBackup validated:");
    println!("  Format version: {}", metadata.format_version);
    println!("  Schema version: {}", metadata.schema_version);
    println!("  Created: {}", metadata.created_at);
    println!("  Size: {} bytes", metadata.file_size);
    println!("  Checksum: {} (verified)", metadata.checksum);

    if metadata.schema_version > CURRENT_VERSION && !force {
        println!(
            "\nWarning: Backup has newer schema version ({}) than current ({}).",
            metadata.schema_version, CURRENT_VERSION
        );
        println!("Use --force to restore anyway.");
        std::process::exit(1);
    }

    println!("\nRestoring database...");

    restore_backup(input, target_path, force)?;

    println!("Database restored from: {}", input.display());
    if target_path.with_extension("old").exists() {
        println!(
            "Previous database saved to: {}",
            target_path.with_extension("old").display()
        );
    }

    Ok(())
}

fn parse_provider(s: &str) -> Result<Provider> {
    match s.to_lowercase().as_str() {
        "chatgpt" => Ok(Provider::ChatGpt),
        "claude" => Ok(Provider::Claude),
        "gemini" => Ok(Provider::Gemini),
        "copilot" => Ok(Provider::Copilot),
        _ => Err(anyhow::anyhow!("Unknown provider: {}", s)),
    }
}
