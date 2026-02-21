//! Search engine implementation

use crate::Result;
use llm_unify_storage::Database;
use sqlx::Row;

/// Search result
#[derive(Debug, Clone)]
pub struct SearchResult {
    pub message_id: String,
    pub conversation_id: String,
    pub content: String,
    pub snippet: String,
    pub relevance: f32,
}

/// Search engine for full-text search
pub struct SearchEngine<'a> {
    db: &'a Database,
}

impl<'a> SearchEngine<'a> {
    pub fn new(db: &'a Database) -> Self {
        Self { db }
    }

    /// Search messages with full-text search
    pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>> {
        let rows = sqlx::query(
            r#"
            SELECT m.id as message_id, m.conversation_id, m.content,
                   snippet(messages_fts, 0, '<mark>', '</mark>', '...', 64) as snippet
            FROM messages_fts
            JOIN messages m ON messages_fts.rowid = m.rowid
            WHERE messages_fts MATCH ?
            ORDER BY rank
            LIMIT 100
            "#,
        )
        .bind(query)
        .fetch_all(self.db.pool())
        .await?;

        let mut results = Vec::new();
        for row in rows {
            let message_id: String = row.get("message_id");
            let conversation_id: String = row.get("conversation_id");
            let content: String = row.get("content");
            let snippet: String = row.get("snippet");

            results.push(SearchResult {
                message_id,
                conversation_id,
                content,
                snippet,
                relevance: 1.0, // SQLite FTS5 doesn't provide direct relevance scores
            });
        }

        Ok(results)
    }

    /// Search messages by conversation ID
    pub async fn search_in_conversation(
        &self,
        conversation_id: &str,
        query: &str,
    ) -> Result<Vec<SearchResult>> {
        let rows = sqlx::query(
            r#"
            SELECT m.id as message_id, m.conversation_id, m.content,
                   snippet(messages_fts, 0, '<mark>', '</mark>', '...', 64) as snippet
            FROM messages_fts
            JOIN messages m ON messages_fts.rowid = m.rowid
            WHERE messages_fts MATCH ? AND m.conversation_id = ?
            ORDER BY rank
            LIMIT 100
            "#,
        )
        .bind(query)
        .bind(conversation_id)
        .fetch_all(self.db.pool())
        .await?;

        let mut results = Vec::new();
        for row in rows {
            let message_id: String = row.get("message_id");
            let conversation_id: String = row.get("conversation_id");
            let content: String = row.get("content");
            let snippet: String = row.get("snippet");

            results.push(SearchResult {
                message_id,
                conversation_id,
                content,
                snippet,
                relevance: 1.0,
            });
        }

        Ok(results)
    }
}
