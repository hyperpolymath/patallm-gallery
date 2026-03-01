//! Repository implementations for data access

use crate::{Database, Error, Result};
use llm_unify_core::{Conversation, Message, Metadata, MessageRole, Provider};
use sqlx::Row;

/// Conversation repository
pub struct ConversationRepository<'a> {
    db: &'a Database,
}

impl<'a> ConversationRepository<'a> {
    pub fn new(db: &'a Database) -> Self {
        Self { db }
    }

    /// Save a conversation
    pub async fn save(&self, conversation: &Conversation) -> Result<()> {
        let metadata_json = serde_json::to_string(&conversation.metadata)?;

        sqlx::query(
            r#"
            INSERT OR REPLACE INTO conversations (id, title, provider, created_at, updated_at, metadata)
            VALUES (?, ?, ?, ?, ?, ?)
            "#,
        )
        .bind(&conversation.id)
        .bind(&conversation.title)
        .bind(conversation.provider.as_str())
        .bind(conversation.created_at.to_rfc3339())
        .bind(conversation.updated_at.to_rfc3339())
        .bind(&metadata_json)
        .execute(self.db.pool())
        .await?;

        let msg_repo = MessageRepository::new(self.db);
        for message in &conversation.messages {
            msg_repo.save(message).await?;
        }

        Ok(())
    }

    /// Find a conversation by ID
    pub async fn find_by_id(&self, id: &str) -> Result<Option<Conversation>> {
        let row = sqlx::query(
            r#"
            SELECT id, title, provider, created_at, updated_at, metadata
            FROM conversations
            WHERE id = ?
            "#,
        )
        .bind(id)
        .fetch_optional(self.db.pool())
        .await?;

        match row {
            Some(row) => {
                let id: String = row.get("id");
                let title: String = row.get("title");
                let provider_str: String = row.get("provider");
                let created_at: String = row.get("created_at");
                let updated_at: String = row.get("updated_at");
                let metadata_json: String = row.get("metadata");

                let provider = match provider_str.as_str() {
                    "chatgpt" => Provider::ChatGpt,
                    "claude" => Provider::Claude,
                    "gemini" => Provider::Gemini,
                    "copilot" => Provider::Copilot,
                    _ => Provider::Other,
                };

                let metadata: Metadata = serde_json::from_str(&metadata_json)?;

                let msg_repo = MessageRepository::new(self.db);
                let messages = msg_repo.find_by_conversation(&id).await?;

                Ok(Some(Conversation {
                    id,
                    title,
                    provider,
                    created_at: created_at.parse().map_err(|e| Error::Other(format!("{}", e)))?,
                    updated_at: updated_at.parse().map_err(|e| Error::Other(format!("{}", e)))?,
                    messages,
                    metadata,
                }))
            }
            None => Ok(None),
        }
    }

    /// List all conversations
    pub async fn list(&self) -> Result<Vec<Conversation>> {
        let rows = sqlx::query(
            r#"
            SELECT id, title, provider, created_at, updated_at, metadata
            FROM conversations
            ORDER BY updated_at DESC
            "#,
        )
        .fetch_all(self.db.pool())
        .await?;

        let mut conversations = Vec::new();
        for row in rows {
            let id: String = row.get("id");
            if let Some(conv) = self.find_by_id(&id).await? {
                conversations.push(conv);
            }
        }

        Ok(conversations)
    }

    /// Delete a conversation
    pub async fn delete(&self, id: &str) -> Result<()> {
        sqlx::query("DELETE FROM conversations WHERE id = ?")
            .bind(id)
            .execute(self.db.pool())
            .await?;
        Ok(())
    }
}

/// Message repository
pub struct MessageRepository<'a> {
    db: &'a Database,
}

impl<'a> MessageRepository<'a> {
    pub fn new(db: &'a Database) -> Self {
        Self { db }
    }

    /// Save a message
    pub async fn save(&self, message: &Message) -> Result<()> {
        let metadata_json = serde_json::to_string(&message.metadata)?;

        sqlx::query(
            r#"
            INSERT OR REPLACE INTO messages (id, conversation_id, role, content, timestamp, metadata)
            VALUES (?, ?, ?, ?, ?, ?)
            "#,
        )
        .bind(&message.id)
        .bind(&message.conversation_id)
        .bind(message.role.to_string())
        .bind(&message.content)
        .bind(message.timestamp.to_rfc3339())
        .bind(&metadata_json)
        .execute(self.db.pool())
        .await?;

        Ok(())
    }

    /// Find messages by conversation ID
    pub async fn find_by_conversation(&self, conversation_id: &str) -> Result<Vec<Message>> {
        let rows = sqlx::query(
            r#"
            SELECT id, conversation_id, role, content, timestamp, metadata
            FROM messages
            WHERE conversation_id = ?
            ORDER BY timestamp ASC
            "#,
        )
        .bind(conversation_id)
        .fetch_all(self.db.pool())
        .await?;

        let mut messages = Vec::new();
        for row in rows {
            let id: String = row.get("id");
            let conversation_id: String = row.get("conversation_id");
            let role_str: String = row.get("role");
            let content: String = row.get("content");
            let timestamp: String = row.get("timestamp");
            let metadata_json: String = row.get("metadata");

            let role = match role_str.as_str() {
                "user" => MessageRole::User,
                "assistant" => MessageRole::Assistant,
                "system" => MessageRole::System,
                _ => MessageRole::User,
            };

            let metadata: Metadata = serde_json::from_str(&metadata_json)?;

            messages.push(Message {
                id,
                conversation_id,
                role,
                content,
                timestamp: timestamp.parse().map_err(|e| Error::Other(format!("{}", e)))?,
                metadata,
            });
        }

        Ok(messages)
    }
}
