//! Core domain models

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// LLM Provider identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Provider {
    ChatGpt,
    Claude,
    Gemini,
    Copilot,
    Other,
}

impl Provider {
    pub fn as_str(&self) -> &'static str {
        match self {
            Provider::ChatGpt => "chatgpt",
            Provider::Claude => "claude",
            Provider::Gemini => "gemini",
            Provider::Copilot => "copilot",
            Provider::Other => "other",
        }
    }
}

impl std::fmt::Display for Provider {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Message role in a conversation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum MessageRole {
    User,
    Assistant,
    System,
}

impl std::fmt::Display for MessageRole {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MessageRole::User => write!(f, "user"),
            MessageRole::Assistant => write!(f, "assistant"),
            MessageRole::System => write!(f, "system"),
        }
    }
}

/// A single message in a conversation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    pub id: String,
    pub conversation_id: String,
    pub role: MessageRole,
    pub content: String,
    pub timestamp: DateTime<Utc>,
    pub metadata: Metadata,
}

/// A conversation containing multiple messages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Conversation {
    pub id: String,
    pub title: String,
    pub provider: Provider,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub messages: Vec<Message>,
    pub metadata: Metadata,
}

impl Conversation {
    /// Create a new conversation
    pub fn new(id: String, title: String, provider: Provider) -> Self {
        let now = Utc::now();
        Self {
            id,
            title,
            provider,
            created_at: now,
            updated_at: now,
            messages: Vec::new(),
            metadata: Metadata::default(),
        }
    }

    /// Add a message to the conversation
    pub fn add_message(&mut self, message: Message) {
        self.updated_at = Utc::now();
        self.messages.push(message);
    }

    /// Get message count
    pub fn message_count(&self) -> usize {
        self.messages.len()
    }
}

/// Flexible metadata storage
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Metadata {
    #[serde(flatten)]
    pub fields: HashMap<String, serde_json::Value>,
}

impl Metadata {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, key: String, value: serde_json::Value) {
        self.fields.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&serde_json::Value> {
        self.fields.get(key)
    }
}
