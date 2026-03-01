//! Gemini export parser
//!
//! Supports two JSON export formats:
//! 1. Simple format (GeoAnima/Gemini-Conversation-Downloader):
//!    { title, url, messages: [{ role, content }] }
//! 2. Extended format (revivalstack/ai-chat-exporter):
//!    { title, tags, author, count, exporter, date, url, messages: [{ id, author, content }] }

use chrono::{DateTime, Utc};
use llm_unify_core::{Conversation, Message, MessageRole, Metadata, Provider, ProviderTrait};
use serde::Deserialize;
use uuid::Uuid;

/// Simple format from Gemini-Conversation-Downloader
#[derive(Deserialize)]
struct SimpleGeminiExport {
    title: Option<String>,
    #[allow(dead_code)]
    url: Option<String>,
    messages: Vec<SimpleGeminiMessage>,
}

#[derive(Deserialize)]
struct SimpleGeminiMessage {
    role: String,
    content: String,
}

/// Extended format from ai-chat-exporter
#[derive(Deserialize)]
struct ExtendedGeminiExport {
    title: Option<String>,
    #[serde(default)]
    tags: Vec<String>,
    #[allow(dead_code)]
    author: Option<String>,
    #[allow(dead_code)]
    count: Option<i32>,
    #[allow(dead_code)]
    exporter: Option<String>,
    date: Option<String>,
    #[allow(dead_code)]
    url: Option<String>,
    messages: Vec<ExtendedGeminiMessage>,
}

#[derive(Deserialize)]
struct ExtendedGeminiMessage {
    id: Option<String>,
    author: String,
    content: String,
}

/// Array wrapper for multiple conversations
#[derive(Deserialize)]
#[serde(untagged)]
enum GeminiExportData {
    Single(ExtendedGeminiExport),
    Multiple(Vec<ExtendedGeminiExport>),
    SimpleSingle(SimpleGeminiExport),
    SimpleMultiple(Vec<SimpleGeminiExport>),
}

pub struct GeminiParser;

impl GeminiParser {
    fn parse_simple(&self, export: SimpleGeminiExport) -> Option<Conversation> {
        let id = Uuid::new_v4().to_string();
        let now = Utc::now();

        let mut conversation = Conversation {
            id: id.clone(),
            title: export.title.unwrap_or_else(|| "Gemini Conversation".to_string()),
            provider: Provider::Gemini,
            created_at: now,
            updated_at: now,
            messages: Vec::new(),
            metadata: Metadata::new(),
        };

        for msg in export.messages {
            let role = match msg.role.to_lowercase().as_str() {
                "user" => MessageRole::User,
                "assistant" | "model" | "ai" => MessageRole::Assistant,
                "system" => MessageRole::System,
                _ => continue,
            };

            if msg.content.is_empty() {
                continue;
            }

            conversation.messages.push(Message {
                id: Uuid::new_v4().to_string(),
                conversation_id: id.clone(),
                role,
                content: msg.content,
                timestamp: now,
                metadata: Metadata::new(),
            });
        }

        if conversation.messages.is_empty() {
            None
        } else {
            Some(conversation)
        }
    }

    fn parse_extended(&self, export: ExtendedGeminiExport) -> Option<Conversation> {
        let id = Uuid::new_v4().to_string();

        let timestamp = export
            .date
            .as_ref()
            .and_then(|d| DateTime::parse_from_rfc3339(d).ok())
            .map(|dt| dt.with_timezone(&Utc))
            .unwrap_or_else(Utc::now);

        let mut metadata = Metadata::new();
        if !export.tags.is_empty() {
            metadata.insert("tags".to_string(), export.tags.join(", ").into());
        }

        let mut conversation = Conversation {
            id: id.clone(),
            title: export.title.unwrap_or_else(|| "Gemini Conversation".to_string()),
            provider: Provider::Gemini,
            created_at: timestamp,
            updated_at: timestamp,
            messages: Vec::new(),
            metadata,
        };

        for msg in export.messages {
            let role = match msg.author.to_lowercase().as_str() {
                "user" => MessageRole::User,
                "assistant" | "model" | "ai" | "gemini" => MessageRole::Assistant,
                "system" => MessageRole::System,
                _ => continue,
            };

            if msg.content.is_empty() {
                continue;
            }

            let msg_id = msg.id.unwrap_or_else(|| Uuid::new_v4().to_string());

            conversation.messages.push(Message {
                id: msg_id,
                conversation_id: id.clone(),
                role,
                content: msg.content,
                timestamp,
                metadata: Metadata::new(),
            });
        }

        if conversation.messages.is_empty() {
            None
        } else {
            Some(conversation)
        }
    }
}

impl ProviderTrait for GeminiParser {
    fn parse(&self, data: &[u8]) -> llm_unify_core::Result<Vec<Conversation>> {
        // Try extended format first
        if let Ok(export_data) = serde_json::from_slice::<GeminiExportData>(data) {
            let conversations: Vec<Conversation> = match export_data {
                GeminiExportData::Single(export) => self.parse_extended(export).into_iter().collect(),
                GeminiExportData::Multiple(exports) => exports
                    .into_iter()
                    .filter_map(|e| self.parse_extended(e))
                    .collect(),
                GeminiExportData::SimpleSingle(export) => self.parse_simple(export).into_iter().collect(),
                GeminiExportData::SimpleMultiple(exports) => exports
                    .into_iter()
                    .filter_map(|e| self.parse_simple(e))
                    .collect(),
            };
            return Ok(conversations);
        }

        Err(llm_unify_core::Error::Other(
            "Failed to parse Gemini export: unrecognized format".to_string(),
        ))
    }

    fn name(&self) -> &'static str {
        "Gemini"
    }

    fn validate(&self, conversation: &Conversation) -> llm_unify_core::Result<()> {
        if conversation.id.is_empty() {
            return Err(llm_unify_core::Error::InvalidConversation(
                "Empty conversation ID".to_string(),
            ));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_extended_format() {
        let json = r#"{
            "title": "Test Conversation",
            "tags": ["test", "gemini"],
            "author": "gemini",
            "date": "2025-01-15T10:00:00Z",
            "messages": [
                {
                    "id": "msg-001",
                    "author": "user",
                    "content": "Hello Gemini!"
                },
                {
                    "id": "msg-002",
                    "author": "ai",
                    "content": "Hello! How can I assist you?"
                }
            ]
        }"#;

        let parser = GeminiParser;
        let result = parser.parse(json.as_bytes());
        assert!(result.is_ok());

        let conversations = result.unwrap();
        assert_eq!(conversations.len(), 1);

        let conv = &conversations[0];
        assert_eq!(conv.title, "Test Conversation");
        assert_eq!(conv.provider, Provider::Gemini);
        assert_eq!(conv.messages.len(), 2);

        assert_eq!(conv.messages[0].role, MessageRole::User);
        assert_eq!(conv.messages[1].role, MessageRole::Assistant);
    }

    #[test]
    fn test_parse_simple_format() {
        let json = r#"{
            "title": "Simple Chat",
            "url": "https://gemini.google.com/share/test",
            "messages": [
                {
                    "role": "user",
                    "content": "What is 2+2?"
                },
                {
                    "role": "assistant",
                    "content": "2+2 equals 4."
                }
            ]
        }"#;

        let parser = GeminiParser;
        let result = parser.parse(json.as_bytes());
        assert!(result.is_ok());

        let conversations = result.unwrap();
        assert_eq!(conversations.len(), 1);
        assert_eq!(conversations[0].messages.len(), 2);
    }

    #[test]
    fn test_parse_multiple_conversations() {
        let json = r#"[
            {
                "title": "Chat 1",
                "messages": [
                    { "id": "1", "author": "user", "content": "Hello" }
                ]
            },
            {
                "title": "Chat 2",
                "messages": [
                    { "id": "2", "author": "user", "content": "Hi" }
                ]
            }
        ]"#;

        let parser = GeminiParser;
        let result = parser.parse(json.as_bytes());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 2);
    }

    #[test]
    fn test_parse_model_role() {
        let json = r#"{
            "title": "Test",
            "messages": [
                { "id": "1", "author": "model", "content": "I am a model response" }
            ]
        }"#;

        let parser = GeminiParser;
        let result = parser.parse(json.as_bytes());
        assert!(result.is_ok());

        let conversations = result.unwrap();
        assert_eq!(conversations[0].messages[0].role, MessageRole::Assistant);
    }

    #[test]
    fn test_parser_name() {
        let parser = GeminiParser;
        assert_eq!(parser.name(), "Gemini");
    }

    #[test]
    fn test_invalid_json() {
        let json = b"not valid json";
        let parser = GeminiParser;
        let result = parser.parse(json);
        assert!(result.is_err());
    }
}
