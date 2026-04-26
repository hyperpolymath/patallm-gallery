//! Claude export parser

use chrono::{DateTime, Utc};
use llm_unify_core::{Conversation, Message, MessageRole, Metadata, Provider, ProviderTrait};
use serde::Deserialize;

#[derive(Deserialize)]
struct ClaudeExport {
    #[serde(default)]
    conversations: Vec<ClaudeConversation>,
}

#[derive(Deserialize)]
struct ClaudeConversation {
    uuid: String,
    name: Option<String>,
    created_at: Option<String>,
    updated_at: Option<String>,
    #[serde(default)]
    chat_messages: Vec<ClaudeMessage>,
}

#[derive(Deserialize)]
struct ClaudeMessage {
    uuid: String,
    sender: String,
    created_at: Option<String>,
    text: String,
}

pub struct ClaudeParser;

impl ProviderTrait for ClaudeParser {
    fn parse(&self, data: &[u8]) -> llm_unify_core::Result<Vec<Conversation>> {
        let export: ClaudeExport = serde_json::from_slice(data)
            .map_err(|e| llm_unify_core::Error::Other(format!("Claude parse error: {}", e)))?;

        let mut conversations = Vec::new();

        for conv_data in export.conversations {
            let id = conv_data.uuid;
            let title = conv_data.name.unwrap_or_else(|| "Untitled".to_string());

            let created_at = conv_data
                .created_at
                .and_then(|s| DateTime::parse_from_rfc3339(&s).ok())
                .map(|dt| dt.with_timezone(&Utc))
                .unwrap_or_else(Utc::now);

            let updated_at = conv_data
                .updated_at
                .and_then(|s| DateTime::parse_from_rfc3339(&s).ok())
                .map(|dt| dt.with_timezone(&Utc))
                .unwrap_or_else(Utc::now);

            let mut conversation = Conversation {
                id: id.clone(),
                title,
                provider: Provider::Claude,
                created_at,
                updated_at,
                messages: Vec::new(),
                metadata: Metadata::new(),
            };

            for msg_data in conv_data.chat_messages {
                let role = match msg_data.sender.as_str() {
                    "human" => MessageRole::User,
                    "assistant" => MessageRole::Assistant,
                    "system" => MessageRole::System,
                    _ => continue,
                };

                if msg_data.text.is_empty() {
                    continue;
                }

                let timestamp = msg_data
                    .created_at
                    .and_then(|s| DateTime::parse_from_rfc3339(&s).ok())
                    .map(|dt| dt.with_timezone(&Utc))
                    .unwrap_or_else(Utc::now);

                conversation.messages.push(Message {
                    id: msg_data.uuid,
                    conversation_id: id.clone(),
                    role,
                    content: msg_data.text,
                    timestamp,
                    metadata: Metadata::new(),
                });
            }

            // Sort messages by timestamp
            conversation.messages.sort_by_key(|m| m.timestamp);

            if !conversation.messages.is_empty() {
                conversations.push(conversation);
            }
        }

        Ok(conversations)
    }

    fn name(&self) -> &'static str {
        "Claude"
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
    fn test_parse_claude_export() {
        let json = r#"{
            "conversations": [
                {
                    "uuid": "conv-001",
                    "name": "Test Conversation",
                    "created_at": "2025-01-15T10:00:00Z",
                    "updated_at": "2025-01-15T10:30:00Z",
                    "chat_messages": [
                        {
                            "uuid": "msg-001",
                            "sender": "human",
                            "created_at": "2025-01-15T10:00:00Z",
                            "text": "Hello, Claude!"
                        },
                        {
                            "uuid": "msg-002",
                            "sender": "assistant",
                            "created_at": "2025-01-15T10:00:30Z",
                            "text": "Hello! How can I help you today?"
                        }
                    ]
                }
            ]
        }"#;

        let parser = ClaudeParser;
        let result = parser.parse(json.as_bytes());
        assert!(result.is_ok());

        let conversations = result.unwrap();
        assert_eq!(conversations.len(), 1);

        let conv = &conversations[0];
        assert_eq!(conv.id, "conv-001");
        assert_eq!(conv.title, "Test Conversation");
        assert_eq!(conv.provider, Provider::Claude);
        assert_eq!(conv.messages.len(), 2);

        assert_eq!(conv.messages[0].role, MessageRole::User);
        assert_eq!(conv.messages[0].content, "Hello, Claude!");

        assert_eq!(conv.messages[1].role, MessageRole::Assistant);
        assert_eq!(conv.messages[1].content, "Hello! How can I help you today?");
    }

    #[test]
    fn test_parse_empty_export() {
        let json = r#"{"conversations": []}"#;
        let parser = ClaudeParser;
        let result = parser.parse(json.as_bytes());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 0);
    }

    #[test]
    fn test_parse_missing_optional_fields() {
        let json = r#"{
            "conversations": [
                {
                    "uuid": "conv-001",
                    "chat_messages": [
                        {
                            "uuid": "msg-001",
                            "sender": "human",
                            "text": "Hello"
                        }
                    ]
                }
            ]
        }"#;

        let parser = ClaudeParser;
        let result = parser.parse(json.as_bytes());
        assert!(result.is_ok());

        let conversations = result.unwrap();
        assert_eq!(conversations.len(), 1);
        assert_eq!(conversations[0].title, "Untitled");
    }

    #[test]
    fn test_validate_empty_id() {
        let parser = ClaudeParser;
        let conv = Conversation {
            id: "".to_string(),
            title: "Test".to_string(),
            provider: Provider::Claude,
            created_at: Utc::now(),
            updated_at: Utc::now(),
            messages: vec![],
            metadata: Metadata::new(),
        };
        assert!(parser.validate(&conv).is_err());
    }

    #[test]
    fn test_parser_name() {
        let parser = ClaudeParser;
        assert_eq!(parser.name(), "Claude");
    }
}
