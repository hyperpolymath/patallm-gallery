//! ChatGPT export parser

use chrono::{DateTime, Utc};
use llm_unify_core::{Conversation, Message, MessageRole, Metadata, Provider, ProviderTrait};
use serde::Deserialize;

#[derive(Deserialize)]
struct ChatGptExport {
    #[serde(default)]
    conversations: Vec<ChatGptConversation>,
}

#[derive(Deserialize)]
struct ChatGptConversation {
    id: String,
    title: Option<String>,
    create_time: Option<f64>,
    update_time: Option<f64>,
    #[serde(default)]
    mapping: serde_json::Value,
}

#[derive(Deserialize)]
struct ChatGptMessage {
    id: String,
    author: ChatGptAuthor,
    create_time: Option<f64>,
    content: ChatGptContent,
}

#[derive(Deserialize)]
struct ChatGptAuthor {
    role: String,
}

#[derive(Deserialize)]
struct ChatGptContent {
    #[allow(dead_code)] // Part of ChatGPT JSON schema, may be used for format detection
    content_type: String,
    parts: Option<Vec<String>>,
}

/// ChatGPT parser implementation
pub struct ChatGptParser;

impl ProviderTrait for ChatGptParser {
    fn parse(&self, data: &[u8]) -> llm_unify_core::Result<Vec<Conversation>> {
        let export: ChatGptExport = serde_json::from_slice(data)
            .map_err(|e| llm_unify_core::Error::Other(format!("ChatGPT parse error: {}", e)))?;

        let mut conversations = Vec::new();

        for conv_data in export.conversations {
            let id = conv_data.id;
            let title = conv_data.title.unwrap_or_else(|| "Untitled".to_string());

            let created_at = conv_data
                .create_time
                .and_then(|t| DateTime::from_timestamp(t as i64, 0))
                .unwrap_or_else(Utc::now);

            let updated_at = conv_data
                .update_time
                .and_then(|t| DateTime::from_timestamp(t as i64, 0))
                .unwrap_or_else(Utc::now);

            let mut conversation = Conversation {
                id: id.clone(),
                title,
                provider: Provider::ChatGpt,
                created_at,
                updated_at,
                messages: Vec::new(),
                metadata: Metadata::new(),
            };

            // Parse messages from mapping
            if let Some(mapping) = conv_data.mapping.as_object() {
                for (_, node) in mapping {
                    if let Some(message_data) = node.get("message") {
                        if let Ok(msg) = serde_json::from_value::<ChatGptMessage>(message_data.clone()) {
                            let role = match msg.author.role.as_str() {
                                "user" => MessageRole::User,
                                "assistant" => MessageRole::Assistant,
                                "system" => MessageRole::System,
                                _ => continue,
                            };

                            let content = msg
                                .content
                                .parts
                                .and_then(|parts| parts.first().cloned())
                                .unwrap_or_default();

                            if content.is_empty() {
                                continue;
                            }

                            let timestamp = msg
                                .create_time
                                .and_then(|t| DateTime::from_timestamp(t as i64, 0))
                                .unwrap_or_else(Utc::now);

                            conversation.messages.push(Message {
                                id: msg.id,
                                conversation_id: id.clone(),
                                role,
                                content,
                                timestamp,
                                metadata: Metadata::new(),
                            });
                        }
                    }
                }
            }

            // Sort messages by timestamp
            conversation
                .messages
                .sort_by_key(|m| m.timestamp);

            if !conversation.messages.is_empty() {
                conversations.push(conversation);
            }
        }

        Ok(conversations)
    }

    fn name(&self) -> &'static str {
        "ChatGPT"
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
