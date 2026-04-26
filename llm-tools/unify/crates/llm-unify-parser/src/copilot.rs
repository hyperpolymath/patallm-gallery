//! GitHub Copilot export parser
//!
//! Supports VS Code "Chat: Export Chat..." JSON format
//! Structure: { requesterUsername, responderUsername, requests: [{ message, response, result }] }

use chrono::Utc;
use llm_unify_core::{Conversation, Message, MessageRole, Metadata, Provider, ProviderTrait};
use serde::Deserialize;
use serde_json::Value;
use uuid::Uuid;

/// VS Code Copilot Chat export format
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct CopilotExport {
    #[allow(dead_code)]
    requester_username: Option<String>,
    #[allow(dead_code)]
    responder_username: Option<String>,
    #[allow(dead_code)]
    initial_location: Option<String>,
    requests: Vec<CopilotRequest>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct CopilotRequest {
    request_id: Option<String>,
    message: CopilotMessage,
    response: Vec<Value>,
    #[allow(dead_code)]
    result: Option<CopilotResult>,
}

#[derive(Deserialize)]
struct CopilotMessage {
    text: String,
    #[allow(dead_code)]
    parts: Option<Vec<Value>>,
}

#[derive(Deserialize)]
struct CopilotResult {
    #[allow(dead_code)]
    timings: Option<CopilotTimings>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct CopilotTimings {
    #[allow(dead_code)]
    total_elapsed: Option<u64>,
}

/// Alternative format from ai-chat-exporter
#[derive(Deserialize)]
struct SimpleCopilotExport {
    title: Option<String>,
    #[allow(dead_code)]
    author: Option<String>,
    #[allow(dead_code)]
    date: Option<String>,
    #[allow(dead_code)]
    url: Option<String>,
    messages: Vec<SimpleCopilotMessage>,
}

#[derive(Deserialize)]
struct SimpleCopilotMessage {
    id: Option<String>,
    author: String,
    content: String,
}

#[derive(Deserialize)]
#[serde(untagged)]
enum CopilotExportData {
    VsCode(CopilotExport),
    VsCodeMultiple(Vec<CopilotExport>),
    Simple(SimpleCopilotExport),
    SimpleMultiple(Vec<SimpleCopilotExport>),
}

pub struct CopilotParser;

impl CopilotParser {
    fn extract_response_text(response: &[Value]) -> String {
        let mut texts = Vec::new();

        for item in response {
            // Handle direct text content
            if let Some(text) = item.as_str() {
                texts.push(text.to_string());
            }
            // Handle object with value field (progress/text items)
            else if let Some(obj) = item.as_object() {
                if let Some(value) = obj.get("value") {
                    if let Some(text) = value.as_str() {
                        texts.push(text.to_string());
                    }
                }
                // Handle tool result content
                if let Some(content) = obj.get("content") {
                    if let Some(text) = content.as_str() {
                        texts.push(text.to_string());
                    }
                }
            }
        }

        texts.join("")
    }

    fn parse_vscode(&self, export: CopilotExport) -> Option<Conversation> {
        let conv_id = Uuid::new_v4().to_string();
        let now = Utc::now();

        let mut conversation = Conversation {
            id: conv_id.clone(),
            title: "Copilot Conversation".to_string(),
            provider: Provider::Copilot,
            created_at: now,
            updated_at: now,
            messages: Vec::new(),
            metadata: Metadata::new(),
        };

        for request in export.requests {
            // User message
            if !request.message.text.is_empty() {
                let msg_id = request
                    .request_id
                    .clone()
                    .unwrap_or_else(|| Uuid::new_v4().to_string());

                conversation.messages.push(Message {
                    id: format!("{}-user", msg_id),
                    conversation_id: conv_id.clone(),
                    role: MessageRole::User,
                    content: request.message.text,
                    timestamp: now,
                    metadata: Metadata::new(),
                });
            }

            // Assistant response
            let response_text = Self::extract_response_text(&request.response);
            if !response_text.is_empty() {
                let msg_id = request
                    .request_id
                    .unwrap_or_else(|| Uuid::new_v4().to_string());

                conversation.messages.push(Message {
                    id: format!("{}-assistant", msg_id),
                    conversation_id: conv_id.clone(),
                    role: MessageRole::Assistant,
                    content: response_text,
                    timestamp: now,
                    metadata: Metadata::new(),
                });
            }
        }

        // Update title from first user message
        if let Some(first_msg) = conversation.messages.first() {
            let title = first_msg.content.chars().take(50).collect::<String>();
            conversation.title = if title.len() < first_msg.content.len() {
                format!("{}...", title)
            } else {
                title
            };
        }

        if conversation.messages.is_empty() {
            None
        } else {
            Some(conversation)
        }
    }

    fn parse_simple(&self, export: SimpleCopilotExport) -> Option<Conversation> {
        let conv_id = Uuid::new_v4().to_string();
        let now = Utc::now();

        let mut conversation = Conversation {
            id: conv_id.clone(),
            title: export
                .title
                .unwrap_or_else(|| "Copilot Conversation".to_string()),
            provider: Provider::Copilot,
            created_at: now,
            updated_at: now,
            messages: Vec::new(),
            metadata: Metadata::new(),
        };

        for msg in export.messages {
            let role = match msg.author.to_lowercase().as_str() {
                "user" => MessageRole::User,
                "ai" | "assistant" | "copilot" => MessageRole::Assistant,
                "system" => MessageRole::System,
                _ => continue,
            };

            if msg.content.is_empty() {
                continue;
            }

            let msg_id = msg.id.unwrap_or_else(|| Uuid::new_v4().to_string());

            conversation.messages.push(Message {
                id: msg_id,
                conversation_id: conv_id.clone(),
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
}

impl ProviderTrait for CopilotParser {
    fn parse(&self, data: &[u8]) -> llm_unify_core::Result<Vec<Conversation>> {
        if let Ok(export_data) = serde_json::from_slice::<CopilotExportData>(data) {
            let conversations: Vec<Conversation> = match export_data {
                CopilotExportData::VsCode(export) => {
                    self.parse_vscode(export).into_iter().collect()
                }
                CopilotExportData::VsCodeMultiple(exports) => exports
                    .into_iter()
                    .filter_map(|e| self.parse_vscode(e))
                    .collect(),
                CopilotExportData::Simple(export) => {
                    self.parse_simple(export).into_iter().collect()
                }
                CopilotExportData::SimpleMultiple(exports) => exports
                    .into_iter()
                    .filter_map(|e| self.parse_simple(e))
                    .collect(),
            };
            return Ok(conversations);
        }

        Err(llm_unify_core::Error::Other(
            "Failed to parse Copilot export: unrecognized format".to_string(),
        ))
    }

    fn name(&self) -> &'static str {
        "Copilot"
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
    fn test_parse_vscode_format() {
        let json = r#"{
            "requesterUsername": "Developer",
            "responderUsername": "GitHub Copilot",
            "requests": [
                {
                    "requestId": "req-001",
                    "message": {
                        "text": "How do I read a file?"
                    },
                    "response": [
                        { "value": "You can use fs.readFile() in Node.js" }
                    ]
                }
            ]
        }"#;

        let parser = CopilotParser;
        let result = parser.parse(json.as_bytes());
        assert!(result.is_ok());

        let conversations = result.unwrap();
        assert_eq!(conversations.len(), 1);

        let conv = &conversations[0];
        assert_eq!(conv.provider, Provider::Copilot);
        assert_eq!(conv.messages.len(), 2);

        assert_eq!(conv.messages[0].role, MessageRole::User);
        assert_eq!(conv.messages[0].content, "How do I read a file?");

        assert_eq!(conv.messages[1].role, MessageRole::Assistant);
        assert!(conv.messages[1].content.contains("fs.readFile"));
    }

    #[test]
    fn test_parse_simple_format() {
        let json = r#"{
            "title": "Coding Help",
            "messages": [
                { "id": "1", "author": "user", "content": "What is async?" },
                { "id": "2", "author": "ai", "content": "Async is..." }
            ]
        }"#;

        let parser = CopilotParser;
        let result = parser.parse(json.as_bytes());
        assert!(result.is_ok());

        let conversations = result.unwrap();
        assert_eq!(conversations.len(), 1);
        assert_eq!(conversations[0].messages.len(), 2);
    }

    #[test]
    fn test_parse_multiple_requests() {
        let json = r#"{
            "requesterUsername": "Dev",
            "responderUsername": "Copilot",
            "requests": [
                {
                    "requestId": "1",
                    "message": { "text": "Question 1" },
                    "response": [{ "value": "Answer 1" }]
                },
                {
                    "requestId": "2",
                    "message": { "text": "Question 2" },
                    "response": [{ "value": "Answer 2" }]
                }
            ]
        }"#;

        let parser = CopilotParser;
        let result = parser.parse(json.as_bytes());
        assert!(result.is_ok());

        let conversations = result.unwrap();
        assert_eq!(conversations[0].messages.len(), 4);
    }

    #[test]
    fn test_empty_response() {
        let json = r#"{
            "requesterUsername": "Dev",
            "responderUsername": "Copilot",
            "requests": [
                {
                    "requestId": "1",
                    "message": { "text": "Hello" },
                    "response": []
                }
            ]
        }"#;

        let parser = CopilotParser;
        let result = parser.parse(json.as_bytes());
        assert!(result.is_ok());

        let conversations = result.unwrap();
        // Only user message, no assistant response
        assert_eq!(conversations[0].messages.len(), 1);
    }

    #[test]
    fn test_parser_name() {
        let parser = CopilotParser;
        assert_eq!(parser.name(), "Copilot");
    }

    #[test]
    fn test_invalid_json() {
        let json = b"not valid json";
        let parser = CopilotParser;
        let result = parser.parse(json);
        assert!(result.is_err());
    }
}
