// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
//! Provider adapter conformance tests
//!
//! This module provides conformance tests that any `ProviderTrait` implementation
//! must pass to be considered a valid provider adapter.

use chrono::Utc;
use llm_unify_core::{
    Conversation, Error, Message, MessageRole, Metadata, Provider, ProviderTrait, Result,
};

/// A mock provider for testing the conformance test infrastructure
struct MockProvider {
    name: &'static str,
    provider: Provider,
}

impl MockProvider {
    fn new(name: &'static str, provider: Provider) -> Self {
        Self { name, provider }
    }

    fn chatgpt() -> Self {
        Self::new("chatgpt", Provider::ChatGpt)
    }

    fn claude() -> Self {
        Self::new("claude", Provider::Claude)
    }

    fn gemini() -> Self {
        Self::new("gemini", Provider::Gemini)
    }
}

impl ProviderTrait for MockProvider {
    fn parse(&self, data: &[u8]) -> Result<Vec<Conversation>> {
        // Parse JSON array of conversations
        let conversations: Vec<serde_json::Value> =
            serde_json::from_slice(data).map_err(Error::Serialization)?;

        let mut result = Vec::new();
        for conv_data in conversations {
            let id = conv_data["id"]
                .as_str()
                .ok_or_else(|| Error::InvalidConversation("missing id".to_string()))?
                .to_string();

            let title = conv_data["title"]
                .as_str()
                .ok_or_else(|| Error::InvalidConversation("missing title".to_string()))?
                .to_string();

            let mut conversation = Conversation::new(id, title, self.provider);

            if let Some(messages) = conv_data["messages"].as_array() {
                for msg_data in messages {
                    let msg_id = msg_data["id"]
                        .as_str()
                        .ok_or_else(|| Error::InvalidMessage("missing id".to_string()))?
                        .to_string();

                    let content = msg_data["content"]
                        .as_str()
                        .ok_or_else(|| Error::InvalidMessage("missing content".to_string()))?
                        .to_string();

                    let role_str = msg_data["role"]
                        .as_str()
                        .ok_or_else(|| Error::InvalidMessage("missing role".to_string()))?;

                    let role = match role_str {
                        "user" => MessageRole::User,
                        "assistant" => MessageRole::Assistant,
                        "system" => MessageRole::System,
                        _ => {
                            return Err(Error::InvalidMessage(format!(
                                "invalid role: {}",
                                role_str
                            )))
                        }
                    };

                    let message = Message {
                        id: msg_id,
                        conversation_id: conversation.id.clone(),
                        role,
                        content,
                        timestamp: Utc::now(),
                        metadata: Metadata::new(),
                    };

                    conversation.add_message(message);
                }
            }

            result.push(conversation);
        }

        Ok(result)
    }

    fn name(&self) -> &'static str {
        self.name
    }

    fn validate(&self, conversation: &Conversation) -> Result<()> {
        // Validate conversation has required fields
        if conversation.id.is_empty() {
            return Err(Error::InvalidConversation(
                "conversation id cannot be empty".to_string(),
            ));
        }

        if conversation.title.is_empty() {
            return Err(Error::InvalidConversation(
                "conversation title cannot be empty".to_string(),
            ));
        }

        // Validate all messages
        for message in &conversation.messages {
            if message.id.is_empty() {
                return Err(Error::InvalidMessage("message id cannot be empty".to_string()));
            }
            if message.content.is_empty() {
                return Err(Error::InvalidMessage(
                    "message content cannot be empty".to_string(),
                ));
            }
            if message.conversation_id != conversation.id {
                return Err(Error::InvalidMessage(
                    "message conversation_id does not match".to_string(),
                ));
            }
        }

        Ok(())
    }
}

// =============================================================================
// Conformance Test Suite
// =============================================================================

mod provider_trait_conformance {
    use super::*;

    /// Test that provider name() returns a non-empty static string
    fn test_provider_name_not_empty<P: ProviderTrait>(provider: &P) {
        let name = provider.name();
        assert!(!name.is_empty(), "provider name must not be empty");
    }

    /// Test that provider name() returns consistent values
    fn test_provider_name_consistent<P: ProviderTrait>(provider: &P) {
        let name1 = provider.name();
        let name2 = provider.name();
        assert_eq!(name1, name2, "provider name must be consistent");
    }

    /// Test that parse() handles empty input correctly
    fn test_parse_empty_array<P: ProviderTrait>(provider: &P) {
        let data = b"[]";
        let result = provider.parse(data);
        assert!(result.is_ok(), "parsing empty array should succeed");
        assert!(
            result.unwrap().is_empty(),
            "parsing empty array should return empty vec"
        );
    }

    /// Test that parse() rejects invalid JSON
    fn test_parse_invalid_json<P: ProviderTrait>(provider: &P) {
        let data = b"not valid json";
        let result = provider.parse(data);
        assert!(result.is_err(), "parsing invalid JSON should fail");
    }

    /// Test that validate() accepts valid conversations
    fn test_validate_valid_conversation<P: ProviderTrait>(provider: &P) {
        let mut conversation = Conversation::new(
            "conv-123".to_string(),
            "Test Conversation".to_string(),
            Provider::ChatGpt,
        );

        let message = Message {
            id: "msg-1".to_string(),
            conversation_id: "conv-123".to_string(),
            role: MessageRole::User,
            content: "Hello!".to_string(),
            timestamp: Utc::now(),
            metadata: Metadata::new(),
        };
        conversation.add_message(message);

        let result = provider.validate(&conversation);
        assert!(result.is_ok(), "valid conversation should pass validation");
    }

    /// Test that validate() rejects conversations with empty id
    fn test_validate_empty_id<P: ProviderTrait>(provider: &P) {
        let conversation = Conversation::new(
            "".to_string(),
            "Test Conversation".to_string(),
            Provider::ChatGpt,
        );

        let result = provider.validate(&conversation);
        assert!(
            result.is_err(),
            "conversation with empty id should fail validation"
        );
    }

    /// Test that validate() rejects conversations with empty title
    fn test_validate_empty_title<P: ProviderTrait>(provider: &P) {
        let conversation =
            Conversation::new("conv-123".to_string(), "".to_string(), Provider::ChatGpt);

        let result = provider.validate(&conversation);
        assert!(
            result.is_err(),
            "conversation with empty title should fail validation"
        );
    }

    // Run all conformance tests for a provider
    fn run_conformance_suite<P: ProviderTrait>(provider: &P, provider_name: &str) {
        println!("Running conformance tests for: {}", provider_name);

        test_provider_name_not_empty(provider);
        test_provider_name_consistent(provider);
        test_parse_empty_array(provider);
        test_parse_invalid_json(provider);
        test_validate_valid_conversation(provider);
        test_validate_empty_id(provider);
        test_validate_empty_title(provider);

        println!("All conformance tests passed for: {}", provider_name);
    }

    #[test]
    fn chatgpt_provider_conformance() {
        let provider = MockProvider::chatgpt();
        run_conformance_suite(&provider, "chatgpt");
    }

    #[test]
    fn claude_provider_conformance() {
        let provider = MockProvider::claude();
        run_conformance_suite(&provider, "claude");
    }

    #[test]
    fn gemini_provider_conformance() {
        let provider = MockProvider::gemini();
        run_conformance_suite(&provider, "gemini");
    }
}

mod parse_conformance {
    use super::*;

    #[test]
    fn parse_single_conversation() {
        let provider = MockProvider::chatgpt();
        let data = r#"[{
            "id": "conv-1",
            "title": "Hello World",
            "messages": [
                {"id": "msg-1", "role": "user", "content": "Hello!"},
                {"id": "msg-2", "role": "assistant", "content": "Hi there!"}
            ]
        }]"#;

        let result = provider.parse(data.as_bytes());
        assert!(result.is_ok());

        let conversations = result.unwrap();
        assert_eq!(conversations.len(), 1);

        let conv = &conversations[0];
        assert_eq!(conv.id, "conv-1");
        assert_eq!(conv.title, "Hello World");
        assert_eq!(conv.provider, Provider::ChatGpt);
        assert_eq!(conv.messages.len(), 2);
    }

    #[test]
    fn parse_multiple_conversations() {
        let provider = MockProvider::claude();
        let data = r#"[
            {"id": "conv-1", "title": "First", "messages": []},
            {"id": "conv-2", "title": "Second", "messages": []},
            {"id": "conv-3", "title": "Third", "messages": []}
        ]"#;

        let result = provider.parse(data.as_bytes());
        assert!(result.is_ok());

        let conversations = result.unwrap();
        assert_eq!(conversations.len(), 3);
        assert_eq!(conversations[0].title, "First");
        assert_eq!(conversations[1].title, "Second");
        assert_eq!(conversations[2].title, "Third");
    }

    #[test]
    fn parse_preserves_message_roles() {
        let provider = MockProvider::gemini();
        let data = r#"[{
            "id": "conv-1",
            "title": "Roles Test",
            "messages": [
                {"id": "msg-1", "role": "system", "content": "You are helpful."},
                {"id": "msg-2", "role": "user", "content": "Hello!"},
                {"id": "msg-3", "role": "assistant", "content": "Hi!"}
            ]
        }]"#;

        let result = provider.parse(data.as_bytes());
        assert!(result.is_ok());

        let conversations = result.unwrap();
        let messages = &conversations[0].messages;

        assert_eq!(messages[0].role, MessageRole::System);
        assert_eq!(messages[1].role, MessageRole::User);
        assert_eq!(messages[2].role, MessageRole::Assistant);
    }

    #[test]
    fn parse_rejects_missing_id() {
        let provider = MockProvider::chatgpt();
        let data = r#"[{"title": "No ID", "messages": []}]"#;

        let result = provider.parse(data.as_bytes());
        assert!(result.is_err());
    }

    #[test]
    fn parse_rejects_missing_title() {
        let provider = MockProvider::chatgpt();
        let data = r#"[{"id": "conv-1", "messages": []}]"#;

        let result = provider.parse(data.as_bytes());
        assert!(result.is_err());
    }

    #[test]
    fn parse_rejects_invalid_role() {
        let provider = MockProvider::chatgpt();
        let data = r#"[{
            "id": "conv-1",
            "title": "Test",
            "messages": [{"id": "msg-1", "role": "invalid", "content": "test"}]
        }]"#;

        let result = provider.parse(data.as_bytes());
        assert!(result.is_err());
    }
}

mod validate_conformance {
    use super::*;

    fn create_valid_conversation() -> Conversation {
        let mut conv = Conversation::new(
            "conv-test".to_string(),
            "Test Title".to_string(),
            Provider::ChatGpt,
        );

        conv.add_message(Message {
            id: "msg-1".to_string(),
            conversation_id: "conv-test".to_string(),
            role: MessageRole::User,
            content: "Hello".to_string(),
            timestamp: Utc::now(),
            metadata: Metadata::new(),
        });

        conv
    }

    #[test]
    fn validate_accepts_conversation_without_messages() {
        let provider = MockProvider::chatgpt();
        let conv = Conversation::new(
            "conv-empty".to_string(),
            "Empty Conversation".to_string(),
            Provider::ChatGpt,
        );

        let result = provider.validate(&conv);
        assert!(result.is_ok(), "conversation without messages should be valid");
    }

    #[test]
    fn validate_accepts_conversation_with_messages() {
        let provider = MockProvider::claude();
        let conv = create_valid_conversation();

        let result = provider.validate(&conv);
        assert!(result.is_ok());
    }

    #[test]
    fn validate_rejects_message_with_empty_id() {
        let provider = MockProvider::gemini();
        let mut conv = Conversation::new(
            "conv-test".to_string(),
            "Test".to_string(),
            Provider::Gemini,
        );

        conv.add_message(Message {
            id: "".to_string(), // Empty ID
            conversation_id: "conv-test".to_string(),
            role: MessageRole::User,
            content: "Hello".to_string(),
            timestamp: Utc::now(),
            metadata: Metadata::new(),
        });

        let result = provider.validate(&conv);
        assert!(result.is_err(), "message with empty id should fail validation");
    }

    #[test]
    fn validate_rejects_message_with_empty_content() {
        let provider = MockProvider::chatgpt();
        let mut conv = Conversation::new(
            "conv-test".to_string(),
            "Test".to_string(),
            Provider::ChatGpt,
        );

        conv.add_message(Message {
            id: "msg-1".to_string(),
            conversation_id: "conv-test".to_string(),
            role: MessageRole::User,
            content: "".to_string(), // Empty content
            timestamp: Utc::now(),
            metadata: Metadata::new(),
        });

        let result = provider.validate(&conv);
        assert!(
            result.is_err(),
            "message with empty content should fail validation"
        );
    }

    #[test]
    fn validate_rejects_mismatched_conversation_id() {
        let provider = MockProvider::claude();
        let mut conv = Conversation::new(
            "conv-actual".to_string(),
            "Test".to_string(),
            Provider::Claude,
        );

        conv.add_message(Message {
            id: "msg-1".to_string(),
            conversation_id: "conv-different".to_string(), // Mismatched
            role: MessageRole::User,
            content: "Hello".to_string(),
            timestamp: Utc::now(),
            metadata: Metadata::new(),
        });

        let result = provider.validate(&conv);
        assert!(
            result.is_err(),
            "mismatched conversation_id should fail validation"
        );
    }

    #[test]
    fn validate_checks_all_messages() {
        let provider = MockProvider::chatgpt();
        let mut conv = Conversation::new(
            "conv-test".to_string(),
            "Test".to_string(),
            Provider::ChatGpt,
        );

        // First message is valid
        conv.add_message(Message {
            id: "msg-1".to_string(),
            conversation_id: "conv-test".to_string(),
            role: MessageRole::User,
            content: "Hello".to_string(),
            timestamp: Utc::now(),
            metadata: Metadata::new(),
        });

        // Second message has empty content
        conv.add_message(Message {
            id: "msg-2".to_string(),
            conversation_id: "conv-test".to_string(),
            role: MessageRole::Assistant,
            content: "".to_string(),
            timestamp: Utc::now(),
            metadata: Metadata::new(),
        });

        let result = provider.validate(&conv);
        assert!(result.is_err(), "should detect invalid message in list");
    }
}
