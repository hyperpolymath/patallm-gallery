// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
//! Model serialization conformance tests
//!
//! These tests verify that all core types serialize and deserialize correctly,
//! maintaining data integrity across JSON round-trips.

use chrono::{DateTime, Utc};
use llm_unify_core::{Conversation, Message, MessageRole, Metadata, Provider};

mod provider_enum {
    use super::*;

    #[test]
    fn serialize_all_providers() {
        let providers = vec![
            (Provider::ChatGpt, "\"chatgpt\""),
            (Provider::Claude, "\"claude\""),
            (Provider::Gemini, "\"gemini\""),
            (Provider::Copilot, "\"copilot\""),
            (Provider::Other, "\"other\""),
        ];

        for (provider, expected) in providers {
            let json = serde_json::to_string(&provider).unwrap();
            assert_eq!(json, expected, "Provider {:?} should serialize correctly", provider);
        }
    }

    #[test]
    fn deserialize_all_providers() {
        let cases = vec![
            ("\"chatgpt\"", Provider::ChatGpt),
            ("\"claude\"", Provider::Claude),
            ("\"gemini\"", Provider::Gemini),
            ("\"copilot\"", Provider::Copilot),
            ("\"other\"", Provider::Other),
        ];

        for (json, expected) in cases {
            let provider: Provider = serde_json::from_str(json).unwrap();
            assert_eq!(provider, expected, "Should deserialize {} correctly", json);
        }
    }

    #[test]
    fn provider_roundtrip() {
        let providers = vec![
            Provider::ChatGpt,
            Provider::Claude,
            Provider::Gemini,
            Provider::Copilot,
            Provider::Other,
        ];

        for provider in providers {
            let json = serde_json::to_string(&provider).unwrap();
            let restored: Provider = serde_json::from_str(&json).unwrap();
            assert_eq!(provider, restored, "Provider should survive roundtrip");
        }
    }

    #[test]
    fn provider_as_str() {
        assert_eq!(Provider::ChatGpt.as_str(), "chatgpt");
        assert_eq!(Provider::Claude.as_str(), "claude");
        assert_eq!(Provider::Gemini.as_str(), "gemini");
        assert_eq!(Provider::Copilot.as_str(), "copilot");
        assert_eq!(Provider::Other.as_str(), "other");
    }

    #[test]
    fn provider_display() {
        assert_eq!(format!("{}", Provider::ChatGpt), "chatgpt");
        assert_eq!(format!("{}", Provider::Claude), "claude");
        assert_eq!(format!("{}", Provider::Gemini), "gemini");
        assert_eq!(format!("{}", Provider::Copilot), "copilot");
        assert_eq!(format!("{}", Provider::Other), "other");
    }

    #[test]
    fn provider_equality() {
        assert_eq!(Provider::ChatGpt, Provider::ChatGpt);
        assert_ne!(Provider::ChatGpt, Provider::Claude);
    }

    #[test]
    fn provider_clone() {
        let provider = Provider::Claude;
        let cloned = provider;
        assert_eq!(provider, cloned);
    }

    #[test]
    fn provider_hash() {
        use std::collections::HashSet;

        let mut set = HashSet::new();
        set.insert(Provider::ChatGpt);
        set.insert(Provider::Claude);
        set.insert(Provider::ChatGpt); // Duplicate

        assert_eq!(set.len(), 2);
        assert!(set.contains(&Provider::ChatGpt));
        assert!(set.contains(&Provider::Claude));
    }
}

mod message_role_enum {
    use super::*;

    #[test]
    fn serialize_all_roles() {
        let roles = vec![
            (MessageRole::User, "\"user\""),
            (MessageRole::Assistant, "\"assistant\""),
            (MessageRole::System, "\"system\""),
        ];

        for (role, expected) in roles {
            let json = serde_json::to_string(&role).unwrap();
            assert_eq!(json, expected, "Role {:?} should serialize correctly", role);
        }
    }

    #[test]
    fn deserialize_all_roles() {
        let cases = vec![
            ("\"user\"", MessageRole::User),
            ("\"assistant\"", MessageRole::Assistant),
            ("\"system\"", MessageRole::System),
        ];

        for (json, expected) in cases {
            let role: MessageRole = serde_json::from_str(json).unwrap();
            assert_eq!(role, expected, "Should deserialize {} correctly", json);
        }
    }

    #[test]
    fn role_roundtrip() {
        let roles = vec![MessageRole::User, MessageRole::Assistant, MessageRole::System];

        for role in roles {
            let json = serde_json::to_string(&role).unwrap();
            let restored: MessageRole = serde_json::from_str(&json).unwrap();
            assert_eq!(role, restored, "Role should survive roundtrip");
        }
    }

    #[test]
    fn role_display() {
        assert_eq!(format!("{}", MessageRole::User), "user");
        assert_eq!(format!("{}", MessageRole::Assistant), "assistant");
        assert_eq!(format!("{}", MessageRole::System), "system");
    }
}

mod metadata_struct {
    use super::*;

    #[test]
    fn empty_metadata() {
        let metadata = Metadata::new();
        assert!(metadata.fields.is_empty());
    }

    #[test]
    fn insert_and_get() {
        let mut metadata = Metadata::new();
        metadata.insert("key".to_string(), serde_json::json!("value"));

        let value = metadata.get("key");
        assert!(value.is_some());
        assert_eq!(value.unwrap(), &serde_json::json!("value"));
    }

    #[test]
    fn get_missing_key() {
        let metadata = Metadata::new();
        assert!(metadata.get("nonexistent").is_none());
    }

    #[test]
    fn metadata_with_various_types() {
        let mut metadata = Metadata::new();
        metadata.insert("string".to_string(), serde_json::json!("hello"));
        metadata.insert("number".to_string(), serde_json::json!(42));
        metadata.insert("float".to_string(), serde_json::json!(1.5));
        metadata.insert("bool".to_string(), serde_json::json!(true));
        metadata.insert("null".to_string(), serde_json::json!(null));
        metadata.insert("array".to_string(), serde_json::json!([1, 2, 3]));
        metadata.insert(
            "object".to_string(),
            serde_json::json!({"nested": "value"}),
        );

        assert_eq!(metadata.fields.len(), 7);
    }

    #[test]
    fn metadata_serialization() {
        let mut metadata = Metadata::new();
        metadata.insert("key1".to_string(), serde_json::json!("value1"));
        metadata.insert("key2".to_string(), serde_json::json!(123));

        let json = serde_json::to_string(&metadata).unwrap();

        // Metadata uses #[serde(flatten)], so keys appear at top level
        assert!(json.contains("key1"));
        assert!(json.contains("value1"));
        assert!(json.contains("key2"));
        assert!(json.contains("123"));
    }

    #[test]
    fn metadata_roundtrip() {
        let mut original = Metadata::new();
        original.insert("test".to_string(), serde_json::json!("data"));
        original.insert("count".to_string(), serde_json::json!(42));

        let json = serde_json::to_string(&original).unwrap();
        let restored: Metadata = serde_json::from_str(&json).unwrap();

        assert_eq!(
            restored.get("test"),
            Some(&serde_json::json!("data"))
        );
        assert_eq!(
            restored.get("count"),
            Some(&serde_json::json!(42))
        );
    }
}

mod message_struct {
    use super::*;

    fn create_message() -> Message {
        Message {
            id: "msg-123".to_string(),
            conversation_id: "conv-456".to_string(),
            role: MessageRole::User,
            content: "Hello, world!".to_string(),
            timestamp: DateTime::parse_from_rfc3339("2024-01-15T10:30:00Z")
                .unwrap()
                .with_timezone(&Utc),
            metadata: Metadata::new(),
        }
    }

    #[test]
    fn message_serialization() {
        let message = create_message();
        let json = serde_json::to_string(&message).unwrap();

        assert!(json.contains("\"id\":\"msg-123\""));
        assert!(json.contains("\"conversation_id\":\"conv-456\""));
        assert!(json.contains("\"role\":\"user\""));
        assert!(json.contains("\"content\":\"Hello, world!\""));
        assert!(json.contains("\"timestamp\":"));
    }

    #[test]
    fn message_roundtrip() {
        let original = create_message();
        let json = serde_json::to_string(&original).unwrap();
        let restored: Message = serde_json::from_str(&json).unwrap();

        assert_eq!(original.id, restored.id);
        assert_eq!(original.conversation_id, restored.conversation_id);
        assert_eq!(original.role, restored.role);
        assert_eq!(original.content, restored.content);
        assert_eq!(original.timestamp, restored.timestamp);
    }

    #[test]
    fn message_with_metadata() {
        let mut message = create_message();
        message.metadata.insert("tokens".to_string(), serde_json::json!(150));
        message
            .metadata
            .insert("model".to_string(), serde_json::json!("gpt-4"));

        let json = serde_json::to_string(&message).unwrap();
        let restored: Message = serde_json::from_str(&json).unwrap();

        assert_eq!(
            restored.metadata.get("tokens"),
            Some(&serde_json::json!(150))
        );
        assert_eq!(
            restored.metadata.get("model"),
            Some(&serde_json::json!("gpt-4"))
        );
    }

    #[test]
    fn message_all_roles() {
        for role in [MessageRole::User, MessageRole::Assistant, MessageRole::System] {
            let message = Message {
                id: "test".to_string(),
                conversation_id: "conv".to_string(),
                role,
                content: "test content".to_string(),
                timestamp: Utc::now(),
                metadata: Metadata::new(),
            };

            let json = serde_json::to_string(&message).unwrap();
            let restored: Message = serde_json::from_str(&json).unwrap();
            assert_eq!(message.role, restored.role);
        }
    }
}

mod conversation_struct {
    use super::*;

    fn create_conversation() -> Conversation {
        Conversation::new(
            "conv-789".to_string(),
            "Test Conversation".to_string(),
            Provider::Claude,
        )
    }

    #[test]
    fn new_conversation() {
        let conv = create_conversation();

        assert_eq!(conv.id, "conv-789");
        assert_eq!(conv.title, "Test Conversation");
        assert_eq!(conv.provider, Provider::Claude);
        assert!(conv.messages.is_empty());
        assert_eq!(conv.message_count(), 0);
    }

    #[test]
    fn add_message() {
        let mut conv = create_conversation();
        let initial_updated = conv.updated_at;

        // Small delay to ensure timestamp changes
        std::thread::sleep(std::time::Duration::from_millis(10));

        let message = Message {
            id: "msg-1".to_string(),
            conversation_id: conv.id.clone(),
            role: MessageRole::User,
            content: "Hello".to_string(),
            timestamp: Utc::now(),
            metadata: Metadata::new(),
        };

        conv.add_message(message);

        assert_eq!(conv.message_count(), 1);
        assert!(conv.updated_at >= initial_updated);
    }

    #[test]
    fn conversation_serialization() {
        let conv = create_conversation();
        let json = serde_json::to_string(&conv).unwrap();

        assert!(json.contains("\"id\":\"conv-789\""));
        assert!(json.contains("\"title\":\"Test Conversation\""));
        assert!(json.contains("\"provider\":\"claude\""));
        assert!(json.contains("\"messages\":[]"));
    }

    #[test]
    fn conversation_roundtrip() {
        let mut original = create_conversation();

        original.add_message(Message {
            id: "msg-1".to_string(),
            conversation_id: original.id.clone(),
            role: MessageRole::User,
            content: "Hello".to_string(),
            timestamp: Utc::now(),
            metadata: Metadata::new(),
        });

        original.add_message(Message {
            id: "msg-2".to_string(),
            conversation_id: original.id.clone(),
            role: MessageRole::Assistant,
            content: "Hi there!".to_string(),
            timestamp: Utc::now(),
            metadata: Metadata::new(),
        });

        let json = serde_json::to_string(&original).unwrap();
        let restored: Conversation = serde_json::from_str(&json).unwrap();

        assert_eq!(original.id, restored.id);
        assert_eq!(original.title, restored.title);
        assert_eq!(original.provider, restored.provider);
        assert_eq!(original.message_count(), restored.message_count());
        assert_eq!(original.messages[0].content, restored.messages[0].content);
        assert_eq!(original.messages[1].content, restored.messages[1].content);
    }

    #[test]
    fn conversation_with_all_providers() {
        let providers = vec![
            Provider::ChatGpt,
            Provider::Claude,
            Provider::Gemini,
            Provider::Copilot,
            Provider::Other,
        ];

        for provider in providers {
            let conv = Conversation::new(
                format!("conv-{}", provider.as_str()),
                format!("{} Conversation", provider.as_str()),
                provider,
            );

            let json = serde_json::to_string(&conv).unwrap();
            let restored: Conversation = serde_json::from_str(&json).unwrap();

            assert_eq!(conv.provider, restored.provider);
        }
    }

    #[test]
    fn conversation_with_metadata() {
        let mut conv = create_conversation();
        conv.metadata
            .insert("source".to_string(), serde_json::json!("export"));
        conv.metadata
            .insert("version".to_string(), serde_json::json!(2));

        let json = serde_json::to_string(&conv).unwrap();
        let restored: Conversation = serde_json::from_str(&json).unwrap();

        assert_eq!(
            restored.metadata.get("source"),
            Some(&serde_json::json!("export"))
        );
        assert_eq!(
            restored.metadata.get("version"),
            Some(&serde_json::json!(2))
        );
    }
}

mod version_constants {
    use llm_unify_core::{VERSION, VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH};

    #[test]
    fn version_string_format() {
        // VERSION should be in semver format X.Y.Z
        let parts: Vec<&str> = VERSION.split('.').collect();
        assert_eq!(parts.len(), 3, "VERSION should have 3 parts");

        for part in &parts {
            assert!(
                part.parse::<u32>().is_ok(),
                "Each part should be a valid number"
            );
        }
    }

    #[test]
    fn version_components_match() {
        let expected = format!("{}.{}.{}", VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
        assert_eq!(
            VERSION, expected,
            "VERSION should match component constants"
        );
    }

    #[test]
    fn version_is_valid_semver() {
        // Parse version components
        let parts: Vec<u32> = VERSION
            .split('.')
            .map(|s| s.parse().unwrap())
            .collect();

        assert_eq!(parts[0], VERSION_MAJOR);
        assert_eq!(parts[1], VERSION_MINOR);
        assert_eq!(parts[2], VERSION_PATCH);
    }
}
