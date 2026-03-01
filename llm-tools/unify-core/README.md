# llm-unify-core

Core types for managing LLM conversations across multiple providers (ChatGPT, Claude, Gemini, Copilot).

## Features

- Unified conversation model for all LLM providers
- Message and metadata types
- Provider trait for custom implementations
- Serialization support (serde)

## Installation

```toml
[dependencies]
llm-unify-core = "0.1"
```

## Usage

```rust
use llm_unify_core::{Conversation, Message, MessageRole, Provider, Metadata};
use chrono::Utc;

// Create a conversation
let mut conversation = Conversation::new(
    "conv-123".into(),
    "Rust Questions".into(),
    Provider::Claude,
);

// Add a message
let message = Message {
    id: "msg-1".into(),
    conversation_id: "conv-123".into(),
    role: MessageRole::User,
    content: "What is ownership in Rust?".into(),
    timestamp: Utc::now(),
    metadata: Metadata::new(),
};

conversation.add_message(message);
```

## Types

### Provider
```rust
pub enum Provider {
    ChatGpt,
    Claude,
    Gemini,
    Copilot,
    Other,
}
```

### MessageRole
```rust
pub enum MessageRole {
    User,
    Assistant,
    System,
}
```

### Conversation
```rust
pub struct Conversation {
    pub id: String,
    pub title: String,
    pub provider: Provider,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub messages: Vec<Message>,
    pub metadata: Metadata,
}
```

## Provider Trait

Implement custom providers:

```rust
use llm_unify_core::{ProviderTrait, Conversation, Result};

struct MyProvider;

impl ProviderTrait for MyProvider {
    fn parse(&self, data: &[u8]) -> Result<Vec<Conversation>> {
        // Parse provider-specific format
        todo!()
    }

    fn name(&self) -> &'static str {
        "my-provider"
    }

    fn validate(&self, conversation: &Conversation) -> Result<()> {
        Ok(())
    }
}
```

## Related

- [llm-unify](https://github.com/hyperpolymath/llm-unify) - Full application using this library

## License

PMPL-1.0-or-later
