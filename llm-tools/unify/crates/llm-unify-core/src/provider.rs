//! Provider trait definition

use crate::{Conversation, Result};

/// Trait for LLM provider implementations
pub trait ProviderTrait {
    /// Parse provider-specific export format
    fn parse(&self, data: &[u8]) -> Result<Vec<Conversation>>;

    /// Get provider name
    fn name(&self) -> &'static str;

    /// Validate conversation data
    fn validate(&self, conversation: &Conversation) -> Result<()>;
}
