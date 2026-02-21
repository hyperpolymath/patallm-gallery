//! Core types and traits for LLM Unify
//!
//! This crate provides the fundamental abstractions for managing LLM conversations
//! across multiple platforms.

pub mod error;
pub mod models;
pub mod provider;

pub use error::{Error, Result};
pub use models::{Conversation, Message, MessageRole, Metadata, Provider};
pub use provider::ProviderTrait;
