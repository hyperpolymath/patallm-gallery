//! Core types and traits for LLM Unify
//!
//! This crate provides the fundamental abstractions for managing LLM conversations
//! across multiple platforms.

/// Crate version (synced with Cargo.toml)
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Crate version as semver components
pub const VERSION_MAJOR: u32 = 0;
pub const VERSION_MINOR: u32 = 1;
pub const VERSION_PATCH: u32 = 0;

pub mod error;
pub mod models;
pub mod provider;

pub use error::{Error, Result};
pub use models::{Conversation, Message, MessageRole, Metadata, Provider};
pub use provider::ProviderTrait;
