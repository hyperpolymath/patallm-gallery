//! Error types for LLM Unify

use thiserror::Error;

/// Core error type
#[derive(Error, Debug)]
pub enum Error {
    #[error("Provider error: {0}")]
    Provider(String),

    #[error("Invalid conversation: {0}")]
    InvalidConversation(String),

    #[error("Invalid message: {0}")]
    InvalidMessage(String),

    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("{0}")]
    Other(String),
}

/// Result type alias
pub type Result<T> = std::result::Result<T, Error>;
