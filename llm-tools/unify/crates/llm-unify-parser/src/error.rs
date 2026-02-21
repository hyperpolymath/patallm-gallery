//! Parser error types

use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Parse error: {0}")]
    Parse(String),

    #[error("Invalid format: {0}")]
    InvalidFormat(String),

    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),

    #[error("Core error: {0}")]
    Core(#[from] llm_unify_core::Error),

    #[error("{0}")]
    Other(String),
}

pub type Result<T> = std::result::Result<T, Error>;
