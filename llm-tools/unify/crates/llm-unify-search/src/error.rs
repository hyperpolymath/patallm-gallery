//! Search error types

use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Search error: {0}")]
    Search(String),

    #[error("Storage error: {0}")]
    Storage(#[from] llm_unify_storage::Error),

    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("{0}")]
    Other(String),
}

pub type Result<T> = std::result::Result<T, Error>;
