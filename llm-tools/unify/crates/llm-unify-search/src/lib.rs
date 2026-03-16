//! Full-text search functionality

#![forbid(unsafe_code)]
pub mod error;
pub mod search;

pub use error::{Error, Result};
pub use search::{SearchEngine, SearchResult};
