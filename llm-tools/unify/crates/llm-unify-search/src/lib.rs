//! Full-text search functionality

pub mod error;
pub mod search;

pub use error::{Error, Result};
pub use search::{SearchEngine, SearchResult};
