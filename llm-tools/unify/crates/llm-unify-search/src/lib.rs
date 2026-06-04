// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Full-text search functionality

#![forbid(unsafe_code)]
pub mod error;
pub mod search;

pub use error::{Error, Result};
pub use search::{SearchEngine, SearchResult};
