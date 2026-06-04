// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Core types and traits for LLM Unify
//!
//! This crate provides the fundamental abstractions for managing LLM conversations
//! across multiple platforms.

#![forbid(unsafe_code)]
pub mod error;
pub mod models;
pub mod provider;

pub use error::{Error, Result};
pub use models::{Conversation, Message, MessageRole, Metadata, Provider};
pub use provider::ProviderTrait;
