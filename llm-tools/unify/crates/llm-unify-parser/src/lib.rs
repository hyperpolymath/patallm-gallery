//! Parsers for various LLM platform export formats

pub mod chatgpt;
pub mod claude;
pub mod copilot;
pub mod error;
pub mod gemini;

pub use error::{Error, Result};

use llm_unify_core::{Provider, ProviderTrait};

/// Get parser for a specific provider
pub fn get_parser(provider: Provider) -> Box<dyn ProviderTrait> {
    match provider {
        Provider::ChatGpt => Box::new(chatgpt::ChatGptParser),
        Provider::Claude => Box::new(claude::ClaudeParser),
        Provider::Gemini => Box::new(gemini::GeminiParser),
        Provider::Copilot => Box::new(copilot::CopilotParser),
        Provider::Other => Box::new(chatgpt::ChatGptParser), // Default fallback
    }
}
