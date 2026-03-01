// SPDX-License-Identifier: MPL-2.0
//! Fuzz target for ClaimExtractor text parsing
//!
//! This target tests the robustness of regex-based claim extraction from text.

#![no_main]

use libfuzzer_sys::fuzz_target;
use did_you_actually_do_that::claim_extractor::{ClaimExtractor, ExtractionContext};

fuzz_target!(|data: &[u8]| {
    // Try to parse the bytes as UTF-8 text
    if let Ok(text) = std::str::from_utf8(data) {
        let extractor = ClaimExtractor::new();
        let context = ExtractionContext {
            source: "fuzz-test".to_string(),
            working_directory: None,
            conversation_id: None,
            session_start: None,
        };

        // Extract claims from the fuzzed text - should never crash
        let _ = extractor.extract_from_text(text, &context);
    }
});
