// SPDX-License-Identifier: MPL-2.0
//! Fuzz target for Claim JSON parsing
//!
//! This target tests the robustness of JSON deserialization for Claims.

#![no_main]

use libfuzzer_sys::fuzz_target;
use did_you_actually_do_that::Claim;

fuzz_target!(|data: &[u8]| {
    // Try to parse the bytes as a JSON Claim
    if let Ok(json_str) = std::str::from_utf8(data) {
        // Attempt to parse - we don't care if it fails, just that it doesn't crash
        let _ = serde_json::from_str::<Claim>(json_str);
    }
});
