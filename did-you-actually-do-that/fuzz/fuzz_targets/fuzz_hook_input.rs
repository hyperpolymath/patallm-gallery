// SPDX-License-Identifier: MPL-2.0
//! Fuzz target for HookInput JSON parsing
//!
//! This target tests the robustness of hook input deserialization.

#![no_main]

use libfuzzer_sys::fuzz_target;
use did_you_actually_do_that::hooks::{parse_hook_input, HookHandler};

fuzz_target!(|data: &[u8]| {
    // Try to parse the bytes as JSON hook input
    if let Ok(json_str) = std::str::from_utf8(data) {
        // Attempt to parse
        if let Ok(input) = parse_hook_input(json_str) {
            // If parsing succeeds, try handling the input
            let handler = HookHandler::new();
            let _ = handler.handle(&input);
        }
    }
});
