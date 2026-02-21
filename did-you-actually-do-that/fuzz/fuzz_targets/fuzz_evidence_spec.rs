// SPDX-License-Identifier: MPL-2.0
//! Fuzz target for EvidenceSpec JSON parsing
//!
//! This target tests the robustness of tagged enum deserialization for EvidenceSpec.

#![no_main]

use libfuzzer_sys::fuzz_target;
use did_you_actually_do_that::EvidenceSpec;

fuzz_target!(|data: &[u8]| {
    // Try to parse the bytes as a JSON EvidenceSpec
    if let Ok(json_str) = std::str::from_utf8(data) {
        // Attempt to parse - we don't care if it fails, just that it doesn't crash
        let _ = serde_json::from_str::<EvidenceSpec>(json_str);

        // Also try parsing as a Vec of EvidenceSpec
        let _ = serde_json::from_str::<Vec<EvidenceSpec>>(json_str);
    }
});
