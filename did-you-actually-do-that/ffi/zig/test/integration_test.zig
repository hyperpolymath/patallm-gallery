// DYADT Integration Tests
// SPDX-License-Identifier: PMPL-1.0-or-later
//
// These tests verify that the Zig FFI correctly implements the Idris2 ABI
// for the did-you-actually-do-that verification engine.

const std = @import("std");
const testing = std.testing;

// Import FFI functions
extern fn dyadt_init() ?*opaque {};
extern fn dyadt_free(?*opaque {}) void;
extern fn dyadt_verify_claim(?*opaque {}, ?[*]const u8, u32) c_int;
extern fn dyadt_get_verdict(?*opaque {}) u32;
extern fn dyadt_get_report(?*opaque {}) ?[*:0]const u8;
extern fn dyadt_evaluate_slm(?*opaque {}, ?[*]const u8, u32) c_int;
extern fn dyadt_get_ensemble_result(?*opaque {}) ?[*:0]const u8;
extern fn dyadt_get_suggested_verdict(?*opaque {}) u32;
extern fn dyadt_extract_claims(?*opaque {}, ?[*]const u8, u32) c_int;
extern fn dyadt_get_claims(?*opaque {}) ?[*:0]const u8;
extern fn dyadt_free_string(?[*:0]const u8) void;
extern fn dyadt_last_error() ?[*:0]const u8;
extern fn dyadt_version() [*:0]const u8;
extern fn dyadt_build_info() [*:0]const u8;
extern fn dyadt_is_initialized(?*opaque {}) u32;
extern fn dyadt_backend_count(?*opaque {}) u32;
extern fn dyadt_layer_count(?*opaque {}) u32;

//==============================================================================
// Lifecycle Tests
//==============================================================================

test "create and destroy handle" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    try testing.expect(handle != null);
}

test "handle is initialized" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const initialized = dyadt_is_initialized(handle);
    try testing.expectEqual(@as(u32, 1), initialized);
}

test "null handle is not initialized" {
    const initialized = dyadt_is_initialized(null);
    try testing.expectEqual(@as(u32, 0), initialized);
}

test "engine reports correct backend count" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const count = dyadt_backend_count(handle);
    try testing.expectEqual(@as(u32, 2), count); // GoT + MoE
}

test "engine reports 12 verification layers" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const count = dyadt_layer_count(handle);
    try testing.expectEqual(@as(u32, 12), count);
}

//==============================================================================
// Claim Verification Tests
//==============================================================================

test "verify claim with valid JSON" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const claim = "{\"description\":\"Added login endpoint\",\"evidence\":[]}";
    const result = dyadt_verify_claim(handle, claim.ptr, @intCast(claim.len));
    try testing.expectEqual(@as(c_int, 0), result); // 0 = ok
}

test "verify claim with null handle returns error" {
    const result = dyadt_verify_claim(null, null, 0);
    try testing.expectEqual(@as(c_int, 4), result); // 4 = null_pointer
}

test "verify claim with null data returns error" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const result = dyadt_verify_claim(handle, null, 0);
    try testing.expectEqual(@as(c_int, 4), result); // 4 = null_pointer
}

test "verify claim with empty data returns error" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const claim = "x";
    const result = dyadt_verify_claim(handle, claim.ptr, 0);
    try testing.expectEqual(@as(c_int, 2), result); // 2 = invalid_param
}

test "get verdict after verification" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const claim = "{\"description\":\"test\"}";
    _ = dyadt_verify_claim(handle, claim.ptr, @intCast(claim.len));

    const verdict = dyadt_get_verdict(handle);
    try testing.expect(verdict <= 3); // Valid verdict range
}

test "get report after verification" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const claim = "{\"description\":\"test\"}";
    _ = dyadt_verify_claim(handle, claim.ptr, @intCast(claim.len));

    const report = dyadt_get_report(handle);
    try testing.expect(report != null);
    if (report) |r| {
        const report_str = std.mem.span(r);
        try testing.expect(report_str.len > 0);
        dyadt_free_string(r);
    }
}

//==============================================================================
// SLM Ensemble Tests
//==============================================================================

test "evaluate SLM with valid context" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const ctx = "{\"claim_text\":\"Added auth module\",\"evidence_summary\":\"FileExists: Confirmed\"}";
    const result = dyadt_evaluate_slm(handle, ctx.ptr, @intCast(ctx.len));
    try testing.expectEqual(@as(c_int, 0), result); // 0 = ok
}

test "get ensemble result after evaluation" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const ctx = "{\"claim_text\":\"test\",\"evidence_summary\":\"\"}";
    _ = dyadt_evaluate_slm(handle, ctx.ptr, @intCast(ctx.len));

    const ensemble = dyadt_get_ensemble_result(handle);
    try testing.expect(ensemble != null);
    if (ensemble) |e| {
        const ensemble_str = std.mem.span(e);
        try testing.expect(ensemble_str.len > 0);
        dyadt_free_string(e);
    }
}

test "get suggested verdict after evaluation" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const ctx = "{\"claim_text\":\"test\",\"evidence_summary\":\"\"}";
    _ = dyadt_evaluate_slm(handle, ctx.ptr, @intCast(ctx.len));

    const verdict = dyadt_get_suggested_verdict(handle);
    try testing.expect(verdict <= 3); // Valid verdict range
}

//==============================================================================
// Claim Extraction Tests
//==============================================================================

test "extract claims from text" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const text = "I added a login endpoint and wrote comprehensive tests.";
    const result = dyadt_extract_claims(handle, text.ptr, @intCast(text.len));
    try testing.expectEqual(@as(c_int, 0), result);
}

test "get claims after extraction" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const text = "Implemented the parser module.";
    _ = dyadt_extract_claims(handle, text.ptr, @intCast(text.len));

    const claims = dyadt_get_claims(handle);
    try testing.expect(claims != null);
    if (claims) |c| {
        const claims_str = std.mem.span(c);
        try testing.expect(claims_str.len > 0);
        dyadt_free_string(c);
    }
}

//==============================================================================
// Error Handling Tests
//==============================================================================

test "last error after null handle operation" {
    _ = dyadt_verify_claim(null, null, 0);

    const err = dyadt_last_error();
    try testing.expect(err != null);

    if (err) |e| {
        const err_str = std.mem.span(e);
        try testing.expect(err_str.len > 0);
        dyadt_free_string(e);
    }
}

test "no report before verification" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const report = dyadt_get_report(handle);
    try testing.expect(report == null);
}

test "no ensemble result before evaluation" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const result = dyadt_get_ensemble_result(handle);
    try testing.expect(result == null);
}

//==============================================================================
// Version Tests
//==============================================================================

test "version string is not empty" {
    const ver = dyadt_version();
    const ver_str = std.mem.span(ver);

    try testing.expect(ver_str.len > 0);
}

test "version string is semantic version format" {
    const ver = dyadt_version();
    const ver_str = std.mem.span(ver);

    // Should be in format X.Y.Z
    try testing.expect(std.mem.count(u8, ver_str, ".") >= 1);
}

test "build info contains DYADT" {
    const info = dyadt_build_info();
    const info_str = std.mem.span(info);

    try testing.expect(std.mem.indexOf(u8, info_str, "DYADT") != null);
}

//==============================================================================
// Memory Safety Tests
//==============================================================================

test "multiple handles are independent" {
    const h1 = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(h1);

    const h2 = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(h2);

    try testing.expect(h1 != h2);

    // Verify on h1 should not affect h2's state
    const claim = "{\"description\":\"test\"}";
    _ = dyadt_verify_claim(h1, claim.ptr, @intCast(claim.len));

    // h2 should still have no report
    const report = dyadt_get_report(h2);
    try testing.expect(report == null);
}

test "free null is safe" {
    dyadt_free(null); // Should not crash
}

test "free string null is safe" {
    dyadt_free_string(null); // Should not crash
}
