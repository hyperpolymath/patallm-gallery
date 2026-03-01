// DYADT FFI Implementation
//
// This module implements the C-compatible FFI declared in src/abi/Foreign.idr
// for the did-you-actually-do-that verification engine.
//
// All types and layouts must match the Idris2 ABI definitions in src/abi/Types.idr.
//
// SPDX-License-Identifier: PMPL-1.0-or-later

const std = @import("std");

// Version information (keep in sync with Cargo.toml)
const VERSION = "0.1.0";
const BUILD_INFO = "DYADT built with Zig " ++ @import("builtin").zig_version_string;

/// Thread-local error storage
threadlocal var last_error: ?[]const u8 = null;

/// Set the last error message
fn setError(msg: []const u8) void {
    last_error = msg;
}

/// Clear the last error
fn clearError() void {
    last_error = null;
}

//==============================================================================
// Core Types (must match src/abi/Types.idr)
//==============================================================================

/// Result codes (must match Idris2 Result type)
pub const Result = enum(c_int) {
    ok = 0,
    @"error" = 1,
    invalid_param = 2,
    out_of_memory = 3,
    null_pointer = 4,
    verification_failed = 5,
    ensemble_timeout = 6,
};

/// Verification verdicts (must match Idris2 Verdict type)
pub const Verdict = enum(u32) {
    confirmed = 0,
    refuted = 1,
    inconclusive = 2,
    unverifiable = 3,
};

/// SLM vote decisions (must match Idris2 VoteDecision type)
pub const VoteDecision = enum(u32) {
    approve = 0,
    reject = 1,
    abstain = 2,
};

/// Violation categories (1-indexed, must match Rust ViolationCategory)
pub const ViolationCategory = enum(u32) {
    incomplete_implementation = 1,
    missing_error_handling = 2,
    untested_code_paths = 3,
    stub_placeholder_left_in_place = 4,
    claimed_but_nonexistent_file = 5,
    broken_imports_exports = 6,
    regression_introduced = 7,
    documentation_mismatch = 8,
    dependency_not_added = 9,
    configuration_not_updated = 10,
    test_not_actually_run = 11,
    scope_silently_reduced = 12,
};

/// LayerResult FFI struct (16 bytes, 8-byte aligned)
pub const LayerResult = extern struct {
    layer: u32,
    verdict: u32,
    confidence: f64,
};

/// Vote FFI struct (24 bytes, 8-byte aligned)
pub const Vote = extern struct {
    decision: u32,
    _pad0: u32 = 0,
    confidence: f64,
    weight: f64,
};

/// EnsembleResult FFI struct (32 bytes, 8-byte aligned)
pub const EnsembleResult = extern struct {
    approve_weight: f64,
    reject_weight: f64,
    abstain_count: u32,
    suggested_verdict: u32,
    vote_count: u32,
    _pad0: u32 = 0,
};

/// Report summary FFI struct (24 bytes, 8-byte aligned)
pub const ReportSummary = extern struct {
    verdict: u32,
    layer_count: u32,
    confidence: f64,
    violation_count: u32,
    _pad0: u32 = 0,
};

/// Library handle (internal state)
const HandleData = struct {
    allocator: std.mem.Allocator,
    initialized: bool,
    // Last verification result
    last_verdict: Verdict,
    last_report_json: ?[]const u8,
    // Last ensemble result
    last_ensemble_json: ?[]const u8,
    last_suggested_verdict: Verdict,
    // Last extracted claims
    last_claims_json: ?[]const u8,
    // Engine stats
    backend_count: u32,
    layer_count: u32,
};

//==============================================================================
// Library Lifecycle
//==============================================================================

/// Initialize the DYADT verification engine
/// Returns a handle, or null on failure
export fn dyadt_init() ?*HandleData {
    const allocator = std.heap.c_allocator;

    const handle = allocator.create(HandleData) catch {
        setError("Failed to allocate DYADT handle");
        return null;
    };

    handle.* = .{
        .allocator = allocator,
        .initialized = true,
        .last_verdict = .unverifiable,
        .last_report_json = null,
        .last_ensemble_json = null,
        .last_suggested_verdict = .unverifiable,
        .last_claims_json = null,
        .backend_count = 2, // GoT + MoE
        .layer_count = 12,
    };

    clearError();
    return handle;
}

/// Free the DYADT engine handle
export fn dyadt_free(handle: ?*HandleData) void {
    const h = handle orelse return;
    const allocator = h.allocator;

    // Free any allocated result strings
    if (h.last_report_json) |json| {
        allocator.free(json);
    }
    if (h.last_ensemble_json) |json| {
        allocator.free(json);
    }
    if (h.last_claims_json) |json| {
        allocator.free(json);
    }

    h.initialized = false;
    allocator.destroy(h);
    clearError();
}

//==============================================================================
// Claim Verification
//==============================================================================

/// Verify a single claim
/// Takes JSON claim data, stores result internally
export fn dyadt_verify_claim(
    handle: ?*HandleData,
    claim_ptr: ?[*]const u8,
    claim_len: u32,
) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Engine not initialized");
        return .@"error";
    }

    const data = claim_ptr orelse {
        setError("Null claim pointer");
        return .null_pointer;
    };

    if (claim_len == 0) {
        setError("Empty claim data");
        return .invalid_param;
    }

    // Access claim JSON for validation
    const claim_json = data[0..claim_len];
    _ = claim_json;

    // In a full implementation, this would call into the Rust verification engine
    // via the port protocol. For FFI purposes, we validate the interface contract.
    h.last_verdict = .inconclusive;

    // Free previous report
    if (h.last_report_json) |old| {
        h.allocator.free(old);
    }

    const report = h.allocator.dupeZ(u8, "{\"verdict\":\"Inconclusive\",\"layers\":[]}") catch {
        setError("Failed to allocate report");
        return .out_of_memory;
    };
    h.last_report_json = report;

    clearError();
    return .ok;
}

/// Get the verdict from the last verification
export fn dyadt_get_verdict(handle: ?*HandleData) u32 {
    const h = handle orelse return @intFromEnum(Verdict.unverifiable);

    if (!h.initialized) return @intFromEnum(Verdict.unverifiable);

    return @intFromEnum(h.last_verdict);
}

/// Get the full verification report as JSON string
/// Caller must free with dyadt_free_string
export fn dyadt_get_report(handle: ?*HandleData) ?[*:0]const u8 {
    const h = handle orelse {
        setError("Null handle");
        return null;
    };

    if (!h.initialized) {
        setError("Engine not initialized");
        return null;
    }

    const json = h.last_report_json orelse {
        setError("No verification result available");
        return null;
    };

    // Duplicate so caller can free independently
    const result = h.allocator.dupeZ(u8, json) catch {
        setError("Failed to allocate report string");
        return null;
    };

    clearError();
    return result.ptr;
}

//==============================================================================
// SLM Ensemble Evaluation
//==============================================================================

/// Evaluate through the SLM ensemble
export fn dyadt_evaluate_slm(
    handle: ?*HandleData,
    context_ptr: ?[*]const u8,
    context_len: u32,
) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Engine not initialized");
        return .@"error";
    }

    const data = context_ptr orelse {
        setError("Null context pointer");
        return .null_pointer;
    };

    if (context_len == 0) {
        setError("Empty context data");
        return .invalid_param;
    }

    const context_json = data[0..context_len];
    _ = context_json;

    // Placeholder: full implementation calls Rust ensemble via port protocol
    h.last_suggested_verdict = .inconclusive;

    if (h.last_ensemble_json) |old| {
        h.allocator.free(old);
    }

    const result_json = h.allocator.dupeZ(
        u8,
        "{\"votes\":[],\"approve_weight\":0.0,\"reject_weight\":0.0,\"abstain_count\":0,\"suggested_verdict\":\"Inconclusive\"}",
    ) catch {
        setError("Failed to allocate ensemble result");
        return .out_of_memory;
    };
    h.last_ensemble_json = result_json;

    clearError();
    return .ok;
}

/// Get the ensemble result as JSON string
export fn dyadt_get_ensemble_result(handle: ?*HandleData) ?[*:0]const u8 {
    const h = handle orelse {
        setError("Null handle");
        return null;
    };

    if (!h.initialized) {
        setError("Engine not initialized");
        return null;
    }

    const json = h.last_ensemble_json orelse {
        setError("No ensemble result available");
        return null;
    };

    const result = h.allocator.dupeZ(u8, json) catch {
        setError("Failed to allocate ensemble result string");
        return null;
    };

    clearError();
    return result.ptr;
}

/// Get the suggested verdict from the last SLM evaluation
export fn dyadt_get_suggested_verdict(handle: ?*HandleData) u32 {
    const h = handle orelse return @intFromEnum(Verdict.unverifiable);

    if (!h.initialized) return @intFromEnum(Verdict.unverifiable);

    return @intFromEnum(h.last_suggested_verdict);
}

//==============================================================================
// Claim Extraction
//==============================================================================

/// Extract claims from text
export fn dyadt_extract_claims(
    handle: ?*HandleData,
    text_ptr: ?[*]const u8,
    text_len: u32,
) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Engine not initialized");
        return .@"error";
    }

    const data = text_ptr orelse {
        setError("Null text pointer");
        return .null_pointer;
    };

    if (text_len == 0) {
        setError("Empty text data");
        return .invalid_param;
    }

    const text = data[0..text_len];
    _ = text;

    if (h.last_claims_json) |old| {
        h.allocator.free(old);
    }

    const claims = h.allocator.dupeZ(u8, "[]") catch {
        setError("Failed to allocate claims result");
        return .out_of_memory;
    };
    h.last_claims_json = claims;

    clearError();
    return .ok;
}

/// Get extracted claims as JSON array string
export fn dyadt_get_claims(handle: ?*HandleData) ?[*:0]const u8 {
    const h = handle orelse {
        setError("Null handle");
        return null;
    };

    if (!h.initialized) {
        setError("Engine not initialized");
        return null;
    }

    const json = h.last_claims_json orelse {
        setError("No claims result available");
        return null;
    };

    const result = h.allocator.dupeZ(u8, json) catch {
        setError("Failed to allocate claims string");
        return null;
    };

    clearError();
    return result.ptr;
}

//==============================================================================
// String Operations
//==============================================================================

/// Free a string allocated by the library
export fn dyadt_free_string(str: ?[*:0]const u8) void {
    const s = str orelse return;
    const allocator = std.heap.c_allocator;

    const slice = std.mem.span(s);
    allocator.free(slice);
}

//==============================================================================
// Error Handling
//==============================================================================

/// Get the last error message
/// Returns null if no error
export fn dyadt_last_error() ?[*:0]const u8 {
    const err = last_error orelse return null;

    const allocator = std.heap.c_allocator;
    const c_str = allocator.dupeZ(u8, err) catch return null;
    return c_str.ptr;
}

//==============================================================================
// Version Information
//==============================================================================

/// Get the library version
export fn dyadt_version() [*:0]const u8 {
    return VERSION.ptr;
}

/// Get build information
export fn dyadt_build_info() [*:0]const u8 {
    return BUILD_INFO.ptr;
}

//==============================================================================
// Utility Functions
//==============================================================================

/// Check if engine is initialized
export fn dyadt_is_initialized(handle: ?*HandleData) u32 {
    const h = handle orelse return 0;
    return if (h.initialized) 1 else 0;
}

/// Get the number of SLM backends registered
export fn dyadt_backend_count(handle: ?*HandleData) u32 {
    const h = handle orelse return 0;
    if (!h.initialized) return 0;
    return h.backend_count;
}

/// Get the number of verification layers
export fn dyadt_layer_count(handle: ?*HandleData) u32 {
    const h = handle orelse return 0;
    if (!h.initialized) return 0;
    return h.layer_count;
}

//==============================================================================
// Tests
//==============================================================================

test "lifecycle" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    try std.testing.expect(dyadt_is_initialized(handle) == 1);
}

test "backend and layer counts" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    try std.testing.expectEqual(@as(u32, 2), dyadt_backend_count(handle));
    try std.testing.expectEqual(@as(u32, 12), dyadt_layer_count(handle));
}

test "verify claim lifecycle" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const claim = "{\"description\":\"Added login endpoint\"}";
    const result = dyadt_verify_claim(handle, claim.ptr, @intCast(claim.len));
    try std.testing.expectEqual(Result.ok, result);

    // Should have a verdict now
    const verdict = dyadt_get_verdict(handle);
    try std.testing.expect(verdict <= 3); // Valid verdict range

    // Should have a report
    const report = dyadt_get_report(handle);
    try std.testing.expect(report != null);
    if (report) |r| dyadt_free_string(r);
}

test "evaluate slm lifecycle" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const ctx = "{\"claim_text\":\"test\",\"evidence_summary\":\"\"}";
    const result = dyadt_evaluate_slm(handle, ctx.ptr, @intCast(ctx.len));
    try std.testing.expectEqual(Result.ok, result);

    const ensemble = dyadt_get_ensemble_result(handle);
    try std.testing.expect(ensemble != null);
    if (ensemble) |e| dyadt_free_string(e);
}

test "extract claims lifecycle" {
    const handle = dyadt_init() orelse return error.InitFailed;
    defer dyadt_free(handle);

    const text = "I added a login endpoint and wrote tests for it.";
    const result = dyadt_extract_claims(handle, text.ptr, @intCast(text.len));
    try std.testing.expectEqual(Result.ok, result);

    const claims = dyadt_get_claims(handle);
    try std.testing.expect(claims != null);
    if (claims) |c| dyadt_free_string(c);
}

test "null handle errors" {
    try std.testing.expectEqual(Result.null_pointer, dyadt_verify_claim(null, null, 0));
    try std.testing.expectEqual(Result.null_pointer, dyadt_evaluate_slm(null, null, 0));
    try std.testing.expectEqual(Result.null_pointer, dyadt_extract_claims(null, null, 0));
    try std.testing.expectEqual(@as(u32, 0), dyadt_is_initialized(null));
    try std.testing.expectEqual(@as(u32, 0), dyadt_backend_count(null));
    try std.testing.expectEqual(@as(u32, 0), dyadt_layer_count(null));
}

test "error handling" {
    _ = dyadt_verify_claim(null, null, 0);

    const err = dyadt_last_error();
    try std.testing.expect(err != null);

    if (err) |e| {
        const err_str = std.mem.span(e);
        try std.testing.expect(err_str.len > 0);
        dyadt_free_string(e);
    }
}

test "version" {
    const ver = dyadt_version();
    const ver_str = std.mem.span(ver);
    try std.testing.expectEqualStrings(VERSION, ver_str);
}

test "struct sizes" {
    // Verify C ABI struct sizes match Idris2 proofs
    try std.testing.expectEqual(@as(usize, 16), @sizeOf(LayerResult));
    try std.testing.expectEqual(@as(usize, 24), @sizeOf(Vote));
    try std.testing.expectEqual(@as(usize, 32), @sizeOf(EnsembleResult));
    try std.testing.expectEqual(@as(usize, 24), @sizeOf(ReportSummary));
}

test "struct alignments" {
    // Verify 8-byte alignment for all FFI structs
    try std.testing.expectEqual(@as(usize, 8), @alignOf(LayerResult));
    try std.testing.expectEqual(@as(usize, 8), @alignOf(Vote));
    try std.testing.expectEqual(@as(usize, 8), @alignOf(EnsembleResult));
    try std.testing.expectEqual(@as(usize, 8), @alignOf(ReportSummary));
}
