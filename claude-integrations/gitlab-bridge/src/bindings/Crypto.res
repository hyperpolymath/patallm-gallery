// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * FFI bindings for Node.js crypto module
 *
 * Provides bindings for:
 * - HMAC-SHA256 signing via createHmac
 * - Timing-safe buffer comparison via timingSafeEqual
 * - UUID generation via crypto.randomUUID
 */

// Buffer type (Node.js)
type buffer

// Hmac instance
type hmac

// Create an HMAC instance with algorithm and secret
@module("crypto") external createHmac: (string, string) => hmac = "createHmac"

// Update HMAC with string data
@send external updateString: (hmac, string) => hmac = "update"

// Update HMAC with buffer data
@send external updateBuffer: (hmac, buffer) => hmac = "update"

// Digest HMAC as hex string
@send external digestHex: (hmac, @as("hex") _) => string = "digest"

// Timing-safe comparison of two buffers
@module("crypto") external timingSafeEqual: (buffer, buffer) => bool = "timingSafeEqual"

// Generate a random UUID
@val @scope("crypto") external randomUUID: unit => string = "randomUUID"

// Buffer.from (UTF-8)
@val @scope("Buffer") external bufferFromString: (string, @as("utf8") _) => buffer = "from"

// Buffer.from (hex)
@val @scope("Buffer") external bufferFromHex: (string, @as("hex") _) => buffer = "from"

// Buffer.alloc
@val @scope("Buffer") external bufferAlloc: int => buffer = "alloc"

// Buffer.copy
@send external bufferCopy: (buffer, buffer) => int = "copy"

// Buffer.length
@get external bufferLength: buffer => int = "length"

/**
 * Compute HMAC-SHA256 signature for a string payload.
 * Returns hex-encoded signature string.
 */
let computeHmacSha256 = (payload: string, secret: string): string => {
  createHmac("sha256", secret)->updateString(payload)->digestHex
}

/**
 * Timing-safe string comparison.
 * Compares two strings without leaking timing information.
 * Returns false for different-length strings (after padding comparison).
 */
let timingSafeStringCompare = (a: string, b: string): bool => {
  try {
    let bufA = bufferFromString(a)
    let bufB = bufferFromString(b)
    let lenA = bufferLength(bufA)
    let lenB = bufferLength(bufB)

    if lenA !== lenB {
      // Pad to equal length and compare (to avoid timing leak)
      let maxLen = Math.Int.max(lenA, lenB)
      let paddedA = bufferAlloc(maxLen)
      let paddedB = bufferAlloc(maxLen)
      let _ = bufferCopy(bufA, paddedA)
      let _ = bufferCopy(bufB, paddedB)
      let _ = timingSafeEqual(paddedA, paddedB)
      false
    } else {
      timingSafeEqual(bufA, bufB)
    }
  } catch {
  | _ => false
  }
}

/**
 * Timing-safe hex string comparison.
 * Compares two hex-encoded strings without leaking timing information.
 */
let timingSafeHexCompare = (a: string, b: string): bool => {
  try {
    let bufA = bufferFromHex(a)
    let bufB = bufferFromHex(b)
    let lenA = bufferLength(bufA)
    let lenB = bufferLength(bufB)

    if lenA !== lenB {
      false
    } else {
      timingSafeEqual(bufA, bufB)
    }
  } catch {
  | _ => false
  }
}
