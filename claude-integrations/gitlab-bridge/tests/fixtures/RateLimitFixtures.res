// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Test fixtures for rate limiting.
 */

open RateLimiter

// Test rate limit configurations
let testStrict: rateLimitConfig = {
  limit: 2,
  windowMs: 100,
  headers: true,
  message: "Too many requests, please try again later.",
}

let testStandard: rateLimitConfig = {
  limit: 5,
  windowMs: 500,
  headers: true,
  message: "Too many requests, please try again later.",
}

let testRelaxed: rateLimitConfig = {
  limit: 10,
  windowMs: 1000,
  headers: true,
  message: "Too many requests, please try again later.",
}

let testSingle: rateLimitConfig = {
  limit: 1,
  windowMs: 100,
  headers: true,
  message: "Too many requests, please try again later.",
}

// Test IP addresses
type testIps = {
  client1: string,
  client2: string,
  client3: string,
  localhost: string,
  unknown: string,
}

let testIps: testIps = {
  client1: "192.168.1.100",
  client2: "192.168.1.101",
  client3: "10.0.0.50",
  localhost: "127.0.0.1",
  unknown: "unknown",
}

/**
 * Helper to wait for a specified number of milliseconds.
 */
let wait = (ms: int): promise<unit> => {
  Promise.make((resolve, _reject) => {
    let _ = setTimeout(() => resolve(), ms)
  })
}
