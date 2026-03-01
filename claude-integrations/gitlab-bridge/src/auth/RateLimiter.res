// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * In-memory rate limiter using sliding window algorithm.
 *
 * Provides configurable rate limiting with sliding window,
 * per-key tracking, automatic cleanup, and Express middleware integration.
 */

open Types

// Timer ID type for setInterval/clearInterval
type intervalId

@val external setInterval: (unit => unit, int) => intervalId = "setInterval"
@val external clearInterval: intervalId => unit = "clearInterval"
@send external unref: intervalId => unit = "unref"

// Rate limit configuration
type rateLimitConfig = {
  limit: int,
  windowMs: int,
  headers: bool,
  message: string,
}

// Default rate limit configuration
let defaultConfig: rateLimitConfig = {
  limit: 60,
  windowMs: 60 * 1000,
  headers: true,
  message: "Too many requests, please try again later.",
}

// Internal record for tracking request timestamps
type rateLimitRecord = {
  mutable timestamps: array<float>,
  mutable lastAccess: float,
}

// Rate limiter instance (mutable state)
type rateLimiter = {
  config: rateLimitConfig,
  store: Map.t<string, rateLimitRecord>,
  mutable cleanupInterval: option<intervalId>,
}

/**
 * Create a new rate limiter with the given configuration.
 * Starts a cleanup interval to prevent memory leaks.
 */
let make = (~config: rateLimitConfig=defaultConfig): rateLimiter => {
  let limiter = {
    config,
    store: Map.make(),
    cleanupInterval: None,
  }

  // Start cleanup interval (every 60 seconds)
  let interval = setInterval(() => {
    let now = Date.now()
    let expireTime = config.windowMs->Int.toFloat *. 2.0

    limiter.store->Map.forEach((record, key) => {
      if now -. record.lastAccess > expireTime {
        let _ = limiter.store->Map.delete(key)
      }
    })
  }, 60 * 1000)

  // Don't prevent Node from exiting
  unref(interval)

  limiter.cleanupInterval = Some(interval)
  limiter
}

/**
 * Stop the cleanup interval.
 */
let stop = (limiter: rateLimiter): unit => {
  switch limiter.cleanupInterval {
  | Some(interval) => {
      clearInterval(interval)
      limiter.cleanupInterval = None
    }
  | None => ()
  }
}

/**
 * Check rate limit status for a key without recording a hit.
 */
let check = (limiter: rateLimiter, key: string): rateLimitInfo => {
  let now = Date.now()
  let windowStart = now -. limiter.config.windowMs->Int.toFloat

  // Get or create record
  let record = switch limiter.store->Map.get(key) {
  | None => {
      let newRecord = {timestamps: [], lastAccess: now}
      limiter.store->Map.set(key, newRecord)
      newRecord
    }
  | Some(existing) => existing
  }

  // Filter out timestamps outside the window
  record.timestamps = record.timestamps->Array.filter(ts => ts > windowStart)
  record.lastAccess = now

  // Calculate remaining requests
  let currentCount = record.timestamps->Array.length
  let remaining = Math.Int.max(0, limiter.config.limit - currentCount)
  let isLimited = remaining == 0

  // Calculate reset time
  let resetTime = switch record.timestamps->Array.get(0) {
  | Some(oldest) => oldest +. limiter.config.windowMs->Int.toFloat
  | None => now +. limiter.config.windowMs->Int.toFloat
  }

  {
    limit: limiter.config.limit,
    remaining,
    resetAt: Date.fromTime(resetTime),
    isLimited,
  }
}

/**
 * Record a request hit for a key and return updated rate limit info.
 */
let hit = (limiter: rateLimiter, key: string): rateLimitInfo => {
  let now = Date.now()
  let info = check(limiter, key)

  if !info.isLimited {
    switch limiter.store->Map.get(key) {
    | Some(record) => {
        let _ = record.timestamps->Array.push(now)
        {
          ...info,
          remaining: Math.Int.max(0, info.remaining - 1),
        }
      }
    | None => info
    }
  } else {
    info
  }
}

/**
 * Reset the rate limit for a specific key.
 */
let reset = (limiter: rateLimiter, key: string): unit => {
  let _ = limiter.store->Map.delete(key)
}

/**
 * Reset all rate limits.
 */
let resetAll = (limiter: rateLimiter): unit => {
  limiter.store->Map.clear
}

/**
 * Get the current count of tracked keys.
 */
let size = (limiter: rateLimiter): int => {
  limiter.store->Map.size
}

/**
 * Get rate limit info without recording a hit (alias for check).
 */
let getInfo = (limiter: rateLimiter, key: string): rateLimitInfo => {
  check(limiter, key)
}

/**
 * Check rate limit and raise exception if exceeded.
 */
let requireRateLimit = (limiter: rateLimiter, key: string): unit => {
  let info = hit(limiter, key)

  if info.isLimited {
    let retryAfter = Math.Int.max(
      1,
      Float.toInt(Math.ceil((info.resetAt->Date.getTime -. Date.now()) /. 1000.0)),
    )
    raise(Errors.RateLimit(retryAfter))
  }
}

// Preset rate limit configurations
type presetConfig = {limit: int, windowMs: int}

let presets: Dict.t<presetConfig> = Dict.fromArray([
  ("strict", {limit: 30, windowMs: 60 * 1000}),
  ("standard", {limit: 60, windowMs: 60 * 1000}),
  ("relaxed", {limit: 120, windowMs: 60 * 1000}),
  ("api", {limit: 1000, windowMs: 60 * 60 * 1000}),
  ("webhook", {limit: 100, windowMs: 60 * 1000}),
  ("auth", {limit: 10, windowMs: 60 * 1000}),
])

/**
 * Create a rate limiter from a preset name.
 */
let makeFromPreset = (~preset: string, ~overrides: option<rateLimitConfig>=?): rateLimiter => {
  let base = switch presets->Dict.get(preset) {
  | Some(p) => {
      limit: p.limit,
      windowMs: p.windowMs,
      headers: true,
      message: "Too many requests, please try again later.",
    }
  | None => defaultConfig
  }

  let config = switch overrides {
  | Some(ov) => {
      limit: if ov.limit > 0 { ov.limit } else { base.limit },
      windowMs: if ov.windowMs > 0 { ov.windowMs } else { base.windowMs },
      headers: ov.headers,
      message: ov.message,
    }
  | None => base
  }

  make(~config)
}
