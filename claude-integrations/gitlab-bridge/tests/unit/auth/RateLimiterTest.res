// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Tests for the rate limiter module.
 */

open Vitest
open RateLimitFixtures

describe("rate-limiter", () => {
  describe("RateLimiter", () => {
    let limiterRef: ref<option<RateLimiter.rateLimiter>> = ref(None)

    beforeEach(() => {
      limiterRef := Some(RateLimiter.make(~config=testStrict))
    })

    afterEach(() => {
      switch limiterRef.contents {
      | Some(l) => RateLimiter.stop(l)
      | None => ()
      }
    })

    it("should allow requests within limit", () => {
      switch limiterRef.contents {
      | Some(limiter) => {
          let info1 = RateLimiter.hit(limiter, testIps.client1)
          expect(info1.isLimited)->toBe(false)
          expect(info1.remaining)->toBe(1)

          let info2 = RateLimiter.hit(limiter, testIps.client1)
          expect(info2.isLimited)->toBe(false)
          expect(info2.remaining)->toBe(0)
        }
      | None => Expect.fail("Limiter not initialized")
      }
    })

    it("should block requests over limit", () => {
      switch limiterRef.contents {
      | Some(limiter) => {
          let _ = RateLimiter.hit(limiter, testIps.client1)
          let _ = RateLimiter.hit(limiter, testIps.client1)
          let info = RateLimiter.hit(limiter, testIps.client1)
          expect(info.isLimited)->toBe(true)
          expect(info.remaining)->toBe(0)
        }
      | None => Expect.fail("Limiter not initialized")
      }
    })

    it("should track different keys separately", () => {
      switch limiterRef.contents {
      | Some(limiter) => {
          let _ = RateLimiter.hit(limiter, testIps.client1)
          let _ = RateLimiter.hit(limiter, testIps.client1)

          let info1 = RateLimiter.hit(limiter, testIps.client1)
          expect(info1.isLimited)->toBe(true)

          let info2 = RateLimiter.hit(limiter, testIps.client2)
          expect(info2.isLimited)->toBe(false)
          expect(info2.remaining)->toBe(1)
        }
      | None => Expect.fail("Limiter not initialized")
      }
    })

    itAsync("should reset limit after window expires", async () => {
      switch limiterRef.contents {
      | Some(limiter) => {
          let _ = RateLimiter.hit(limiter, testIps.client1)
          let _ = RateLimiter.hit(limiter, testIps.client1)

          let info = RateLimiter.hit(limiter, testIps.client1)
          expect(info.isLimited)->toBe(true)

          // Wait for window to expire
          await wait(150)

          let info2 = RateLimiter.hit(limiter, testIps.client1)
          expect(info2.isLimited)->toBe(false)
          expect(info2.remaining)->toBe(1)
        }
      | None => Expect.fail("Limiter not initialized")
      }
    })

    it("should return correct limit info", () => {
      switch limiterRef.contents {
      | Some(limiter) => {
          let info = RateLimiter.getInfo(limiter, testIps.client1)
          expect(info.limit)->toBe(2)
          expect(info.remaining)->toBe(2)
          expect(info.isLimited)->toBe(false)
        }
      | None => Expect.fail("Limiter not initialized")
      }
    })

    it("should reset specific key", () => {
      switch limiterRef.contents {
      | Some(limiter) => {
          let _ = RateLimiter.hit(limiter, testIps.client1)
          let _ = RateLimiter.hit(limiter, testIps.client1)
          RateLimiter.reset(limiter, testIps.client1)

          let info = RateLimiter.hit(limiter, testIps.client1)
          expect(info.isLimited)->toBe(false)
          expect(info.remaining)->toBe(1)
        }
      | None => Expect.fail("Limiter not initialized")
      }
    })

    it("should reset all keys", () => {
      switch limiterRef.contents {
      | Some(limiter) => {
          let _ = RateLimiter.hit(limiter, testIps.client1)
          let _ = RateLimiter.hit(limiter, testIps.client2)
          RateLimiter.resetAll(limiter)

          expect(RateLimiter.size(limiter))->toBe(0)

          let info1 = RateLimiter.getInfo(limiter, testIps.client1)
          let info2 = RateLimiter.getInfo(limiter, testIps.client2)
          expect(info1.remaining)->toBe(2)
          expect(info2.remaining)->toBe(2)
        }
      | None => Expect.fail("Limiter not initialized")
      }
    })

    it("should track store size", () => {
      switch limiterRef.contents {
      | Some(limiter) => {
          expect(RateLimiter.size(limiter))->toBe(0)

          let _ = RateLimiter.hit(limiter, testIps.client1)
          expect(RateLimiter.size(limiter))->toBe(1)

          let _ = RateLimiter.hit(limiter, testIps.client2)
          expect(RateLimiter.size(limiter))->toBe(2)
        }
      | None => Expect.fail("Limiter not initialized")
      }
    })
  })

  describe("requireRateLimit", () => {
    let limiterRef: ref<option<RateLimiter.rateLimiter>> = ref(None)

    beforeEach(() => {
      limiterRef := Some(RateLimiter.make(~config=testSingle))
    })

    afterEach(() => {
      switch limiterRef.contents {
      | Some(l) => RateLimiter.stop(l)
      | None => ()
      }
    })

    it("should not throw when within limit", () => {
      switch limiterRef.contents {
      | Some(limiter) => {
          let fn = () => RateLimiter.requireRateLimit(limiter, "test")
          expect(fn)->(not_)->toThrow
        }
      | None => Expect.fail("Limiter not initialized")
      }
    })

    it("should throw when over limit", () => {
      switch limiterRef.contents {
      | Some(limiter) => {
          RateLimiter.requireRateLimit(limiter, "test")
          let fn = () => RateLimiter.requireRateLimit(limiter, "test")
          expect(fn)->toThrow
        }
      | None => Expect.fail("Limiter not initialized")
      }
    })
  })

  describe("presets", () => {
    it("should have all expected presets", () => {
      expect(RateLimiter.presets->Dict.get("strict"))->toBeDefined
      expect(RateLimiter.presets->Dict.get("standard"))->toBeDefined
      expect(RateLimiter.presets->Dict.get("relaxed"))->toBeDefined
      expect(RateLimiter.presets->Dict.get("api"))->toBeDefined
      expect(RateLimiter.presets->Dict.get("webhook"))->toBeDefined
      expect(RateLimiter.presets->Dict.get("auth"))->toBeDefined
    })

    it("should have valid configurations", () => {
      RateLimiter.presets->Dict.forEachWithKey((_name, preset) => {
        expect(preset.limit)->toBeGreaterThan(0)
        expect(preset.windowMs)->toBeGreaterThan(0)
      })
    })
  })

  describe("defaultConfig", () => {
    it("should have sensible defaults", () => {
      expect(RateLimiter.defaultConfig.limit)->toBe(60)
      expect(RateLimiter.defaultConfig.windowMs)->toBe(60 * 1000)
      expect(RateLimiter.defaultConfig.headers)->toBe(true)
    })
  })
})
