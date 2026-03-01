// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Tests for authentication error types and their properties.
 */

open Vitest
open Errors

describe("auth errors", () => {
  describe("invalidTokenError", () => {
    it("should have correct code and status", () => {
      let error = invalidTokenError()
      expect(error.code)->toBe("INVALID_TOKEN")
      expect(error.statusCode)->toBe(401)
      expect(error.name)->toBe("InvalidTokenError")
    })

    it("should accept custom message", () => {
      let error = invalidTokenError(~message="Custom message")
      expect(error.message)->toBe("Custom message")
    })

    it("should have default message", () => {
      let error = invalidTokenError()
      expect(error.message)->toBe("Invalid or malformed token")
    })
  })

  describe("tokenExpiredError", () => {
    it("should include expiration date", () => {
      let expiredAt = Date.fromString("2024-01-01T00:00:00Z")
      let error = tokenExpiredError(~expiredAt)

      expect(error.code)->toBe("TOKEN_EXPIRED")
      expect(error.statusCode)->toBe(401)
      expect(error.message)->toMatchString("2024-01-01")
    })
  })

  describe("tokenRevokedError", () => {
    it("should have correct code and status", () => {
      let error = tokenRevokedError()
      expect(error.code)->toBe("TOKEN_REVOKED")
      expect(error.statusCode)->toBe(401)
      expect(error.message)->toMatchString("revoked")
    })
  })

  describe("insufficientScopeError", () => {
    it("should include scope information", () => {
      let error = insufficientScopeError(
        ~requiredScopes=["api", "write_repository"],
        ~availableScopes=["read_repository"],
      )

      expect(error.code)->toBe("INSUFFICIENT_SCOPE")
      expect(error.statusCode)->toBe(403)
      expect(error.message)->toMatchString("api")
      expect(error.message)->toMatchString("write_repository")
    })
  })

  describe("dangerousScopeError", () => {
    it("should include dangerous scopes", () => {
      let error = dangerousScopeError(~dangerousScopes=["sudo", "admin_mode"])

      expect(error.code)->toBe("DANGEROUS_SCOPE")
      expect(error.statusCode)->toBe(403)
      expect(error.message)->toMatchString("sudo")
      expect(error.message)->toMatchString("admin_mode")
    })
  })

  describe("missingTokenError", () => {
    it("should have correct code and status", () => {
      let error = missingTokenError()
      expect(error.code)->toBe("MISSING_TOKEN")
      expect(error.statusCode)->toBe(401)
      expect(error.message)->toMatchString("No token provided")
    })
  })

  describe("webhookSignatureError", () => {
    it("should have correct code and status", () => {
      let error = webhookSignatureError()
      expect(error.code)->toBe("INVALID_WEBHOOK_SIGNATURE")
      expect(error.statusCode)->toBe(401)
    })

    it("should accept custom message", () => {
      let error = webhookSignatureError(~message="Custom validation failure")
      expect(error.message)->toBe("Custom validation failure")
    })
  })

  describe("rateLimitError", () => {
    it("should include retry-after information", () => {
      let error = rateLimitError(~retryAfter=60)
      expect(error.code)->toBe("RATE_LIMIT_EXCEEDED")
      expect(error.statusCode)->toBe(429)
      expect(error.message)->toMatchString("60 seconds")
    })
  })

  describe("userBlockedError", () => {
    it("should include user and state information", () => {
      let error = userBlockedError(~userId=12345, ~state="blocked")
      expect(error.code)->toBe("USER_BLOCKED")
      expect(error.statusCode)->toBe(403)
      expect(error.message)->toMatchString("12345")
      expect(error.message)->toMatchString("blocked")
    })
  })

  describe("errorToJSON", () => {
    it("should convert error to JSON", () => {
      let error = invalidTokenError()
      let json = errorToJSON(error)
      expect(json)->toBeDefined
    })
  })

  describe("isRetryable", () => {
    it("should return true for rate limit errors", () => {
      let error = rateLimitError(~retryAfter=60)
      expect(isRetryable(error))->toBe(true)
    })

    it("should return false for other errors", () => {
      let error = invalidTokenError()
      expect(isRetryable(error))->toBe(false)
    })
  })
})
