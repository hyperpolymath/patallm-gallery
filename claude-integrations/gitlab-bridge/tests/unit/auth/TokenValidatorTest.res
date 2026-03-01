// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Tests for token validation functions.
 */

open Vitest
open Tokens

describe("token-validator", () => {
  describe("validateTokenFormat", () => {
    it("should accept valid personal access token", () => {
      let result = TokenValidator.validateTokenFormat(validTokens.personal)
      switch result {
      | Ok(info) => expect(info.isValid)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should accept valid deploy token", () => {
      let result = TokenValidator.validateTokenFormat(validTokens.deploy)
      switch result {
      | Ok(info) => expect(info.isValid)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should accept valid runner token", () => {
      let result = TokenValidator.validateTokenFormat(validTokens.runner)
      switch result {
      | Ok(info) => expect(info.isValid)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should accept valid job token", () => {
      let result = TokenValidator.validateTokenFormat(validTokens.job)
      switch result {
      | Ok(info) => expect(info.isValid)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should accept valid agent token", () => {
      let result = TokenValidator.validateTokenFormat(validTokens.agent)
      switch result {
      | Ok(info) => expect(info.isValid)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should accept token at minimum length", () => {
      let result = TokenValidator.validateTokenFormat(validTokens.minLength)
      switch result {
      | Ok(info) => expect(info.isValid)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should accept long valid token", () => {
      let result = TokenValidator.validateTokenFormat(validTokens.long)
      switch result {
      | Ok(info) => expect(info.isValid)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should trim whitespace from token", () => {
      let result = TokenValidator.validateTokenFormat(
        "  " ++ validTokens.personal ++ "  ",
      )
      switch result {
      | Ok(info) => expect(info.isValid)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should reject empty token", () => {
      let result = TokenValidator.validateTokenFormat(invalidTokens.empty)
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.code)->toBe("INVALID_TOKEN")
      }
    })

    it("should reject whitespace-only token", () => {
      let result = TokenValidator.validateTokenFormat(invalidTokens.whitespace)
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.code)->toBe("INVALID_TOKEN")
      }
    })

    it("should reject token that is too short", () => {
      let result = TokenValidator.validateTokenFormat(invalidTokens.tooShort)
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.message)->toMatchString("too short")
      }
    })

    it("should reject token that is too long", () => {
      let result = TokenValidator.validateTokenFormat(invalidTokens.tooLong)
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.message)->toMatchString("too long")
      }
    })

    it("should reject token with invalid prefix", () => {
      let result = TokenValidator.validateTokenFormat(invalidTokens.invalidPrefix)
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.message)->toMatchString("prefix")
      }
    })

    it("should reject token without prefix", () => {
      let result = TokenValidator.validateTokenFormat(invalidTokens.noPrefix)
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.code)->toBe("INVALID_TOKEN")
      }
    })

    it("should reject token with invalid characters", () => {
      let result = TokenValidator.validateTokenFormat(invalidTokens.invalidChars)
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.message)->toMatchString("invalid characters")
      }
    })

    it("should reject token with spaces", () => {
      let result = TokenValidator.validateTokenFormat(invalidTokens.withSpaces)
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.code)->toBe("INVALID_TOKEN")
      }
    })

    it("should include validation timestamp", () => {
      let before = Date.make()
      let result = TokenValidator.validateTokenFormat(validTokens.personal)
      let after = Date.make()

      switch result {
      | Ok(info) => {
          expect(info.validatedAt->Date.getTime)->toBeGreaterThanOrEqual(
            before->Date.getTime,
          )
          expect(info.validatedAt->Date.getTime)->toBeLessThanOrEqual(
            after->Date.getTime,
          )
        }
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })
  })

  describe("getTokenType", () => {
    it("should identify personal access token", () => {
      let result = TokenValidator.getTokenType(validTokens.personal)
      switch result {
      | Some(Types.Personal) => expect(true)->toBe(true)
      | _ => Expect.fail("Expected Some(Personal)")
      }
    })

    it("should identify deploy token", () => {
      let result = TokenValidator.getTokenType(validTokens.deploy)
      switch result {
      | Some(Types.Deploy) => expect(true)->toBe(true)
      | _ => Expect.fail("Expected Some(Deploy)")
      }
    })

    it("should identify runner token", () => {
      let result = TokenValidator.getTokenType(validTokens.runner)
      switch result {
      | Some(Types.Runner) => expect(true)->toBe(true)
      | _ => Expect.fail("Expected Some(Runner)")
      }
    })

    it("should return None for unknown prefix", () => {
      let result = TokenValidator.getTokenType("unknown-token")
      switch result {
      | None => expect(true)->toBe(true)
      | Some(_) => Expect.fail("Expected None")
      }
    })
  })

  describe("maskToken", () => {
    it("should mask token showing prefix and suffix", () => {
      let masked = TokenValidator.maskToken(validTokens.personal)
      expect(masked)->toMatch(%re("/^glpat-xx\.\.\.xxxx$/"))
    })

    it("should fully mask short tokens", () => {
      let masked = TokenValidator.maskToken("short")
      expect(masked)->toBe("***")
    })

    it("should handle long tokens", () => {
      let masked = TokenValidator.maskToken(validTokens.long)
      expect(masked->String.length)->toBeLessThan(validTokens.long->String.length)
      expect(masked)->toMatchString("...")
    })
  })

  describe("checkTokenExpiration", () => {
    it("should not return error for non-expired token", () => {
      let result = TokenValidator.checkTokenExpiration(Some(expiresInDays(30)))
      switch result {
      | Ok(_) => expect(true)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should not return error for token with no expiration", () => {
      let result = TokenValidator.checkTokenExpiration(None)
      switch result {
      | Ok(_) => expect(true)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should return error for expired token", () => {
      let result = TokenValidator.checkTokenExpiration(Some(expiredDaysAgo(1)))
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.code)->toBe("TOKEN_EXPIRED")
      }
    })
  })

  describe("checkTokenRevocation", () => {
    it("should not return error for active non-revoked token", () => {
      let result = TokenValidator.checkTokenRevocation(Some(false))
      switch result {
      | Ok(_) => expect(true)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should return error for revoked token", () => {
      let result = TokenValidator.checkTokenRevocation(Some(true))
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.code)->toBe("TOKEN_REVOKED")
      }
    })
  })

  describe("checkDangerousScopes", () => {
    it("should not return error for safe scopes", () => {
      let result = TokenValidator.checkDangerousScopes([
        Types.Api,
        Types.ReadRepository,
        Types.WriteRepository,
      ])
      switch result {
      | Ok(_) => expect(true)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should return error for sudo scope", () => {
      let result = TokenValidator.checkDangerousScopes([Types.Api, Types.Sudo])
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.code)->toBe("DANGEROUS_SCOPE")
      }
    })

    it("should return error for admin_mode scope", () => {
      let result = TokenValidator.checkDangerousScopes([Types.Api, Types.AdminMode])
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.code)->toBe("DANGEROUS_SCOPE")
      }
    })
  })
})
