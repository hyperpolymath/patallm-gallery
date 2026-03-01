// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Tests for permission checking and scope validation.
 */

open Vitest
open Types

describe("permission-checker", () => {
  describe("checkScopeSatisfaction", () => {
    it("should allow when required scope is present", () => {
      let result = PermissionChecker.checkScopeSatisfaction(
        ~availableScopes=[Api, ReadRepository],
        ~requiredScopes=[Api],
      )
      expect(result.allowed)->toBe(true)
      expect(result.missingScopes)->toEqual([])
    })

    it("should allow when any of the required scopes is present (OR logic)", () => {
      let result = PermissionChecker.checkScopeSatisfaction(
        ~availableScopes=[ReadRepository],
        ~requiredScopes=[Api, ReadRepository],
      )
      expect(result.allowed)->toBe(true)
    })

    it("should deny when no required scope is present", () => {
      let result = PermissionChecker.checkScopeSatisfaction(
        ~availableScopes=[ReadUser],
        ~requiredScopes=[Api, ReadRepository],
      )
      expect(result.allowed)->toBe(false)
      expect(result.missingScopes)->toEqual([Api, ReadRepository])
    })

    it("should allow with empty required scopes", () => {
      let result = PermissionChecker.checkScopeSatisfaction(
        ~availableScopes=[Api],
        ~requiredScopes=[],
      )
      expect(result.allowed)->toBe(true)
    })

    it("should include reason when denied", () => {
      let result = PermissionChecker.checkScopeSatisfaction(
        ~availableScopes=[],
        ~requiredScopes=[Api],
      )
      expect(result.allowed)->toBe(false)
      switch result.reason {
      | Some(reason) => expect(reason)->toMatchString("Missing required scope")
      | None => Expect.fail("Expected reason to be present")
      }
    })
  })

  describe("checkOperationPermission", () => {
    it("should allow repository:read with api scope", () => {
      let result = PermissionChecker.checkOperationPermission(
        ~operation="repository:read",
        ~availableScopes=[Api],
      )
      expect(result.allowed)->toBe(true)
    })

    it("should allow repository:read with read_repository scope", () => {
      let result = PermissionChecker.checkOperationPermission(
        ~operation="repository:read",
        ~availableScopes=[ReadRepository],
      )
      expect(result.allowed)->toBe(true)
    })

    it("should deny repository:write without write scope", () => {
      let result = PermissionChecker.checkOperationPermission(
        ~operation="repository:write",
        ~availableScopes=[ReadRepository],
      )
      expect(result.allowed)->toBe(false)
    })

    it("should handle unknown operations", () => {
      let result = PermissionChecker.checkOperationPermission(
        ~operation="unknown:operation",
        ~availableScopes=[Api],
      )
      // Unknown operations return empty requiredScopes, so they are allowed
      // with empty requirements
      expect(result.allowed)->toBe(true)
    })

    it("should check merge_request:create requires api or write_repository", () => {
      let withBoth = PermissionChecker.checkOperationPermission(
        ~operation="merge_request:create",
        ~availableScopes=[Api, WriteRepository],
      )
      expect(withBoth.allowed)->toBe(true)

      let withApiOnly = PermissionChecker.checkOperationPermission(
        ~operation="merge_request:create",
        ~availableScopes=[Api],
      )
      expect(withApiOnly.allowed)->toBe(true)

      let withReadOnly = PermissionChecker.checkOperationPermission(
        ~operation="merge_request:create",
        ~availableScopes=[ReadRepository],
      )
      expect(withReadOnly.allowed)->toBe(false)
    })
  })

  describe("validateRequiredScopes", () => {
    it("should not return error with all required scopes", () => {
      let result = PermissionChecker.validateRequiredScopes(
        ~availableScopes=[Api, ReadRepository, WriteRepository],
      )
      switch result {
      | Ok(_) => expect(true)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should return error when scopes are missing", () => {
      let result = PermissionChecker.validateRequiredScopes(
        ~availableScopes=[ReadApi, ReadRepository, ReadUser],
      )
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.code)->toBe("INSUFFICIENT_SCOPE")
      }
    })

    it("should return error with empty scopes", () => {
      let result = PermissionChecker.validateRequiredScopes(~availableScopes=[])
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.code)->toBe("INSUFFICIENT_SCOPE")
      }
    })
  })

  describe("validateNoDangerousScopes", () => {
    it("should not return error with safe scopes", () => {
      let result = PermissionChecker.validateNoDangerousScopes(
        ~scopes=[Api, ReadRepository, WriteRepository],
      )
      switch result {
      | Ok(_) => expect(true)->toBe(true)
      | Error(_) => Expect.fail("Expected Ok result")
      }
    })

    it("should return error with sudo scope", () => {
      let result = PermissionChecker.validateNoDangerousScopes(
        ~scopes=[Api, Sudo, AdminMode],
      )
      switch result {
      | Ok(_) => Expect.fail("Expected Error result")
      | Error(err) => expect(err.code)->toBe("DANGEROUS_SCOPE")
      }
    })
  })

  describe("requirePermission", () => {
    it("should not throw when permission is granted", () => {
      let fn = () => {
        PermissionChecker.requirePermission(
          ~operation="repository:read",
          ~availableScopes=[Api],
        )
      }
      expect(fn)->(not_)->toThrow
    })

    it("should throw when permission denied", () => {
      let fn = () => {
        PermissionChecker.requirePermission(
          ~operation="repository:write",
          ~availableScopes=[ReadRepository],
        )
      }
      expect(fn)->toThrow
    })
  })

  describe("checkBridgeScopes", () => {
    it("should report complete when all bridge scopes present", () => {
      let result = PermissionChecker.checkBridgeScopes(
        ~availableScopes=[Api, ReadRepository, WriteRepository],
      )
      expect(result.allowed)->toBe(true)
      expect(result.missingScopes)->toEqual([])
    })

    it("should report incomplete with missing scopes", () => {
      let result = PermissionChecker.checkBridgeScopes(~availableScopes=[Api])
      expect(result.allowed)->toBe(false)
    })

    it("should report incomplete with empty scopes", () => {
      let result = PermissionChecker.checkBridgeScopes(~availableScopes=[])
      expect(result.allowed)->toBe(false)
      expect(result.missingScopes->Array.length)->toBeGreaterThan(0)
    })
  })

  describe("getRequiredScopesForOperations", () => {
    it("should return scopes for multiple operations", () => {
      let scopes = PermissionChecker.getRequiredScopesForOperations([
        "repository:read",
        "repository:write",
      ])
      expect(scopes->Array.length)->toBeGreaterThan(0)
    })

    it("should return empty for empty operations", () => {
      let scopes = PermissionChecker.getRequiredScopesForOperations([])
      expect(scopes)->toEqual([])
    })
  })

  describe("checkMultipleOperations", () => {
    it("should check all operations and return results", () => {
      let results = PermissionChecker.checkMultipleOperations(
        ~operations=["repository:read", "repository:write"],
        ~availableScopes=[Api],
      )
      expect(results->Array.length)->toBe(2)
    })

    it("should handle mixed results", () => {
      let results = PermissionChecker.checkMultipleOperations(
        ~operations=["repository:read", "repository:write"],
        ~availableScopes=[ReadRepository],
      )

      let readResult = results->Array.find(((op, _)) => op == "repository:read")
      let writeResult = results->Array.find(((op, _)) => op == "repository:write")

      switch readResult {
      | Some((_, r)) => expect(r.allowed)->toBe(true)
      | None => Expect.fail("Expected read result")
      }

      switch writeResult {
      | Some((_, r)) => expect(r.allowed)->toBe(false)
      | None => Expect.fail("Expected write result")
      }
    })
  })
})
