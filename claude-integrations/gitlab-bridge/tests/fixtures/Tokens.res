// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Test fixtures for GitLab tokens.
 * These are fake tokens for testing purposes only - never use in production.
 */

// Valid token fixtures for testing
type validTokens = {
  personal: string,
  personalWithDashes: string,
  deploy: string,
  runner: string,
  job: string,
  featureFlag: string,
  agent: string,
  long: string,
  minLength: string,
}

let validTokens: validTokens = {
  personal: "glpat-xxxxxxxxxxxxxxxxxxxx",
  personalWithDashes: "glpat-abc123-def456-ghi789",
  deploy: "gldt-xxxxxxxxxxxxxxxxxxxx",
  runner: "glrt-xxxxxxxxxxxxxxxxxxxx",
  job: "glcbt-xxxxxxxxxxxxxxxxxxxx",
  featureFlag: "glffct-xxxxxxxxxxxxxxxxxxxx",
  agent: "glagent-xxxxxxxxxxxxxxxxxxxx",
  long: "glpat-" ++ String.repeat("x", 100),
  minLength: "glpat-xxxxxxxxxxxxxxx",
}

// Invalid token fixtures for testing
type invalidTokens = {
  empty: string,
  whitespace: string,
  tooShort: string,
  tooLong: string,
  invalidPrefix: string,
  noPrefix: string,
  invalidChars: string,
  withSpaces: string,
  onlyPrefix: string,
  nullString: string,
  undefinedString: string,
}

let invalidTokens: invalidTokens = {
  empty: "",
  whitespace: "   ",
  tooShort: "glpat-xxx",
  tooLong: "glpat-" ++ String.repeat("x", 300),
  invalidPrefix: "invalid-xxxxxxxxxxxxxxxxxxxx",
  noPrefix: "xxxxxxxxxxxxxxxxxxxx",
  invalidChars: "glpat-xxx!@#$%^&*()",
  withSpaces: "glpat-xxxx xxxx xxxx",
  onlyPrefix: "glpat-",
  nullString: "null",
  undefinedString: "undefined",
}

// Token information response fixtures
type tokenInfoResponse = {
  scopes: array<string>,
  created_at: string,
  expires_at: Nullable.t<string>,
  user_id: int,
  active: bool,
  revoked: bool,
}

let validActive: tokenInfoResponse = {
  scopes: ["api", "read_repository", "write_repository"],
  created_at: "2024-01-01T00:00:00Z",
  expires_at: Nullable.Value("2025-12-31T23:59:59Z"),
  user_id: 12345,
  active: true,
  revoked: false,
}

let noExpiration: tokenInfoResponse = {
  scopes: ["api", "read_repository"],
  created_at: "2024-01-01T00:00:00Z",
  expires_at: Nullable.null,
  user_id: 12345,
  active: true,
  revoked: false,
}

let expired: tokenInfoResponse = {
  scopes: ["api"],
  created_at: "2023-01-01T00:00:00Z",
  expires_at: Nullable.Value("2023-12-31T23:59:59Z"),
  user_id: 12345,
  active: false,
  revoked: false,
}

let revoked: tokenInfoResponse = {
  scopes: ["api"],
  created_at: "2024-01-01T00:00:00Z",
  expires_at: Nullable.Value("2025-12-31T23:59:59Z"),
  user_id: 12345,
  active: false,
  revoked: true,
}

let dangerous: tokenInfoResponse = {
  scopes: ["api", "sudo", "admin_mode"],
  created_at: "2024-01-01T00:00:00Z",
  expires_at: Nullable.null,
  user_id: 12345,
  active: true,
  revoked: false,
}

let minimalScopes: tokenInfoResponse = {
  scopes: ["read_api"],
  created_at: "2024-01-01T00:00:00Z",
  expires_at: Nullable.null,
  user_id: 12345,
  active: true,
  revoked: false,
}

let readOnly: tokenInfoResponse = {
  scopes: ["read_api", "read_repository", "read_user"],
  created_at: "2024-01-01T00:00:00Z",
  expires_at: Nullable.null,
  user_id: 12345,
  active: true,
  revoked: false,
}

// Scope combinations for permission testing
let scopeSets: Dict.t<array<string>> = Dict.fromArray([
  ("fullBridge", ["api", "read_repository", "write_repository"]),
  ("readOnly", ["read_api", "read_repository", "read_user"]),
  ("writeOnly", ["api", "write_repository"]),
  ("minimalApi", ["read_api"]),
  (
    "all",
    [
      "api",
      "read_api",
      "read_user",
      "read_repository",
      "write_repository",
      "read_registry",
      "write_registry",
    ],
  ),
  ("dangerous", ["api", "sudo", "admin_mode"]),
  ("empty", []),
  ("cicd", ["api", "read_repository", "create_runner"]),
])

/**
 * Creates an expiration date a certain number of days from now.
 */
let expiresInDays = (days: int): string => {
  let ms = Date.now() +. Float.fromInt(days) *. 24.0 *. 60.0 *. 60.0 *. 1000.0
  Date.fromTime(ms)->Date.toISOString
}

/**
 * Creates an expiration date a certain number of days ago.
 */
let expiredDaysAgo = (days: int): string => {
  let ms = Date.now() -. Float.fromInt(days) *. 24.0 *. 60.0 *. 60.0 *. 1000.0
  Date.fromTime(ms)->Date.toISOString
}
