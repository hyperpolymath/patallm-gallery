// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Token validation functions
 */

open Types
open Errors

// Constants
let minTokenLength = 20
let maxTokenLength = 256

// Regular expression for valid token characters
@val @scope("RegExp") external makeTokenPattern: string => Js.Re.t = "RegExp"
let tokenCharPattern = makeTokenPattern("^[a-zA-Z0-9_-]+$")

/**
 * Mask a token for safe logging
 * Shows only first 8 and last 4 characters
 */
let maskToken = (token: string): string => {
  let len = token->String.length
  if len <= 12 {
    // For very short tokens, just show ***
    "***"
  } else {
    let prefix = token->String.slice(~start=0, ~end=8)
    let suffix = token->String.sliceToEnd(~start=len - 4)
    `${prefix}...${suffix}`
  }
}

/**
 * Determine token type from prefix
 */
let getTokenType = (token: string): option<tokenType> => {
  let prefixes = tokenPrefixes
  if token->String.startsWith(prefixes.personal) || token->String.startsWith(prefixes.project) {
    Some(Personal)
  } else if token->String.startsWith(prefixes.deploy) {
    Some(Deploy)
  } else if token->String.startsWith(prefixes.runner) {
    Some(Runner)
  } else if token->String.startsWith(prefixes.job) {
    Some(Job)
  } else if token->String.startsWith(prefixes.featureFlag) {
    Some(FeatureFlag)
  } else if token->String.startsWith(prefixes.email) {
    Some(Email)
  } else if token->String.startsWith(prefixes.agent) {
    Some(Agent)
  } else if token->String.startsWith(prefixes.oauth) {
    Some(OAuth)
  } else if token->String.startsWith(prefixes.scim) {
    Some(Scim)
  } else {
    None
  }
}

/**
 * Get prefix string for a token type
 */
let getPrefixForType = (tokenType: tokenType): string => {
  let prefixes = tokenPrefixes
  switch tokenType {
  | Personal | Project => prefixes.personal
  | Group => prefixes.group
  | Deploy => prefixes.deploy
  | Runner => prefixes.runner
  | Job => prefixes.job
  | FeatureFlag => prefixes.featureFlag
  | Email => prefixes.email
  | Agent => prefixes.agent
  | OAuth => prefixes.oauth
  | Scim => prefixes.scim
  }
}

/**
 * Validate token format
 * Returns tokenInfo or raises InvalidToken exception
 */
let validateTokenFormat = (token: string): result<tokenInfo, authError> => {
  // Check for empty or whitespace-only tokens
  if token->String.trim->String.length == 0 {
    Error(invalidTokenError(~message="Token cannot be empty"))
  } else {
    let trimmedToken = token->String.trim

    // Length checks
    if trimmedToken->String.length < minTokenLength {
      Error(
        invalidTokenError(
          ~message=`Token too short (minimum ${minTokenLength->Int.toString} characters)`,
        ),
      )
    } else if trimmedToken->String.length > maxTokenLength {
      Error(
        invalidTokenError(
          ~message=`Token too long (maximum ${maxTokenLength->Int.toString} characters)`,
        ),
      )
    } else {
      // Determine token type from prefix
      switch getTokenType(trimmedToken) {
      | None =>
        Error(
          invalidTokenError(
            ~message="Invalid token prefix. Expected glpat-, gldt-, glrt-, glcbt-, or similar GitLab token prefix.",
          ),
        )
      | Some(tokenType) => {
          let prefix = getPrefixForType(tokenType)
          let tokenPart = trimmedToken->String.sliceToEnd(~start=prefix->String.length)

          // Validate token characters using regex test
          let isValidChars = tokenCharPattern->Js.Re.test_(tokenPart)

          if !isValidChars {
            Error(
              invalidTokenError(
                ~message="Token contains invalid characters. Only alphanumeric characters, hyphens, and underscores are allowed.",
              ),
            )
          } else {
            Ok({
              maskedToken: maskToken(trimmedToken),
              tokenType,
              isValid: true,
              validatedAt: Date.make(),
            })
          }
        }
      }
    }
  }
}

/**
 * Parse token info from GitLab API response
 */
let parseTokenInfo = (apiResponse: gitLabTokenInfo): tokenInfo => {
  // For now, assume Personal type - in real implementation
  // this would be determined from the API response
  {
    maskedToken: "***",
    tokenType: Personal,
    isValid: true,
    validatedAt: Date.make(),
  }
}

/**
 * Check if token has expired
 */
let checkTokenExpiration = (expiresAt: option<string>): result<unit, authError> => {
  switch expiresAt {
  | None => Ok() // No expiration
  | Some(expiryStr) => {
      let expiry = Date.fromString(expiryStr)
      let now = Date.make()

      if expiry < now {
        Error(tokenExpiredError(~expiredAt=expiry))
      } else {
        Ok()
      }
    }
  }
}

/**
 * Check if token is revoked
 */
let checkTokenRevocation = (revoked: option<bool>): result<unit, authError> => {
  switch revoked {
  | Some(true) => Error(tokenRevokedError())
  | _ => Ok()
  }
}

/**
 * Check for dangerous scopes
 */
let checkDangerousScopes = (scopes: array<gitLabScope>): result<unit, authError> => {
  let dangerous = scopes->Array.filter(scope => dangerousScopes->Array.includes(scope))

  if dangerous->Array.length > 0 {
    Error(dangerousScopeError(~dangerousScopes=dangerous->Array.map(scopeToString)))
  } else {
    Ok()
  }
}

/**
 * Check if token expiration warning should be shown
 */
let checkExpirationWarning = (expiresAt: option<string>): bool => {
  switch expiresAt {
  | None => false
  | Some(expiryStr) => {
      let expiry = Date.fromString(expiryStr)
      let now = Date.make()
      let sevenDaysMs = 7.0 *. 24.0 *. 60.0 *. 60.0 *. 1000.0
      let warningTime = Date.fromTime(expiry->Date.getTime +. sevenDaysMs)

      now > warningTime
    }
  }
}
