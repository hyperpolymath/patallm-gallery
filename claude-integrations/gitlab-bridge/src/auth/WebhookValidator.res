// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Webhook validation for GitLab webhooks.
 *
 * Validates webhook tokens (timing-safe), computes and validates
 * HMAC-SHA256 signatures, and extracts webhook metadata from headers.
 */

open Types

/**
 * Header name for the GitLab webhook secret token.
 */
let webhookTokenHeader = "x-gitlab-token"

/**
 * Header name for the GitLab webhook event type.
 */
let webhookEventHeader = "x-gitlab-event"

/**
 * Header name for the GitLab instance URL.
 */
let webhookInstanceHeader = "x-gitlab-instance"

/**
 * Supported webhook event types.
 */
let webhookEvents = [
  "Push Hook",
  "Tag Push Hook",
  "Issue Hook",
  "Confidential Issue Hook",
  "Note Hook",
  "Confidential Note Hook",
  "Merge Request Hook",
  "Wiki Page Hook",
  "Pipeline Hook",
  "Job Hook",
  "Deployment Hook",
  "Feature Flag Hook",
  "Release Hook",
  "Emoji Hook",
  "Member Hook",
  "Subgroup Hook",
]

/**
 * Validates a webhook token matches the expected secret.
 * Uses timing-safe comparison to prevent timing attacks.
 */
let validateWebhookToken = (receivedToken: string, expectedSecret: string): bool => {
  // Ensure neither is empty
  if receivedToken->String.length == 0 || expectedSecret->String.length == 0 {
    false
  } else {
    Crypto.timingSafeStringCompare(receivedToken, expectedSecret)
  }
}

/**
 * Computes HMAC-SHA256 signature for webhook payload.
 * Returns hex-encoded HMAC signature.
 */
let computeWebhookSignature = (payload: string, secret: string): string => {
  Crypto.computeHmacSha256(payload, secret)
}

/**
 * Validates webhook signature using HMAC-SHA256.
 * Used for systems that use signature-based validation.
 */
let validateWebhookSignature = (payload: string, signature: string, secret: string): bool => {
  if payload->String.length == 0 || signature->String.length == 0 || secret->String.length == 0 {
    false
  } else {
    let expectedSignature = computeWebhookSignature(payload, secret)
    Crypto.timingSafeHexCompare(signature, expectedSignature)
  }
}

// Header value type: can be string, array of strings, or undefined in JS
type headerValue
external headerToString: headerValue => string = "%identity"
external headerToArray: headerValue => array<string> = "%identity"

/**
 * Get a header value, handling both string and array-of-string formats.
 */
let getHeaderValue = (
  headers: Dict.t<headerValue>,
  name: string,
): option<string> => {
  switch headers->Dict.get(name) {
  | None => None
  | Some(value) =>
    if Array.isArray(value) {
      let arr = headerToArray(value)
      arr->Array.get(0)
    } else {
      Some(headerToString(value))
    }
  }
}

/**
 * Validates a complete webhook request.
 * Checks secret configuration, token presence, token validity,
 * and event type recognition.
 */
let validateWebhookRequest = (
  headers: Dict.t<headerValue>,
  _body: string,
  secret: string,
): webhookValidationResult => {
  // Check for required secret
  if secret->String.trim->String.length == 0 {
    {valid: false, event: None, reason: Some("Webhook secret not configured")}
  } else {
    // Get the token from headers
    let token =
      getHeaderValue(headers, webhookTokenHeader)->Option.orElse(
        getHeaderValue(headers, "X-Gitlab-Token"),
      )

    switch token {
    | None => {
        valid: false,
        event: None,
        reason: Some("Missing webhook token header"),
      }
    | Some(tokenValue) =>
      if !validateWebhookToken(tokenValue, secret) {
        {valid: false, event: None, reason: Some("Invalid webhook token")}
      } else {
        // Get the event type
        let event =
          getHeaderValue(headers, webhookEventHeader)->Option.orElse(
            getHeaderValue(headers, "X-Gitlab-Event"),
          )

        // Validate event type if present
        switch event {
        | Some(evt) if !(webhookEvents->Array.includes(evt)) => {
            valid: false,
            event: Some(evt),
            reason: Some(`Unknown webhook event type: ${evt}`),
          }
        | _ => {valid: true, event, reason: None}
        }
      }
    }
  }
}

/**
 * Validates and raises exception if webhook request is invalid.
 */
let requireValidWebhook = (
  headers: Dict.t<headerValue>,
  body: string,
  secret: string,
): unit => {
  let result = validateWebhookRequest(headers, body, secret)

  if !result.valid {
    raise(Errors.WebhookSignatureError(result.reason->Option.getOr("Invalid webhook signature")))
  }
}

// Webhook metadata extracted from headers
type webhookMetadata = {
  event: option<string>,
  instance: option<string>,
  requestId: option<string>,
}

/**
 * Extracts webhook metadata from headers.
 */
let extractWebhookMetadata = (
  headers: Dict.t<headerValue>,
): webhookMetadata => {
  {
    event: getHeaderValue(headers, webhookEventHeader)->Option.orElse(
      getHeaderValue(headers, "X-Gitlab-Event"),
    ),
    instance: getHeaderValue(headers, webhookInstanceHeader)->Option.orElse(
      getHeaderValue(headers, "X-Gitlab-Instance"),
    ),
    requestId: getHeaderValue(headers, "x-request-id"),
  }
}

// Secret strength validation result
type secretStrengthResult = {
  valid: bool,
  issues: array<string>,
}

/**
 * Validates minimum secret strength.
 * Checks length, character diversity, and common weak patterns.
 */
let validateSecretStrength = (secret: string): secretStrengthResult => {
  let issues: array<string> = []

  if secret->String.length == 0 {
    {valid: false, issues: ["Secret is empty"]}
  } else {
    if secret->String.length < 32 {
      let _ = issues->Array.push("Secret should be at least 32 characters")
    }

    // Check for letters-only (via regex)
    let alphaOnly = %re("/^[a-z]+$/i")
    if alphaOnly->RegExp.test(secret) {
      let _ = issues->Array.push("Secret should contain mixed character types")
    }

    // Check for repeated character
    let repeatedChar = %re("/^(.)\1+$/")
    if repeatedChar->RegExp.test(secret) {
      let _ = issues->Array.push("Secret should not be a repeated character")
    }

    // Check for common weak patterns
    let commonSecrets = [
      "your-webhook-secret-here",
      "secret",
      "password",
      "webhook",
      "test",
    ]
    let lowerSecret = secret->String.toLowerCase
    let hasCommon = commonSecrets->Array.some(s => lowerSecret->String.includes(s))
    if hasCommon {
      let _ = issues->Array.push("Secret appears to be a placeholder or common value")
    }

    {valid: issues->Array.length == 0, issues}
  }
}
