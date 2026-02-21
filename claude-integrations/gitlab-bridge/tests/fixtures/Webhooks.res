// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Test fixtures for GitLab webhooks.
 * Provides replayable fixtures for testing webhook handling.
 */

/**
 * Valid webhook secret for testing (hex-like format, 64 chars).
 */
let webhookSecret = "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2"

// Weak secrets for testing validation
type weakSecrets = {
  empty: string,
  short: string,
  placeholder: string,
  common: string,
  repeated: string,
}

let weakSecrets: weakSecrets = {
  empty: "",
  short: "abc",
  placeholder: "your-webhook-secret-here",
  common: "password123",
  repeated: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
}

/**
 * Creates webhook headers with proper token.
 */
let createWebhookHeaders = (
  event: string,
  ~secret: string=webhookSecret,
): Dict.t<string> => {
  Dict.fromArray([
    ("content-type", "application/json"),
    ("x-gitlab-token", secret),
    ("x-gitlab-event", event),
    ("x-gitlab-instance", "https://gitlab.com"),
    ("x-request-id", "test-request-" ++ Date.now()->Float.toString),
  ])
}

/**
 * Creates headers with invalid token for testing rejection.
 */
let createInvalidWebhookHeaders = (event: string): Dict.t<string> => {
  Dict.fromArray([
    ("content-type", "application/json"),
    ("x-gitlab-token", "invalid-secret-that-should-not-match"),
    ("x-gitlab-event", event),
  ])
}

/**
 * Creates headers with missing token for testing rejection.
 */
let createMissingTokenHeaders = (event: string): Dict.t<string> => {
  Dict.fromArray([
    ("content-type", "application/json"),
    ("x-gitlab-event", event),
  ])
}

// Push event webhook payload (simplified)
let pushHookPayload = `{"object_kind":"push","event_name":"push","ref":"refs/heads/main"}`

// All supported webhook events
let allWebhookEvents = [
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
