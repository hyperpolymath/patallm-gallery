// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Tests for webhook validation functions.
 */

open Vitest
open Webhooks

// Helper to convert Dict.t<string> to Dict.t<WebhookValidator.headerValue>
external toHeaderDict: Dict.t<string> => Dict.t<WebhookValidator.headerValue> = "%identity"

describe("webhook-validator", () => {
  describe("validateWebhookToken", () => {
    it("should return true for matching tokens", () => {
      expect(
        WebhookValidator.validateWebhookToken(webhookSecret, webhookSecret),
      )->toBe(true)
    })

    it("should return false for non-matching tokens", () => {
      expect(
        WebhookValidator.validateWebhookToken("wrong-secret", webhookSecret),
      )->toBe(false)
    })

    it("should return false for empty received token", () => {
      expect(
        WebhookValidator.validateWebhookToken("", webhookSecret),
      )->toBe(false)
    })

    it("should return false for empty expected secret", () => {
      expect(
        WebhookValidator.validateWebhookToken(webhookSecret, ""),
      )->toBe(false)
    })

    it("should handle different length tokens safely (timing-safe)", () => {
      expect(
        WebhookValidator.validateWebhookToken("short", webhookSecret),
      )->toBe(false)
      expect(
        WebhookValidator.validateWebhookToken(
          webhookSecret ++ "extra",
          webhookSecret,
        ),
      )->toBe(false)
    })
  })

  describe("computeWebhookSignature", () => {
    it("should compute consistent HMAC-SHA256 signature", () => {
      let payload = pushHookPayload
      let sig1 = WebhookValidator.computeWebhookSignature(payload, webhookSecret)
      let sig2 = WebhookValidator.computeWebhookSignature(payload, webhookSecret)

      expect(sig1)->toBe(sig2)
      expect(sig1)->toMatch(%re("/^[a-f0-9]{64}$/"))
    })

    it("should produce different signatures for different payloads", () => {
      let sig1 = WebhookValidator.computeWebhookSignature(
        "payload1",
        webhookSecret,
      )
      let sig2 = WebhookValidator.computeWebhookSignature(
        "payload2",
        webhookSecret,
      )
      expect(sig1)->(not_)->toBe(sig2)
    })

    it("should produce different signatures for different secrets", () => {
      let payload = "same-payload"
      let sig1 = WebhookValidator.computeWebhookSignature(payload, "secret1")
      let sig2 = WebhookValidator.computeWebhookSignature(payload, "secret2")
      expect(sig1)->(not_)->toBe(sig2)
    })
  })

  describe("validateWebhookSignature", () => {
    it("should return true for valid signature", () => {
      let payload = pushHookPayload
      let signature = WebhookValidator.computeWebhookSignature(
        payload,
        webhookSecret,
      )
      expect(
        WebhookValidator.validateWebhookSignature(
          payload,
          signature,
          webhookSecret,
        ),
      )->toBe(true)
    })

    it("should return false for invalid signature", () => {
      let payload = pushHookPayload
      expect(
        WebhookValidator.validateWebhookSignature(
          payload,
          "invalidsig",
          webhookSecret,
        ),
      )->toBe(false)
    })

    it("should return false for tampered payload", () => {
      let payload = pushHookPayload
      let signature = WebhookValidator.computeWebhookSignature(
        payload,
        webhookSecret,
      )
      let tamperedPayload = payload ++ "tampered"
      expect(
        WebhookValidator.validateWebhookSignature(
          tamperedPayload,
          signature,
          webhookSecret,
        ),
      )->toBe(false)
    })

    it("should return false for empty inputs", () => {
      expect(
        WebhookValidator.validateWebhookSignature("", "sig", webhookSecret),
      )->toBe(false)
      expect(
        WebhookValidator.validateWebhookSignature("payload", "", webhookSecret),
      )->toBe(false)
      expect(
        WebhookValidator.validateWebhookSignature("payload", "sig", ""),
      )->toBe(false)
    })
  })

  describe("validateWebhookRequest", () => {
    it("should validate request with valid token", () => {
      let headers = createWebhookHeaders("Push Hook")->toHeaderDict
      let body = pushHookPayload

      let result = WebhookValidator.validateWebhookRequest(
        headers,
        body,
        webhookSecret,
      )
      expect(result.valid)->toBe(true)
      expect(result.event)->toEqual(Some("Push Hook"))
    })

    it("should reject request with invalid token", () => {
      let headers = createInvalidWebhookHeaders("Push Hook")->toHeaderDict
      let body = pushHookPayload

      let result = WebhookValidator.validateWebhookRequest(
        headers,
        body,
        webhookSecret,
      )
      expect(result.valid)->toBe(false)
      switch result.reason {
      | Some(reason) => expect(reason)->toMatchString("Invalid webhook token")
      | None => Expect.fail("Expected reason")
      }
    })

    it("should reject request with missing token", () => {
      let headers = createMissingTokenHeaders("Push Hook")->toHeaderDict
      let body = pushHookPayload

      let result = WebhookValidator.validateWebhookRequest(
        headers,
        body,
        webhookSecret,
      )
      expect(result.valid)->toBe(false)
      switch result.reason {
      | Some(reason) => expect(reason)->toMatchString("Missing webhook token")
      | None => Expect.fail("Expected reason")
      }
    })

    it("should reject when secret not configured", () => {
      let headers = createWebhookHeaders("Push Hook")->toHeaderDict
      let body = pushHookPayload

      let result = WebhookValidator.validateWebhookRequest(headers, body, "")
      expect(result.valid)->toBe(false)
      switch result.reason {
      | Some(reason) => expect(reason)->toMatchString("not configured")
      | None => Expect.fail("Expected reason")
      }
    })

    it("should reject unknown event types", () => {
      let headers =
        Dict.fromArray([
          ("x-gitlab-token", webhookSecret),
          ("x-gitlab-event", "Unknown Event"),
        ])->toHeaderDict
      let body = pushHookPayload

      let result = WebhookValidator.validateWebhookRequest(
        headers,
        body,
        webhookSecret,
      )
      expect(result.valid)->toBe(false)
      switch result.reason {
      | Some(reason) => expect(reason)->toMatchString("Unknown webhook event")
      | None => Expect.fail("Expected reason")
      }
    })

    it("should handle all known event types", () => {
      allWebhookEvents->Array.forEach(event => {
        let headers = createWebhookHeaders(event)->toHeaderDict
        let body = "{}"

        let result = WebhookValidator.validateWebhookRequest(
          headers,
          body,
          webhookSecret,
        )
        expect(result.valid)->toBe(true)
        expect(result.event)->toEqual(Some(event))
      })
    })
  })

  describe("requireValidWebhook", () => {
    it("should not throw for valid webhook", () => {
      let headers = createWebhookHeaders("Push Hook")->toHeaderDict
      let body = pushHookPayload

      let fn = () =>
        WebhookValidator.requireValidWebhook(headers, body, webhookSecret)
      expect(fn)->(not_)->toThrow
    })

    it("should throw for invalid webhook", () => {
      let headers = createInvalidWebhookHeaders("Push Hook")->toHeaderDict
      let body = pushHookPayload

      let fn = () =>
        WebhookValidator.requireValidWebhook(headers, body, webhookSecret)
      expect(fn)->toThrow
    })
  })

  describe("extractWebhookMetadata", () => {
    it("should extract metadata from headers", () => {
      let headers = createWebhookHeaders("Push Hook")->toHeaderDict
      let metadata = WebhookValidator.extractWebhookMetadata(headers)

      expect(metadata.event)->toEqual(Some("Push Hook"))
      expect(metadata.instance)->toEqual(Some("https://gitlab.com"))
      switch metadata.requestId {
      | Some(id) => expect(id)->toMatchString("test-request-")
      | None => Expect.fail("Expected requestId")
      }
    })

    it("should handle missing headers", () => {
      let headers: Dict.t<WebhookValidator.headerValue> = Dict.fromArray([])
      let metadata = WebhookValidator.extractWebhookMetadata(headers)

      expect(metadata.event)->toEqual(None)
      expect(metadata.instance)->toEqual(None)
      expect(metadata.requestId)->toEqual(None)
    })
  })

  describe("validateSecretStrength", () => {
    it("should accept strong secrets", () => {
      let result = WebhookValidator.validateSecretStrength(webhookSecret)
      expect(result.valid)->toBe(true)
      expect(result.issues)->toEqual([])
    })

    it("should reject empty secrets", () => {
      let result = WebhookValidator.validateSecretStrength(weakSecrets.empty)
      expect(result.valid)->toBe(false)
      expect(result.issues)->toContain("Secret is empty")
    })

    it("should warn about short secrets", () => {
      let result = WebhookValidator.validateSecretStrength(weakSecrets.short)
      expect(result.valid)->toBe(false)
      let hasLengthIssue = result.issues->Array.some(i =>
        i->String.includes("32 characters")
      )
      expect(hasLengthIssue)->toBe(true)
    })

    it("should warn about placeholder secrets", () => {
      let result = WebhookValidator.validateSecretStrength(weakSecrets.placeholder)
      expect(result.valid)->toBe(false)
      let hasPlaceholderIssue = result.issues->Array.some(i =>
        i->String.includes("placeholder")
      )
      expect(hasPlaceholderIssue)->toBe(true)
    })

    it("should warn about repeated character secrets", () => {
      let result = WebhookValidator.validateSecretStrength(weakSecrets.repeated)
      expect(result.valid)->toBe(false)
      let hasRepeatedIssue = result.issues->Array.some(i =>
        i->String.includes("repeated")
      )
      expect(hasRepeatedIssue)->toBe(true)
    })
  })
})
