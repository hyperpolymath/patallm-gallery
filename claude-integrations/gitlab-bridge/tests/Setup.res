// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Vitest setup file for global test configuration.
 * Clears cached environment variables before each test suite.
 */

// External bindings for process.env manipulation
@val @scope(("process", "env")) external gitlabToken: option<string> = "GITLAB_TOKEN"
@val @scope(("process", "env")) external gitlabUrl: option<string> = "GITLAB_URL"
@val @scope(("process", "env")) external anthropicApiKey: option<string> = "ANTHROPIC_API_KEY"
@val @scope(("process", "env")) external webhookSecret: option<string> = "WEBHOOK_SECRET"

// Delete env vars by setting to undefined
@set @scope(("process", "env")) external setGitlabToken: string => unit = "GITLAB_TOKEN"
@set @scope(("process", "env")) external setGitlabUrl: string => unit = "GITLAB_URL"
@set @scope(("process", "env")) external setAnthropicApiKey: string => unit = "ANTHROPIC_API_KEY"
@set @scope(("process", "env")) external setWebhookSecret: string => unit = "WEBHOOK_SECRET"

// We use JS interop to delete env vars
%%raw(`
import { beforeEach } from "vitest";

beforeEach(() => {
  delete process.env.GITLAB_TOKEN;
  delete process.env.GITLAB_URL;
  delete process.env.ANTHROPIC_API_KEY;
  delete process.env.WEBHOOK_SECRET;
});
`)
