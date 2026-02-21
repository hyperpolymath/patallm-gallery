// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Claude GitLab Bridge - Main entry point
 *
 * This module exports all public APIs for the bridge.
 */

// Re-export auth module
module Auth = Auth

// Re-export bindings
module Anthropic = Anthropic
module Express = Express

// Configuration type
type config = {
  gitlabToken: string,
  gitlabUrl: string,
  anthropicApiKey: string,
  webhookSecret: option<string>,
  port: int,
}

// Bridge state
type bridge = {
  config: config,
  anthropic: Anthropic.anthropic,
}

// Create a new bridge instance
let make = (~config: config): bridge => {
  let anthropic = Anthropic.make({
    apiKey: config.anthropicApiKey,
    baseURL: None,
    timeout: Some(30000),
  })

  {config, anthropic}
}

// Issue analysis parameters
type issueParams = {
  projectId: string,
  issueId: int,
}

// Issue analysis result
type analysis = {
  summary: string,
  suggestions: array<string>,
  priority: string,
}

// Analyze an issue with Claude
let analyzeIssue = async (bridge: bridge, ~params: issueParams): result<analysis, string> => {
  // This is a placeholder - real implementation would fetch issue from GitLab
  // and analyze it with Claude

  let prompt = `Analyze GitLab issue #${params.issueId->Int.toString} from project ${params.projectId}`

  try {
    let messages = [Anthropic.makeUserMessage(~text=prompt)]
    let messageParams = Anthropic.makeMessageParams(
      ~model="claude-3-5-sonnet-20241022",
      ~maxTokens=2048,
      ~messages,
      (),
    )

    let response = await bridge.anthropic->Anthropic.Messages.messages->Anthropic.Messages.create(
      messageParams,
    )

    // Extract text from response
    let text = response.content
      ->Array.get(0)
      ->Option.map(block => block.text)
      ->Option.getOr("No response")

    Ok({
      summary: text,
      suggestions: ["Review code", "Add tests"],
      priority: "medium",
    })
  } catch {
  | Js.Exn.Error(e) => Error(e->Js.Exn.message->Option.getOr("Unknown error"))
  }
}

// MR review parameters
type mrParams = {
  projectId: string,
  mergeRequestId: int,
}

// MR review result
type review = {
  comments: array<string>,
  approved: bool,
  suggestions: array<string>,
}

// Review a merge request with Claude
let reviewMR = async (bridge: bridge, ~params: mrParams): result<review, string> => {
  // Placeholder implementation
  let prompt = `Review merge request #${params.mergeRequestId->Int.toString} from project ${params.projectId}`

  try {
    let messages = [Anthropic.makeUserMessage(~text=prompt)]
    let messageParams = Anthropic.makeMessageParams(
      ~model="claude-3-5-sonnet-20241022",
      ~maxTokens=4096,
      ~messages,
      (),
    )

    let response = await bridge.anthropic->Anthropic.Messages.messages->Anthropic.Messages.create(
      messageParams,
    )

    Ok({
      comments: ["Good code structure", "Consider adding error handling"],
      approved: true,
      suggestions: ["Add unit tests", "Update documentation"],
    })
  } catch {
  | Js.Exn.Error(e) => Error(e->Js.Exn.message->Option.getOr("Unknown error"))
  }
}

// Task execution parameters
type taskParams = {
  taskType: string,
  projectId: string,
  context: JSON.t,
}

// Task result
type taskResult = {
  success: bool,
  message: string,
  data: option<JSON.t>,
}

// Execute an automated task
let executeTask = async (bridge: bridge, ~params: taskParams): result<taskResult, string> => {
  // Placeholder implementation
  Ok({
    success: true,
    message: "Task completed",
    data: None,
  })
}

// Webhook event type
type webhookEvent = {
  eventType: string,
  projectId: string,
  payload: JSON.t,
}

// Handle webhook event
let handleWebhook = async (bridge: bridge, ~event: webhookEvent): result<unit, string> => {
  // Placeholder implementation
  Console.log(`Webhook received: ${event.eventType}`)
  Ok()
}
