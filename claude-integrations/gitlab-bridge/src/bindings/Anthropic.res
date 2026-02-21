// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Bindings for Anthropic SDK (@anthropic-ai/sdk)
 */

// Message types
type messageRole = [#user | #assistant]

type textContent = {
  @as("type") type_: string,
  text: string,
}

type message = {
  role: messageRole,
  content: array<textContent>,
}

// API response types
type usage = {
  @as("input_tokens") inputTokens: int,
  @as("output_tokens") outputTokens: int,
}

type contentBlock = {
  @as("type") type_: string,
  text: string,
}

type messageResponse = {
  id: string,
  @as("type") type_: string,
  role: messageRole,
  content: array<contentBlock>,
  model: string,
  @as("stop_reason") stopReason: option<string>,
  @as("stop_sequence") stopSequence: option<string>,
  usage: usage,
}

// Client configuration
type clientConfig = {
  apiKey: string,
  baseURL: option<string>,
  timeout: option<int>,
}

// Message creation parameters
type messageParams = {
  model: string,
  @as("max_tokens") maxTokens: int,
  messages: array<message>,
  system: option<string>,
  temperature: option<float>,
  @as("top_p") topP: option<float>,
  @as("top_k") topK: option<int>,
}

// Client type
type anthropic

// Create client
@module("@anthropic-ai/sdk") @new
external make: clientConfig => anthropic = "default"

// Messages API
module Messages = {
  type messages

  @send
  external create: (messages, messageParams) => promise<messageResponse> = "create"

  @get
  external messages: anthropic => messages = "messages"
}

// Helper to create a user message
let makeUserMessage = (~text: string): message => {
  role: #user,
  content: [{type_: "text", text}],
}

// Helper to create an assistant message
let makeAssistantMessage = (~text: string): message => {
  role: #assistant,
  content: [{type_: "text", text}],
}

// Helper to create message params with defaults
let makeMessageParams = (
  ~model="claude-3-5-sonnet-20241022",
  ~maxTokens=4096,
  ~messages: array<message>,
  ~system: option<string>=?,
  ~temperature: option<float>=?,
  ~topP: option<float>=?,
  ~topK: option<int>=?,
  (),
): messageParams => {
  model,
  maxTokens,
  messages,
  system,
  temperature,
  topP,
  topK,
}
