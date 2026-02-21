// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Common Forge Adapter Interface
 * Provides unified API for different git forge platforms
 */

// Forge types
type forgeType =
  | @as("gitlab") GitLab
  | @as("github") GitHub
  | @as("bitbucket") Bitbucket
  | @as("gitea") Gitea
  | @as("disroot") Disroot
  | @as("sourcehut") SourceHut
  | @as("codeberg") Codeberg
  | @as("forgejo") Forgejo

// Common issue type
type issue = {
  id: int,
  iid: int, // Internal ID (per-project)
  title: string,
  description: string,
  state: [#open | #closed],
  labels: array<string>,
  author: user,
  assignees: array<user>,
  createdAt: Date.t,
  updatedAt: Date.t,
  url: string,
}

and user = {
  id: int,
  username: string,
  name: string,
  email: option<string>,
  avatarUrl: option<string>,
}

// Common merge/pull request type
type mergeRequest = {
  id: int,
  iid: int,
  title: string,
  description: string,
  state: [#open | #merged | #closed],
  sourceBranch: string,
  targetBranch: string,
  author: user,
  assignees: array<user>,
  reviewers: array<user>,
  draft: bool,
  createdAt: Date.t,
  updatedAt: Date.t,
  url: string,
}

// Common repository type
type repository = {
  id: int,
  name: string,
  fullName: string,
  description: option<string>,
  defaultBranch: string,
  visibility: [#public | #private | #internal],
  url: string,
  cloneUrl: string,
  sshUrl: string,
}

// Webhook event types
type webhookEvent = {
  eventType: string,
  projectId: string,
  payload: JSON.t,
}

// Forge adapter module type
module type ForgeAdapterType = {
  // Forge information
  let forgeName: forgeType
  let baseUrl: string

  // Authentication
  let validateToken: string => promise<result<unit, string>>

  // Issues
  let getIssue: (~projectId: string, ~issueId: int) => promise<result<issue, string>>
  let listIssues: (~projectId: string, ~state: option<string>) => promise<
    result<array<issue>, string>,
  >
  let createIssue: (
    ~projectId: string,
    ~title: string,
    ~description: string,
    ~labels: option<array<string>>,
  ) => promise<result<issue, string>>
  let updateIssue: (~projectId: string, ~issueId: int, ~updates: JSON.t) => promise<
    result<issue, string>,
  >
  let closeIssue: (~projectId: string, ~issueId: int) => promise<result<unit, string>>

  // Merge/Pull Requests
  let getMR: (~projectId: string, ~mrId: int) => promise<result<mergeRequest, string>>
  let listMRs: (~projectId: string, ~state: option<string>) => promise<
    result<array<mergeRequest>, string>,
  >
  let createMR: (
    ~projectId: string,
    ~title: string,
    ~description: string,
    ~sourceBranch: string,
    ~targetBranch: string,
  ) => promise<result<mergeRequest, string>>
  let mergeMR: (~projectId: string, ~mrId: int) => promise<result<unit, string>>
  let addMRComment: (~projectId: string, ~mrId: int, ~comment: string) => promise<
    result<unit, string>,
  >

  // Repositories
  let getRepository: (~projectId: string) => promise<result<repository, string>>
  let listRepositories: unit => promise<result<array<repository>, string>>

  // Webhooks
  let validateWebhook: (~payload: string, ~signature: string, ~secret: string) => bool
  let parseWebhookEvent: JSON.t => result<webhookEvent, string>

  // Diffs
  let getMRDiff: (~projectId: string, ~mrId: int) => promise<result<string, string>>
  let getCommitDiff: (~projectId: string, ~sha: string) => promise<result<string, string>>
}

// Helper to detect forge type from URL
let detectForgeType = (url: string): option<forgeType> => {
  if url->String.includes("gitlab") {
    Some(GitLab)
  } else if url->String.includes("github") {
    Some(GitHub)
  } else if url->String.includes("bitbucket") {
    Some(Bitbucket)
  } else if url->String.includes("gitea") {
    Some(Gitea)
  } else if url->String.includes("disroot") {
    Some(Disroot)
  } else if url->String.includes("sr.ht") {
    Some(SourceHut)
  } else if url->String.includes("codeberg") {
    Some(Codeberg)
  } else if url->String.includes("forgejo") {
    Some(Forgejo)
  } else {
    None
  }
}

// Helper to convert forge type to string
let forgeTypeToString = (forgeType: forgeType): string => {
  switch forgeType {
  | GitLab => "gitlab"
  | GitHub => "github"
  | Bitbucket => "bitbucket"
  | Gitea => "gitea"
  | Disroot => "disroot"
  | SourceHut => "sourcehut"
  | Codeberg => "codeberg"
  | Forgejo => "forgejo"
  }
}

// Common error type
type forgeError = {
  forge: forgeType,
  message: string,
  statusCode: option<int>,
  details: option<JSON.t>,
}

let makeError = (~forge: forgeType, ~message: string, ~statusCode: option<int>=?, ~details: option<JSON.t>=?, ()): forgeError => {
  {forge, message, statusCode, details}
}
