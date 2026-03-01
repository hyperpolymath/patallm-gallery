// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Gitea/Forgejo/Codeberg Forge Adapter
 * Implementation for Gitea API (compatible with Forgejo and Codeberg)
 */

open ForgeAdapter

module GiteaAdapter: ForgeAdapterType = {
  let forgeName = Gitea
  let baseUrl = "https://gitea.io/api/v1"

  // Gitea API is similar to GitHub's API
  // Forgejo and Codeberg are Gitea forks with compatible APIs

  let validateToken = async (token: string): result<unit, string> => {
    Error("Not implemented")
  }

  let getIssue = async (~projectId: string, ~issueId: int): result<issue, string> => {
    Error("Not implemented")
  }

  let listIssues = async (~projectId: string, ~state: option<string>): result<
    array<issue>,
    string,
  > => {
    Error("Not implemented")
  }

  let createIssue = async (
    ~projectId: string,
    ~title: string,
    ~description: string,
    ~labels: option<array<string>>,
  ): result<issue, string> => {
    Error("Not implemented")
  }

  let updateIssue = async (
    ~projectId: string,
    ~issueId: int,
    ~updates: JSON.t,
  ): result<issue, string> => {
    Error("Not implemented")
  }

  let closeIssue = async (~projectId: string, ~issueId: int): result<unit, string> => {
    Error("Not implemented")
  }

  let getMR = async (~projectId: string, ~mrId: int): result<mergeRequest, string> => {
    Error("Not implemented")
  }

  let listMRs = async (~projectId: string, ~state: option<string>): result<
    array<mergeRequest>,
    string,
  > => {
    Error("Not implemented")
  }

  let createMR = async (
    ~projectId: string,
    ~title: string,
    ~description: string,
    ~sourceBranch: string,
    ~targetBranch: string,
  ): result<mergeRequest, string> => {
    Error("Not implemented")
  }

  let mergeMR = async (~projectId: string, ~mrId: int): result<unit, string> => {
    Error("Not implemented")
  }

  let addMRComment = async (~projectId: string, ~mrId: int, ~comment: string): result<
    unit,
    string,
  > => {
    Error("Not implemented")
  }

  let getRepository = async (~projectId: string): result<repository, string> => {
    Error("Not implemented")
  }

  let listRepositories = async (): result<array<repository>, string> => {
    Error("Not implemented")
  }

  let validateWebhook = (~payload: string, ~signature: string, ~secret: string): bool => {
    true
  }

  let parseWebhookEvent = (json: JSON.t): result<webhookEvent, string> => {
    Error("Not implemented")
  }

  let getMRDiff = async (~projectId: string, ~mrId: int): result<string, string> => {
    Error("Not implemented")
  }

  let getCommitDiff = async (~projectId: string, ~sha: string): result<string, string> => {
    Error("Not implemented")
  }
}
