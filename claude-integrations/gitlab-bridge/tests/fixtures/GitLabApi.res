// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Test fixtures for GitLab API responses.
 * Provides replayable fixtures for mocking GitLab API calls.
 */

// User fixture type
type user = {
  id: int,
  username: string,
  name: string,
  email: option<string>,
  state: string,
  avatar_url: Nullable.t<string>,
  web_url: string,
  is_admin: bool,
  bot: bool,
}

let activeUser: user = {
  id: 12345,
  username: "testuser",
  name: "Test User",
  email: Some("test@example.com"),
  state: "active",
  avatar_url: Nullable.Value(
    "https://gitlab.com/uploads/-/system/user/avatar/12345/avatar.png",
  ),
  web_url: "https://gitlab.com/testuser",
  is_admin: false,
  bot: false,
}

let adminUser: user = {
  id: 1,
  username: "admin",
  name: "Administrator",
  email: Some("admin@example.com"),
  state: "active",
  avatar_url: Nullable.Value(
    "https://gitlab.com/uploads/-/system/user/avatar/1/avatar.png",
  ),
  web_url: "https://gitlab.com/admin",
  is_admin: true,
  bot: false,
}

let botUser: user = {
  id: 99999,
  username: "project_12345_bot",
  name: "Project Bot",
  email: None,
  state: "active",
  avatar_url: Nullable.null,
  web_url: "https://gitlab.com/project_12345_bot",
  is_admin: false,
  bot: true,
}

let blockedUser: user = {
  id: 54321,
  username: "blockeduser",
  name: "Blocked User",
  email: Some("blocked@example.com"),
  state: "blocked",
  avatar_url: Nullable.null,
  web_url: "https://gitlab.com/blockeduser",
  is_admin: false,
  bot: false,
}

let deactivatedUser: user = {
  id: 11111,
  username: "deactivated",
  name: "Deactivated User",
  email: None,
  state: "deactivated",
  avatar_url: Nullable.null,
  web_url: "https://gitlab.com/deactivated",
  is_admin: false,
  bot: false,
}

// Project fixture type
type project = {
  id: int,
  name: string,
  path: string,
  path_with_namespace: string,
  visibility: string,
  default_branch: string,
  web_url: string,
  http_url_to_repo: string,
  ssh_url_to_repo: string,
}

let publicProject: project = {
  id: 100,
  name: "test-project",
  path: "test-project",
  path_with_namespace: "testuser/test-project",
  visibility: "public",
  default_branch: "main",
  web_url: "https://gitlab.com/testuser/test-project",
  http_url_to_repo: "https://gitlab.com/testuser/test-project.git",
  ssh_url_to_repo: "git@gitlab.com:testuser/test-project.git",
}

// API error fixture type
type apiError = {
  status: int,
  message: string,
}

let unauthorizedError: apiError = {status: 401, message: "401 Unauthorized"}
let forbiddenError: apiError = {status: 403, message: "403 Forbidden"}
let notFoundError: apiError = {status: 404, message: "404 Not Found"}
let serverError: apiError = {status: 500, message: "500 Internal Server Error"}
