// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Type definitions for authentication and authorization
 */

// GitLab token scopes
type gitLabScope =
  | @as("api") Api
  | @as("read_api") ReadApi
  | @as("read_user") ReadUser
  | @as("read_repository") ReadRepository
  | @as("write_repository") WriteRepository
  | @as("read_registry") ReadRegistry
  | @as("write_registry") WriteRegistry
  | @as("sudo") Sudo
  | @as("admin_mode") AdminMode
  | @as("create_runner") CreateRunner
  | @as("manage_runner") ManageRunner
  | @as("ai_features") AiFeatures
  | @as("k8s_proxy") K8sProxy

// Dangerous scopes that should never be granted
let dangerousScopes: array<gitLabScope> = [Sudo, AdminMode]

// Required scopes for basic bridge functionality
let requiredScopes: array<gitLabScope> = [Api, ReadRepository, WriteRepository]

// Token type prefixes
type tokenPrefix = {
  personal: string,
  project: string,
  group: string,
  deploy: string,
  runner: string,
  job: string,
  featureFlag: string,
  email: string,
  agent: string,
  oauth: string,
  scim: string,
}

let tokenPrefixes: tokenPrefix = {
  personal: "glpat-",
  project: "glpat-",
  group: "glpat-",
  deploy: "gldt-",
  runner: "glrt-",
  job: "glcbt-",
  featureFlag: "glffct-",
  email: "glimt-",
  agent: "glagent-",
  oauth: "gloas-",
  scim: "glsoat-",
}

// Token type keys
type tokenType =
  | Personal
  | Project
  | Group
  | Deploy
  | Runner
  | Job
  | FeatureFlag
  | Email
  | Agent
  | OAuth
  | Scim

// Validated token information
type tokenInfo = {
  maskedToken: string,
  tokenType: tokenType,
  isValid: bool,
  validatedAt: Date.t,
}

// Permission check result
type permissionResult = {
  allowed: bool,
  requiredScopes: array<gitLabScope>,
  availableScopes: array<gitLabScope>,
  missingScopes: array<gitLabScope>,
  reason: option<string>,
}

// GitLab user state
type userState =
  | @as("active") Active
  | @as("blocked") Blocked
  | @as("deactivated") Deactivated

// GitLab user information
type gitLabUser = {
  id: int,
  username: string,
  name: string,
  email: option<string>,
  state: userState,
  avatarUrl: option<string>,
  webUrl: string,
  isAdmin: option<bool>,
  bot: option<bool>,
}

// Token validation response from GitLab API
type gitLabTokenInfo = {
  scopes: array<string>,
  createdAt: string,
  expiresAt: option<string>,
  userId: option<int>,
  active: option<bool>,
  revoked: option<bool>,
}

// Authentication context
type authContext = {
  authenticated: bool,
  user: option<gitLabUser>,
  token: option<tokenInfo>,
  scopes: array<gitLabScope>,
  gitlabUrl: string,
  authenticatedAt: Date.t,
}

// Webhook validation result
type webhookValidationResult = {
  valid: bool,
  event: option<string>,
  reason: option<string>,
}

// Rate limit information
type rateLimitInfo = {
  limit: int,
  remaining: int,
  resetAt: Date.t,
  isLimited: bool,
}

// Audit log entry
type auditEntry = {
  id: string,
  timestamp: Date.t,
  action: string,
  actor: string,
  resource: string,
  success: bool,
  metadata: option<JSON.t>,
  ipAddress: option<string>,
}

// Helper to convert token type to string
let tokenTypeToString = (tokenType: tokenType): string =>
  switch tokenType {
  | Personal => "personal"
  | Project => "project"
  | Group => "group"
  | Deploy => "deploy"
  | Runner => "runner"
  | Job => "job"
  | FeatureFlag => "featureFlag"
  | Email => "email"
  | Agent => "agent"
  | OAuth => "oauth"
  | Scim => "scim"
  }

// Helper to convert scope to string
let scopeToString = (scope: gitLabScope): string =>
  switch scope {
  | Api => "api"
  | ReadApi => "read_api"
  | ReadUser => "read_user"
  | ReadRepository => "read_repository"
  | WriteRepository => "write_repository"
  | ReadRegistry => "read_registry"
  | WriteRegistry => "write_registry"
  | Sudo => "sudo"
  | AdminMode => "admin_mode"
  | CreateRunner => "create_runner"
  | ManageRunner => "manage_runner"
  | AiFeatures => "ai_features"
  | K8sProxy => "k8s_proxy"
  }

// Helper to parse scope from string
let scopeFromString = (str: string): option<gitLabScope> =>
  switch str {
  | "api" => Some(Api)
  | "read_api" => Some(ReadApi)
  | "read_user" => Some(ReadUser)
  | "read_repository" => Some(ReadRepository)
  | "write_repository" => Some(WriteRepository)
  | "read_registry" => Some(ReadRegistry)
  | "write_registry" => Some(WriteRegistry)
  | "sudo" => Some(Sudo)
  | "admin_mode" => Some(AdminMode)
  | "create_runner" => Some(CreateRunner)
  | "manage_runner" => Some(ManageRunner)
  | "ai_features" => Some(AiFeatures)
  | "k8s_proxy" => Some(K8sProxy)
  | _ => None
  }
