// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Permission checking and scope validation
 */

open Types
open Errors

// Operation to scope mappings
type operationScopes = {
  // Repository operations
  repositoryRead: array<gitLabScope>,
  repositoryWrite: array<gitLabScope>,
  repositoryClone: array<gitLabScope>,
  // Issue operations
  issueRead: array<gitLabScope>,
  issueWrite: array<gitLabScope>,
  issueCreate: array<gitLabScope>,
  issueClose: array<gitLabScope>,
  // Merge request operations
  mergeRequestRead: array<gitLabScope>,
  mergeRequestWrite: array<gitLabScope>,
  mergeRequestCreate: array<gitLabScope>,
  mergeRequestMerge: array<gitLabScope>,
  mergeRequestApprove: array<gitLabScope>,
  // User operations
  userRead: array<gitLabScope>,
  // Project operations
  projectRead: array<gitLabScope>,
  projectAdmin: array<gitLabScope>,
  // Pipeline operations
  pipelineRead: array<gitLabScope>,
  pipelineTrigger: array<gitLabScope>,
  // Branch operations
  branchCreate: array<gitLabScope>,
  branchDelete: array<gitLabScope>,
  // Commit operations
  commitRead: array<gitLabScope>,
  commitCreate: array<gitLabScope>,
}

let operationScopes: operationScopes = {
  repositoryRead: [Api, ReadRepository],
  repositoryWrite: [Api, WriteRepository],
  repositoryClone: [ReadRepository],
  issueRead: [Api, ReadApi],
  issueWrite: [Api],
  issueCreate: [Api],
  issueClose: [Api],
  mergeRequestRead: [Api, ReadApi],
  mergeRequestWrite: [Api],
  mergeRequestCreate: [Api, WriteRepository],
  mergeRequestMerge: [Api, WriteRepository],
  mergeRequestApprove: [Api],
  userRead: [Api, ReadApi, ReadUser],
  projectRead: [Api, ReadApi],
  projectAdmin: [Api],
  pipelineRead: [Api, ReadApi],
  pipelineTrigger: [Api],
  branchCreate: [Api, WriteRepository],
  branchDelete: [Api, WriteRepository],
  commitRead: [Api, ReadRepository],
  commitCreate: [Api, WriteRepository],
}

/**
 * Check if available scopes satisfy required scopes
 * Uses "any-of" logic: any one of the required scopes is sufficient
 */
let checkScopeSatisfaction = (
  ~availableScopes: array<gitLabScope>,
  ~requiredScopes: array<gitLabScope>,
): permissionResult => {
  // If no scopes required, always allowed
  if requiredScopes->Array.length == 0 {
    {
      allowed: true,
      requiredScopes: [],
      availableScopes,
      missingScopes: [],
      reason: None,
    }
  } else {
    // Check if any required scope is available (OR logic)
    let hasAnyRequiredScope = requiredScopes->Array.some(scope =>
      availableScopes->Array.includes(scope)
    )

    if hasAnyRequiredScope {
      {
        allowed: true,
        requiredScopes,
        availableScopes,
        missingScopes: [],
        reason: None,
      }
    } else {
      // None of the required scopes are present
      {
        allowed: false,
        requiredScopes,
        availableScopes,
        missingScopes: requiredScopes,
        reason: Some(
          `Missing required scope. Need one of: ${requiredScopes
            ->Array.map(scopeToString)
            ->Array.joinWith(", ")}`,
        ),
      }
    }
  }
}

/**
 * Check permission for a specific operation
 */
let checkOperationPermission = (
  ~operation: string,
  ~availableScopes: array<gitLabScope>,
): permissionResult => {
  let scopes = operationScopes

  let requiredScopes = switch operation {
  | "repository:read" => scopes.repositoryRead
  | "repository:write" => scopes.repositoryWrite
  | "repository:clone" => scopes.repositoryClone
  | "issue:read" => scopes.issueRead
  | "issue:write" => scopes.issueWrite
  | "issue:create" => scopes.issueCreate
  | "issue:close" => scopes.issueClose
  | "merge_request:read" => scopes.mergeRequestRead
  | "merge_request:write" => scopes.mergeRequestWrite
  | "merge_request:create" => scopes.mergeRequestCreate
  | "merge_request:merge" => scopes.mergeRequestMerge
  | "merge_request:approve" => scopes.mergeRequestApprove
  | "user:read" => scopes.userRead
  | "project:read" => scopes.projectRead
  | "project:admin" => scopes.projectAdmin
  | "pipeline:read" => scopes.pipelineRead
  | "pipeline:trigger" => scopes.pipelineTrigger
  | "branch:create" => scopes.branchCreate
  | "branch:delete" => scopes.branchDelete
  | "commit:read" => scopes.commitRead
  | "commit:create" => scopes.commitCreate
  | _ => []
  }

  checkScopeSatisfaction(~availableScopes, ~requiredScopes)
}

/**
 * Validate required scopes are present
 */
let validateRequiredScopes = (
  ~availableScopes: array<gitLabScope>,
): result<unit, authError> => {
  let result = checkScopeSatisfaction(~availableScopes, ~requiredScopes)

  if result.allowed {
    Ok()
  } else {
    Error(
      insufficientScopeError(
        ~requiredScopes=result.requiredScopes->Array.map(scopeToString),
        ~availableScopes=result.availableScopes->Array.map(scopeToString),
      ),
    )
  }
}

/**
 * Validate no dangerous scopes are present
 */
let validateNoDangerousScopes = (~scopes: array<gitLabScope>): result<unit, authError> => {
  let dangerous = scopes->Array.filter(scope => dangerousScopes->Array.includes(scope))

  if dangerous->Array.length > 0 {
    Error(dangerousScopeError(~dangerousScopes=dangerous->Array.map(scopeToString)))
  } else {
    Ok()
  }
}

/**
 * Require permission - throws exception if not satisfied
 */
let requirePermission = (~operation: string, ~availableScopes: array<gitLabScope>): unit => {
  let result = checkOperationPermission(~operation, ~availableScopes)

  if !result.allowed {
    raise(
      InsufficientScope(
        result.requiredScopes->Array.map(scopeToString),
        result.availableScopes->Array.map(scopeToString),
      ),
    )
  }
}

/**
 * Check if scopes are sufficient for bridge operations
 */
let checkBridgeScopes = (~availableScopes: array<gitLabScope>): permissionResult => {
  checkScopeSatisfaction(~availableScopes, ~requiredScopes)
}

/**
 * Get required scopes for multiple operations
 */
let getRequiredScopesForOperations = (operations: array<string>): array<gitLabScope> => {
  operations
  ->Array.map(op => {
    let result = checkOperationPermission(~operation=op, ~availableScopes=[])
    result.requiredScopes
  })
  ->Array.flat
  ->Array.toSet
  ->Set.toArray
}

/**
 * Check multiple operations at once
 */
let checkMultipleOperations = (
  ~operations: array<string>,
  ~availableScopes: array<gitLabScope>,
): array<(string, permissionResult)> => {
  operations->Array.map(op => (op, checkOperationPermission(~operation=op, ~availableScopes)))
}
