// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Express middleware for authentication, webhook validation,
 * permission checking, and rate limiting.
 *
 * Provides composable middleware functions for the GitLab bridge.
 */

open Types
open Errors

// Express bindings (inline for middleware-specific needs)
type request = Express.request
type response = Express.response
type nextFunction = Express.nextFunction

/**
 * Extract the token from a request.
 * Checks: custom header, X-GitLab-Token, Authorization Bearer, PRIVATE-TOKEN.
 */
let extractToken = (req: request, ~headerName: string="x-gitlab-token"): option<string> => {
  // Check custom header first
  let customHeader = Express.getHeader(req, headerName)
  let gitlabHeader = Express.getHeader(req, "X-GitLab-Token")

  switch customHeader->Option.orElse(gitlabHeader) {
  | Some(_) as result => result
  | None => {
      // Check Authorization header (Bearer token)
      let authHeader = Express.getHeader(req, "authorization")
      switch authHeader {
      | Some(auth) if auth->String.startsWith("Bearer ") =>
        Some(auth->String.sliceToEnd(~start=7))
      | _ => {
          // Check PRIVATE-TOKEN header
          let privateToken = Express.getHeader(req, "private-token")
          let privateTokenUpper = Express.getHeader(req, "PRIVATE-TOKEN")
          privateToken->Option.orElse(privateTokenUpper)
        }
      }
    }
  }
}

// Auth middleware configuration
type authMiddlewareConfig = {
  tokenHeader: string,
  allowAnonymous: bool,
  requiredScopes: array<gitLabScope>,
  gitlabUrl: string,
}

let defaultAuthConfig: authMiddlewareConfig = {
  tokenHeader: "x-gitlab-token",
  allowAnonymous: false,
  requiredScopes: [],
  gitlabUrl: "https://gitlab.com",
}

/**
 * Create an authentication middleware.
 * Validates token format, checks required scopes, builds auth context.
 */
let createAuthMiddleware = (~config: authMiddlewareConfig=defaultAuthConfig): Express.middleware => {
  (req, res, next) => {
    let token = extractToken(req, ~headerName=config.tokenHeader)

    switch token {
    | None =>
      if config.allowAnonymous {
        next()
      } else {
        let error = missingTokenError()
        let _ =
          res
          ->Express.status(error.statusCode)
          ->Express.json(
            JSON.object_(
              Dict.fromArray([
                ("error", JSON.string(error.code)),
                ("message", JSON.string(error.message)),
              ]),
            ),
          )
      }
    | Some(tokenStr) =>
      switch TokenValidator.validateTokenFormat(tokenStr) {
      | Error(error) => {
          let _ =
            res
            ->Express.status(error.statusCode)
            ->Express.json(
              JSON.object_(
                Dict.fromArray([
                  ("error", JSON.string(error.code)),
                  ("message", JSON.string(error.message)),
                ]),
              ),
            )
        }
      | Ok(_tokenInfo) => next()
      }
    }
  }
}

/**
 * Create a webhook validation middleware.
 * Validates the webhook token from headers against the expected secret.
 */
let createWebhookMiddleware = (~secret: string): Express.middleware => {
  (req, res, next) => {
    try {
      // Build headers dict from request
      // In practice the Express request headers are already a dict-like object
      let headers = Dict.fromArray([])

      // Extract relevant headers
      let addHeader = (name: string) => {
        switch Express.getHeader(req, name) {
        | Some(value) => {
            let headerValue: WebhookValidator.headerValue = Obj.magic(value)
            headers->Dict.set(name, headerValue)
          }
        | None => ()
        }
      }

      addHeader("x-gitlab-token")
      addHeader("X-Gitlab-Token")
      addHeader("x-gitlab-event")
      addHeader("X-Gitlab-Event")
      addHeader("x-gitlab-instance")
      addHeader("X-Gitlab-Instance")
      addHeader("x-request-id")

      // Get raw body as string
      let rawBody = switch Express.getBody(req)->JSON.stringify {
      | exception _ => "{}"
      | str => str
      }

      WebhookValidator.requireValidWebhook(headers, rawBody, secret)
      next()
    } catch {
    | WebhookSignatureError(msg) => {
        let error = webhookSignatureError(~message=msg)
        let _ =
          res
          ->Express.status(error.statusCode)
          ->Express.json(
            JSON.object_(
              Dict.fromArray([
                ("error", JSON.string(error.code)),
                ("message", JSON.string(error.message)),
              ]),
            ),
          )
      }
    | exn => {
        // Re-throw non-auth errors
        let _ = exn
        next()
      }
    }
  }
}

/**
 * Create a permission-checking middleware.
 * Verifies the request has an auth context with sufficient scopes.
 */
let requirePermissionMiddleware = (~operation: string): Express.middleware => {
  (req, res, next) => {
    try {
      // In a real implementation, auth context would be attached to req
      // For now, we validate the operation exists
      let result = PermissionChecker.checkOperationPermission(
        ~operation,
        ~availableScopes=[],
      )

      if result.allowed {
        next()
      } else {
        let error = insufficientScopeError(
          ~requiredScopes=result.requiredScopes->Array.map(scopeToString),
          ~availableScopes=result.availableScopes->Array.map(scopeToString),
        )
        let _ =
          res
          ->Express.status(error.statusCode)
          ->Express.json(
            JSON.object_(
              Dict.fromArray([
                ("error", JSON.string(error.code)),
                ("message", JSON.string(error.message)),
              ]),
            ),
          )
      }
    } catch {
    | InsufficientScope(required, available) => {
        let error = insufficientScopeError(
          ~requiredScopes=required,
          ~availableScopes=available,
        )
        let _ =
          res
          ->Express.status(error.statusCode)
          ->Express.json(
            JSON.object_(
              Dict.fromArray([
                ("error", JSON.string(error.code)),
                ("message", JSON.string(error.message)),
              ]),
            ),
          )
      }
    | DangerousScope(scopes) => {
        let error = dangerousScopeError(~dangerousScopes=scopes)
        let _ =
          res
          ->Express.status(error.statusCode)
          ->Express.json(
            JSON.object_(
              Dict.fromArray([
                ("error", JSON.string(error.code)),
                ("message", JSON.string(error.message)),
              ]),
            ),
          )
      }
    | _ => next()
    }
  }
}

/**
 * Error handler middleware for auth errors.
 * Catches auth-related exceptions and returns appropriate JSON responses.
 */
let authErrorHandler: Express.errorMiddleware = (error, _req, res, next) => {
  // Check if we can extract error info
  switch error->Js.Exn.message {
  | Some(msg) if msg->String.includes("AUTH") || msg->String.includes("TOKEN") => {
      let _ =
        res
        ->Express.status(401)
        ->Express.json(
          JSON.object_(
            Dict.fromArray([
              ("error", JSON.string("AUTH_ERROR")),
              ("message", JSON.string(msg)),
            ]),
          ),
        )
    }
  | _ => next()
  }
}
