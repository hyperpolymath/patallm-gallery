// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * Authentication and authorization module for Claude GitLab Bridge
 * Re-exports all auth functionality
 */

// Types
module Types = Types

// Errors
module Errors = Errors

// Token validation
module TokenValidator = TokenValidator

// Permission checking
module PermissionChecker = PermissionChecker

// Webhook validation
module WebhookValidator = WebhookValidator

// Rate limiting
module RateLimiter = RateLimiter

// Middleware
module Middleware = Middleware

// Re-export commonly used types
type gitLabScope = Types.gitLabScope
type tokenInfo = Types.tokenInfo
type permissionResult = Types.permissionResult
type authContext = Types.authContext
type authError = Errors.authError
type rateLimitInfo = Types.rateLimitInfo
type webhookValidationResult = Types.webhookValidationResult

// Re-export commonly used functions
let validateTokenFormat = TokenValidator.validateTokenFormat
let maskToken = TokenValidator.maskToken
let checkScopeSatisfaction = PermissionChecker.checkScopeSatisfaction
let checkOperationPermission = PermissionChecker.checkOperationPermission
let validateRequiredScopes = PermissionChecker.validateRequiredScopes
let validateWebhookToken = WebhookValidator.validateWebhookToken
let computeWebhookSignature = WebhookValidator.computeWebhookSignature
let validateWebhookRequest = WebhookValidator.validateWebhookRequest
let requireValidWebhook = WebhookValidator.requireValidWebhook
let extractWebhookMetadata = WebhookValidator.extractWebhookMetadata
let validateSecretStrength = WebhookValidator.validateSecretStrength
