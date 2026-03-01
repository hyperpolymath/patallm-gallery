// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/**
 * FFI bindings for Vitest test framework
 *
 * Provides bindings for:
 * - describe, it - test structure
 * - expect - assertions
 * - vi.fn - mock functions
 * - beforeEach, afterEach - lifecycle hooks
 */

// Test suite definition
@module("vitest") external describe: (string, unit => unit) => unit = "describe"

// Test case definition
@module("vitest") external it: (string, unit => unit) => unit = "it"

// Async test case definition
@module("vitest") external itAsync: (string, unit => promise<unit>) => unit = "it"

// Expect function returning expectation object
type expectation

@module("vitest") external expect: 'a => expectation = "expect"

// Assertion methods
@send external toBe: (expectation, 'a) => unit = "toBe"
@send external toEqual: (expectation, 'a) => unit = "toEqual"
@send external toBeDefined: expectation => unit = "toBeDefined"
@send external toBeUndefined: expectation => unit = "toBeUndefined"
@send external toBeNull: expectation => unit = "toBeNull"
@send external toBeTruthy: expectation => unit = "toBeTruthy"
@send external toBeFalsy: expectation => unit = "toBeFalsy"
@send external toContain: (expectation, 'a) => unit = "toContain"
@send external toMatch: (expectation, Js.Re.t) => unit = "toMatch"
@send external toMatchString: (expectation, string) => unit = "toMatch"
@send external toMatchObject: (expectation, 'a) => unit = "toMatchObject"
@send external toBeGreaterThan: (expectation, 'a) => unit = "toBeGreaterThan"
@send external toBeGreaterThanOrEqual: (expectation, 'a) => unit = "toBeGreaterThanOrEqual"
@send external toBeLessThan: (expectation, 'a) => unit = "toBeLessThan"
@send external toBeLessThanOrEqual: (expectation, 'a) => unit = "toBeLessThanOrEqual"
@send external toBeInstanceOf: (expectation, 'a) => unit = "toBeInstanceOf"
@send external toHaveBeenCalled: expectation => unit = "toHaveBeenCalled"
@send external toHaveBeenCalledTimes: (expectation, int) => unit = "toHaveBeenCalledTimes"
@send external toHaveBeenCalledWith: (expectation, 'a) => unit = "toHaveBeenCalledWith"
@send external toHaveLength: (expectation, int) => unit = "toHaveLength"
@send external toThrow: expectation => unit = "toThrow"
@send external toThrowError: (expectation, 'a) => unit = "toThrowError"

// Negation
@get external not_: expectation => expectation = "not"

// Expect.objectContaining
module Expect = {
  @module("vitest") @scope("expect")
  external objectContaining: 'a => 'a = "objectContaining"

  @module("vitest") @scope("expect")
  external fail: string => unit = "fail"
}

// Lifecycle hooks
@module("vitest") external beforeEach: (unit => unit) => unit = "beforeEach"
@module("vitest") external afterEach: (unit => unit) => unit = "afterEach"
@module("vitest") external beforeAll: (unit => unit) => unit = "beforeAll"
@module("vitest") external afterAll: (unit => unit) => unit = "afterAll"

// Async lifecycle hooks
@module("vitest") external beforeEachAsync: (unit => promise<unit>) => unit = "beforeEach"
@module("vitest") external afterEachAsync: (unit => promise<unit>) => unit = "afterEach"

// Mock function type
type mockFn<'a>

// vi module
module Vi = {
  @module("vitest") @scope("vi")
  external fn: unit => mockFn<'a> = "fn"

  @module("vitest") @scope("vi")
  external fnWithImpl: ('a => 'b) => mockFn<'a => 'b> = "fn"
}

// Call a mock function with no args
@send external callMock0: (mockFn<unit => 'a>, unit) => 'a = "call"

// Apply a mock function (for calling as middleware next())
external mockToFn0: mockFn<'a> => (unit => unit) = "%identity"
external mockToFn1: mockFn<'a> => ('a => unit) = "%identity"
external mockToFn2: mockFn<'a> => ('a, 'b) => unit = "%identity"
external mockToFn3: mockFn<'a> => ('a, 'b, 'c) => unit = "%identity"
