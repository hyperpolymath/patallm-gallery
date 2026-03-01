// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// FFI bindings for Deno WebSocket, HTTP server, and stdio APIs.
// Used by the MCP server and native host to bridge Claude Code and Firefox.

// --- TextEncoder / TextDecoder ---

module TextEncoder = {
  type t

  @new external make: unit => t = "TextEncoder"
  @send external encode: (t, string) => Js.TypedArray2.Uint8Array.t = "encode"
}

module TextDecoder = {
  type t
  type decodeOptions = {stream: bool}

  @new external make: unit => t = "TextDecoder"
  @send external decode: (t, Js.TypedArray2.Uint8Array.t) => string = "decode"
  @send external decodeWithOptions: (t, Js.TypedArray2.Uint8Array.t, decodeOptions) => string = "decode"
}

// --- DataView ---

module DataView = {
  type t

  @new external make: ArrayBuffer.t => t = "DataView"
  @send external getUint32: (t, int, bool) => int = "getUint32"
  @send external setUint32: (t, int, int, bool) => unit = "setUint32"
}

// --- Uint8Array extensions ---

module Uint8Array = {
  type t = Js.TypedArray2.Uint8Array.t

  @new external make: int => t = "Uint8Array"
  @new external fromArray: array<int> => t = "Uint8Array"
  @get external buffer: t => ArrayBuffer.t = "buffer"
  @get external length: t => int = "length"
  @send external set: (t, t, int) => unit = "set"
  @send external subarray: (t, int, int) => t = "subarray"
  @send external slice: (t, int, int) => t = "slice"
}

// --- WebSocket ---

module WebSocket = {
  type t

  @new external make: string => t = "WebSocket"

  @get external readyState: t => int = "readyState"

  @scope("WebSocket") @val external _OPEN: int = "OPEN"

  @set external setOnopen: (t, unit => unit) => unit = "onopen"
  @set external setOnclose: (t, unit => unit) => unit = "onclose"
  @set external setOnerror: (t, Js.Exn.t => unit) => unit = "onerror"
  @set external setOnmessage: (t, {"data": string} => unit) => unit = "onmessage"

  @send external send: (t, string) => unit = "send"
  @send external close: t => unit = "close"
}

// --- Deno.serve ---

type upgradeResult = {
  socket: WebSocket.t,
  response: Fetch.Response.t,
}

type serveOptions = {port: int}

@scope("Deno") @val
external serve: (serveOptions, Fetch.Request.t => Fetch.Response.t) => unit = "serve"

@scope("Deno") @val
external upgradeWebSocket: Fetch.Request.t => upgradeResult = "upgradeWebSocket"

// --- Deno.stdin / Deno.stdout ---

module ReadableStreamReader = {
  type t

  type readResult = {
    done: bool,
    value: Js.Nullable.t<Js.TypedArray2.Uint8Array.t>,
  }

  @send external read: t => promise<readResult> = "read"
}

module WritableStreamWriter = {
  type t

  @send external write: (t, Js.TypedArray2.Uint8Array.t) => promise<unit> = "write"
}

module Stdin = {
  type readable
  type t = {readable: readable}

  @scope("Deno") @val external stdin: t = "stdin"
  @send external getReader: readable => ReadableStreamReader.t = "getReader"
}

module Stdout = {
  type writable
  type t = {writable: writable}

  @scope("Deno") @val external stdout: t = "stdout"
  @send external getWriter: writable => WritableStreamWriter.t = "getWriter"
  @scope("Deno") @scope("stdout") @val
  external writeSync: Js.TypedArray2.Uint8Array.t => int = "writeSync"
}

// --- Deno process ---

module DenoProcess = {
  @scope("Deno") @val external args: array<string> = "args"
  @scope("Deno") @val external exit: int => unit = "exit"
}

// --- setTimeout ---

@val external setTimeout: (unit => unit, int) => float = "setTimeout"

// --- JSON helpers ---

@scope("JSON") @val external jsonParse: string => 'a = "parse"
@scope("JSON") @val external jsonStringify: 'a => string = "stringify"

// --- Fetch.Request header helpers ---

module RequestHeaders = {
  @send external get: (Fetch.Headers.t, string) => Js.Nullable.t<string> = "get"
}

// --- Response constructor ---

@new external makeResponse: (string, {"status": int}) => Fetch.Response.t = "Response"
