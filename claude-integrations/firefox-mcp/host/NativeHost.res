// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// Claude Firefox MCP - Native Messaging Host (Deno)
// Bridges Claude Code CLI (stdio MCP) <-> Firefox Extension (native messaging)
//
// Operates in two modes:
//   "native" - Firefox calls this process via native messaging protocol
//              (length-prefixed binary JSON on stdin/stdout)
//   "mcp"    - Claude Code calls this process as an MCP server
//              (newline-delimited JSON-RPC on stdin/stdout, WebSocket to extension)

open Deno_Ws

// --- Constants ---

let _extensionId = "claude-mcp@hyperpolymath.org"

// --- MCP tool schema helper types ---

type rec jsonSchema = {
  @as("type") type_: string,
  properties: option<Dict.t<jsonSchema>>,
  items: option<jsonSchema>,
  description: option<string>,
  enum: option<array<string>>,
  required: option<array<string>>,
}

type mcpTool = {
  name: string,
  description: string,
  inputSchema: jsonSchema,
}

// Helper to build a leaf schema property (no nested properties, items, enum, or required).
let prop = (~type_: string, ~description: option<string>=None, ()): jsonSchema => {
  type_,
  properties: None,
  items: None,
  description,
  enum: None,
  required: None,
}

// Helper to build an array-typed schema property with a typed item.
let arrayProp = (~description: option<string>=None, ~itemType: string, ()): jsonSchema => {
  type_: "array",
  properties: None,
  items: Some(prop(~type_=itemType, ())),
  description,
  enum: None,
  required: None,
}

// Helper to build an enum-typed schema property.
let enumProp = (
  ~type_: string,
  ~description: option<string>=None,
  ~enum_: array<string>,
  (),
): jsonSchema => {
  type_,
  properties: None,
  items: None,
  description,
  enum: Some(enum_),
  required: None,
}

// --- MCP Tool definitions ---

let mcpTools: array<mcpTool> = [
  {
    name: "screenshot",
    description: "Take a screenshot of the current browser tab",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([("tabId", prop(~type_="number", ~description=Some("Tab ID (optional, uses active tab)"), ()))]),
      ),
      items: None,
      description: None,
      enum: None,
      required: None,
    },
  },
  {
    name: "navigate",
    description: "Navigate to a URL or go back/forward in history",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          ("url", prop(~type_="string", ~description=Some("URL to navigate to, or 'back'/'forward'"), ())),
          ("tabId", prop(~type_="number", ~description=Some("Tab ID (optional)"), ())),
        ]),
      ),
      items: None,
      description: None,
      enum: None,
      required: Some(["url"]),
    },
  },
  {
    name: "read_page",
    description: "Get the accessibility tree representation of the page",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          ("tabId", prop(~type_="number", ~description=Some("Tab ID (optional)"), ())),
          ("depth", prop(~type_="number", ~description=Some("Max depth (default: 15)"), ())),
          (
            "filter",
            enumProp(
              ~type_="string",
              ~description=Some("Filter elements"),
              ~enum_=["all", "interactive"],
              (),
            ),
          ),
        ]),
      ),
      items: None,
      description: None,
      enum: None,
      required: None,
    },
  },
  {
    name: "click",
    description: "Click at coordinates or on an element",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          ("coordinate", arrayProp(~description=Some("[x, y] coordinates"), ~itemType="number", ())),
          ("ref", prop(~type_="string", ~description=Some("Element reference from read_page"), ())),
          ("button", enumProp(~type_="string", ~description=Some("Mouse button"), ~enum_=["left", "right"], ())),
          ("tabId", prop(~type_="number", ())),
        ]),
      ),
      items: None,
      description: None,
      enum: None,
      required: None,
    },
  },
  {
    name: "type",
    description: "Type text into the focused element or at coordinates",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          ("text", prop(~type_="string", ~description=Some("Text to type"), ())),
          ("coordinate", arrayProp(~description=Some("[x, y] to click first"), ~itemType="number", ())),
          ("tabId", prop(~type_="number", ())),
        ]),
      ),
      items: None,
      description: None,
      enum: None,
      required: Some(["text"]),
    },
  },
  {
    name: "scroll",
    description: "Scroll the page or an element",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          (
            "direction",
            enumProp(~type_="string", ~enum_=["up", "down", "left", "right"], ()),
          ),
          ("amount", prop(~type_="number", ~description=Some("Pixels to scroll (default: 300)"), ())),
          ("coordinate", arrayProp(~description=Some("Scroll at position"), ~itemType="number", ())),
          ("tabId", prop(~type_="number", ())),
        ]),
      ),
      items: None,
      description: None,
      enum: None,
      required: Some(["direction"]),
    },
  },
  {
    name: "execute_js",
    description: "Execute JavaScript code in the page context",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          ("code", prop(~type_="string", ~description=Some("JavaScript code to execute"), ())),
          ("tabId", prop(~type_="number", ())),
        ]),
      ),
      items: None,
      description: None,
      enum: None,
      required: Some(["code"]),
    },
  },
  {
    name: "find",
    description: "Find elements by text content or CSS selector",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          ("query", prop(~type_="string", ~description=Some("Text or selector to search for"), ())),
          ("tabId", prop(~type_="number", ())),
        ]),
      ),
      items: None,
      description: None,
      enum: None,
      required: Some(["query"]),
    },
  },
  {
    name: "form_input",
    description: "Set a form field value",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          ("selector", prop(~type_="string", ~description=Some("CSS selector for the input"), ())),
          ("value", prop(~type_="string", ~description=Some("Value to set (string, number, or boolean)"), ())),
          ("tabId", prop(~type_="number", ())),
        ]),
      ),
      items: None,
      description: None,
      enum: None,
      required: Some(["selector", "value"]),
    },
  },
  {
    name: "tabs_list",
    description: "List all open tabs in the current window",
    inputSchema: {
      type_: "object",
      properties: Some(Dict.make()),
      items: None,
      description: None,
      enum: None,
      required: None,
    },
  },
  {
    name: "tabs_create",
    description: "Create a new tab",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([("url", prop(~type_="string", ~description=Some("URL to open (optional)"), ()))]),
      ),
      items: None,
      description: None,
      enum: None,
      required: None,
    },
  },
  {
    name: "tabs_close",
    description: "Close a tab",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          ("tabId", prop(~type_="number", ~description=Some("Tab ID to close (optional, uses active)"), ())),
        ]),
      ),
      items: None,
      description: None,
      enum: None,
      required: None,
    },
  },
  {
    name: "get_page_text",
    description: "Get the text content of the page",
    inputSchema: {
      type_: "object",
      properties: Some(Dict.fromArray([("tabId", prop(~type_="number", ()))])),
      items: None,
      description: None,
      enum: None,
      required: None,
    },
  },
]

// --- Native messaging protocol ---

// Read a native messaging frame from the stream reader.
// The native messaging protocol uses a 4-byte little-endian length prefix
// followed by that many bytes of JSON payload. Max message size is 1 MB.
let readNativeMessage = async (reader: ReadableStreamReader.t): option<Js.Json.t> => {
  // Read 4-byte length prefix (little-endian uint32)
  let lengthBuf = Uint8Array.make(4)
  let offset = ref(0)

  let headerDone = ref(false)
  let earlyEof = ref(false)

  while !headerDone.contents && !earlyEof.contents {
    let readResult = await ReadableStreamReader.read(reader)
    if readResult.done {
      earlyEof := true
    } else {
      switch Js.Nullable.toOption(readResult.value) {
      | Some(value) => {
          let copyLen = Math.Int.min(4 - offset.contents, Uint8Array.length(value))
          Uint8Array.set(lengthBuf, Uint8Array.subarray(value, 0, copyLen), offset.contents)
          offset := offset.contents + Uint8Array.length(value)
          if offset.contents >= 4 {
            headerDone := true
          }
        }
      | None => earlyEof := true
      }
    }
  }

  if earlyEof.contents {
    None
  } else {
    let dv = DataView.make(Uint8Array.buffer(lengthBuf))
    let length = DataView.getUint32(dv, 0, true)

    // Guard against zero-length or oversized messages (max 1 MB)
    if length === 0 || length > 1024 * 1024 {
      None
    } else {
      let msgBuf = Uint8Array.make(length)
      let offset = ref(0)
      let bodyDone = ref(false)
      let earlyEof = ref(false)

      while !bodyDone.contents && !earlyEof.contents {
        let readResult = await ReadableStreamReader.read(reader)
        if readResult.done {
          earlyEof := true
        } else {
          switch Js.Nullable.toOption(readResult.value) {
          | Some(value) => {
              let copyLen = Math.Int.min(length - offset.contents, Uint8Array.length(value))
              Uint8Array.set(msgBuf, Uint8Array.subarray(value, 0, copyLen), offset.contents)
              offset := offset.contents + Uint8Array.length(value)
              if offset.contents >= length {
                bodyDone := true
              }
            }
          | None => earlyEof := true
          }
        }
      }

      if earlyEof.contents {
        None
      } else {
        let decoder = TextDecoder.make()
        let text = TextDecoder.decode(decoder, msgBuf)
        Some(jsonParse(text))
      }
    }
  }
}

// Write a native messaging frame: 4-byte little-endian length prefix + JSON bytes.
let writeNativeMessage = async (
  writer: WritableStreamWriter.t,
  message: 'a,
): unit => {
  let json = jsonStringify(message)
  let encoder = TextEncoder.make()
  let msgBytes = TextEncoder.encode(encoder, json)
  let lengthBuf = Uint8Array.make(4)
  let dv = DataView.make(Uint8Array.buffer(lengthBuf))
  DataView.setUint32(dv, 0, Uint8Array.length(msgBytes), true)

  // Concatenate length prefix and message into a single write
  let combined = Uint8Array.make(4 + Uint8Array.length(msgBytes))
  Uint8Array.set(combined, lengthBuf, 0)
  Uint8Array.set(combined, msgBytes, 4)
  await WritableStreamWriter.write(writer, combined)
}

// --- Stdio MCP protocol helpers ---

// Write a newline-delimited JSON message to stdout for Claude Code.
let writeStdio = (message: 'a): unit => {
  let json = jsonStringify(message)
  let encoder = TextEncoder.make()
  let _ = Stdout.writeSync(TextEncoder.encode(encoder, json ++ "\n"))
}

// --- MCP Server state ---

let _initialized = ref(false)
let requestId = ref(0)

type pendingRequest = {
  resolve: Js.Json.t => unit,
  reject: exn => unit,
}

let pendingExtensionRequests: Dict.t<pendingRequest> = Dict.make()

// --- Extension communication ---

// Forward a tool call to the Firefox extension via the writer and wait for
// the response. Used when operating in MCP mode (WebSocket bridge).
let callExtension = async (
  extensionWriter: WritableStreamWriter.t,
  toolName: string,
  args: Js.Json.t,
): Js.Json.t => {
  requestId := requestId.contents + 1
  let id = requestId.contents
  let idStr = Int.toString(id)

  let result = await Promise.make((resolve, reject) => {
    Dict.set(pendingExtensionRequests, idStr, {resolve, reject})
    let _ = setTimeout(() => {
      switch Dict.get(pendingExtensionRequests, idStr) {
      | Some(_) => {
          Dict.delete(pendingExtensionRequests, idStr)
          reject(Exn.anyToExnInternal("Extension request timed out"))
        }
      | None => ()
      }
    }, 30000)
  })

  await writeNativeMessage(extensionWriter, {
    "jsonrpc": "2.0",
    "id": id,
    "method": "tools/call",
    "params": {"name": toolName, "arguments": args},
  })

  result
}

// --- MCP request handler ---

type mcpRequest = {
  id: option<int>,
  method: string,
  params: option<Js.Json.t>,
}

type toolCallParams = {
  name: string,
  arguments: Js.Json.t,
}

type mcpResponse = {
  id: option<int>,
  result: option<Js.Json.t>,
  error: option<{"code": int, "message": string}>,
}

// Handle a single MCP JSON-RPC request from Claude Code.
// Returns a response object to be serialized and written to stdout.
let handleMcpRequest = async (
  request: mcpRequest,
  extensionWriter: WritableStreamWriter.t,
): mcpResponse => {
  let {id, method, params} = request

  try {
    switch method {
    | "initialize" => {
        _initialized := true
        {
          id,
          result: Some(
            Obj.magic({
              "protocolVersion": "2024-11-05",
              "capabilities": {"tools": Js.Dict.empty()},
              "serverInfo": {"name": "claude-firefox-mcp", "version": "1.0.0"},
            }),
          ),
          error: None,
        }
      }

    | "notifications/initialized" => {id, result: None, error: None}

    | "tools/list" => {
        id,
        result: Some(Obj.magic({"tools": mcpTools})),
        error: None,
      }

    | "tools/call" =>
      switch params {
      | Some(p) => {
          let toolParams: toolCallParams = Obj.magic(p)
          let result = await callExtension(extensionWriter, toolParams.name, toolParams.arguments)
          {id, result: Some(result), error: None}
        }
      | None => {
          id,
          result: None,
          error: Some({"code": -32600, "message": "Missing params for tools/call"}),
        }
      }

    | _ => {
        id,
        result: None,
        error: Some({"code": -32601, "message": `Method not found: ${method}`}),
      }
    }
  } catch {
  | Exn.Error(err) => {
      let msg = switch Exn.message(err) {
      | Some(m) => m
      | None => "Unknown error"
      }
      {id, result: None, error: Some({"code": -32000, "message": msg})}
    }
  | err => {
      let msg = Obj.magic(err)->Js.String.make
      {id, result: None, error: Some({"code": -32000, "message": msg})}
    }
  }
}

// --- MCP mode ---

// Run in MCP server mode: connect to the Firefox extension via a localhost
// WebSocket, read MCP requests from stdin, forward tool calls to the extension,
// and write responses back to stdout.
let runMcpMode = async (): unit => {
  let wsPort = 9876
  let ws: ref<option<WebSocket.t>> = ref(None)

  // Attempt to connect to the Firefox extension's WebSocket server
  let connectResult = await Promise.make((resolve, _reject) => {
    try {
      let socket = WebSocket.make(`ws://localhost:${Int.toString(wsPort)}`)

      WebSocket.setOnopen(socket, () => {
        Console.error("[MCP Host] Connected to Firefox extension")
        resolve(Some(socket))
      })

      WebSocket.setOnerror(socket, _e => {
        Console.error("[MCP Host] WebSocket error")
        resolve(None)
      })
    } catch {
    | _ => resolve(None)
    }
  })

  ws := connectResult

  // Handle messages from the extension (resolve pending requests)
  switch ws.contents {
  | Some(socket) =>
    WebSocket.setOnmessage(socket, event => {
      try {
        let msg = jsonParse(event["data"])
        let msgId: option<int> = Obj.magic(Js.Dict.get(Obj.magic(msg), "id"))
        switch msgId {
        | Some(id) => {
            let idStr = Int.toString(id)
            switch Dict.get(pendingExtensionRequests, idStr) {
            | Some({resolve, reject}) => {
                Dict.delete(pendingExtensionRequests, idStr)
                let error: option<{"message": string}> = Obj.magic(
                  Js.Dict.get(Obj.magic(msg), "error"),
                )
                switch error {
                | Some(err) => reject(Exn.anyToExnInternal(err["message"]))
                | None => {
                    let result = Js.Dict.get(Obj.magic(msg), "result")
                    switch result {
                    | Some(r) => resolve(r)
                    | None => resolve(Obj.magic(Js.null))
                    }
                  }
                }
              }
            | None => ()
            }
          }
        | None => ()
        }
      } catch {
      | _ => Console.error("[MCP Host] Failed to parse extension message")
      }
    })
  | None => {
      Console.error("[MCP Host] Extension not available, running in standalone mode")
      Console.error("[MCP Host] Start Firefox with the extension loaded, then restart this server")
    }
  }

  // Read MCP requests from stdin
  let stdinReader = Stdin.getReader(Stdin.stdin.readable)
  let decoder = TextDecoder.make()
  let buffer = ref("")

  let continue_ = ref(true)
  while continue_.contents {
    let readResult = await ReadableStreamReader.read(stdinReader)
    if readResult.done {
      continue_ := false
    } else {
      switch Js.Nullable.toOption(readResult.value) {
      | Some(value) =>
        buffer :=
          buffer.contents ++ TextDecoder.decodeWithOptions(decoder, value, {stream: true})

        // Process complete lines
        let processing = ref(true)
        while processing.contents {
          let idx = String.indexOf(buffer.contents, "\n")
          if idx === -1 {
            processing := false
          } else {
            let line = String.trim(String.slice(buffer.contents, ~start=0, ~end=idx))
            buffer := String.sliceToEnd(buffer.contents, ~start=idx + 1)

            if String.length(line) > 0 {
              try {
                let request: mcpRequest = jsonParse(line)
                Console.error2("[MCP Host] Received:", request.method)

                // Create a mock writer that sends via WebSocket
                // (the real native message writer is only for native mode)
                let mockWriter: WritableStreamWriter.t = Obj.magic({
                  "write": async (msg: Js.TypedArray2.Uint8Array.t) => {
                    switch ws.contents {
                    | Some(socket) if WebSocket.readyState(socket) === WebSocket._OPEN => {
                        let dec = TextDecoder.make()
                        WebSocket.send(socket, TextDecoder.decode(dec, msg))
                      }
                    | _ => ()
                    }
                  },
                })

                let response = await handleMcpRequest(request, mockWriter)
                writeStdio(response)
              } catch {
              | _ => {
                  Console.error("[MCP Host] Error processing request")
                  writeStdio({"error": {"code": -32700, "message": "Parse error"}})
                }
              }
            }
          }
        }
      | None => ()
      }
    }
  }
}

// --- Native mode ---

// Run in native messaging mode: the Firefox extension spawns this process
// and communicates via length-prefixed binary JSON on stdin/stdout.
let runNativeMode = async (): unit => {
  let stdinReader = Stdin.getReader(Stdin.stdin.readable)
  let stdoutWriter = Stdout.getWriter(Stdout.stdout.writable)

  let continue_ = ref(true)
  while continue_.contents {
    let message = await readNativeMessage(stdinReader)
    switch message {
    | None => continue_ := false
    | Some(msg) => {
        Console.error2("[MCP Host] Native message:", msg)
        await writeNativeMessage(stdoutWriter, {
          "received": true,
          "echo": msg,
        })
      }
    }
  }
}

// --- Main entry point ---

// Determine operating mode from the first CLI argument.
// "native" = Firefox native messaging mode (binary protocol).
// Anything else (default) = MCP server mode (newline-delimited JSON-RPC).
let main = async (): unit => {
  Console.error("[MCP Host] Starting Claude Firefox MCP native host")

  let mode = switch DenoProcess.args->Array.get(0) {
  | Some(m) => m
  | None => "mcp"
  }

  if mode === "native" {
    Console.error("[MCP Host] Running in native messaging mode")
    await runNativeMode()
  } else {
    Console.error("[MCP Host] Running in MCP server mode")
    await runMcpMode()
  }
}

main()
->Promise.catch(err => {
  Console.error2("[MCP Host] Fatal error:", err)
  DenoProcess.exit(1)
  Promise.resolve()
})
->ignore
