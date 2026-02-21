// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// Claude Firefox MCP - Native Host Server (Deno)
// Bridges: Claude Code (stdio MCP) <-> WebSocket <-> Firefox Extension
//
// This server listens on a WebSocket port for the Firefox extension to connect,
// and reads MCP JSON-RPC requests from stdin (from Claude Code), forwarding
// tool calls to the extension and returning results.

open Deno_Ws

// --- Constants ---

let wsPort = 9876

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

// --- MCP Tool definitions ---

let mcpTools: array<mcpTool> = [
  {
    name: "screenshot",
    description: "Take a screenshot of the current browser tab",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          (
            "tabId",
            {
              type_: "number",
              description: Some("Tab ID (optional, uses active tab)"),
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
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
    name: "navigate",
    description: "Navigate to a URL or go back/forward in history",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          (
            "url",
            {
              type_: "string",
              description: Some("URL to navigate to, or 'back'/'forward'"),
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
          (
            "tabId",
            {
              type_: "number",
              description: Some("Tab ID (optional)"),
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
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
          (
            "tabId",
            {
              type_: "number",
              description: Some("Tab ID (optional)"),
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
          (
            "depth",
            {
              type_: "number",
              description: Some("Max depth (default: 15)"),
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
          (
            "filter",
            {
              type_: "string",
              description: Some("Filter elements"),
              properties: None,
              items: None,
              enum: Some(["all", "interactive"]),
              required: None,
            },
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
    description: "Click at coordinates",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          (
            "coordinate",
            {
              type_: "array",
              description: Some("[x, y] coordinates"),
              properties: None,
              items: Some({
                type_: "number",
                description: None,
                properties: None,
                items: None,
                enum: None,
                required: None,
              }),
              enum: None,
              required: None,
            },
          ),
          (
            "button",
            {
              type_: "string",
              description: Some("Mouse button"),
              properties: None,
              items: None,
              enum: Some(["left", "right"]),
              required: None,
            },
          ),
          (
            "tabId",
            {
              type_: "number",
              description: None,
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
        ]),
      ),
      items: None,
      description: None,
      enum: None,
      required: Some(["coordinate"]),
    },
  },
  {
    name: "type",
    description: "Type text into the focused element",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          (
            "text",
            {
              type_: "string",
              description: Some("Text to type"),
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
          (
            "coordinate",
            {
              type_: "array",
              description: Some("[x, y] to click first"),
              properties: None,
              items: Some({
                type_: "number",
                description: None,
                properties: None,
                items: None,
                enum: None,
                required: None,
              }),
              enum: None,
              required: None,
            },
          ),
          (
            "tabId",
            {
              type_: "number",
              description: None,
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
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
    description: "Scroll the page",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          (
            "direction",
            {
              type_: "string",
              description: None,
              properties: None,
              items: None,
              enum: Some(["up", "down", "left", "right"]),
              required: None,
            },
          ),
          (
            "amount",
            {
              type_: "number",
              description: Some("Pixels (default: 300)"),
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
          (
            "coordinate",
            {
              type_: "array",
              description: None,
              properties: None,
              items: Some({
                type_: "number",
                description: None,
                properties: None,
                items: None,
                enum: None,
                required: None,
              }),
              enum: None,
              required: None,
            },
          ),
          (
            "tabId",
            {
              type_: "number",
              description: None,
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
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
    description: "Execute JavaScript in the page",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          (
            "code",
            {
              type_: "string",
              description: Some("JavaScript code"),
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
          (
            "tabId",
            {
              type_: "number",
              description: None,
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
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
    description: "Find elements by text or CSS selector",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          (
            "query",
            {
              type_: "string",
              description: Some("Text or selector"),
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
          (
            "tabId",
            {
              type_: "number",
              description: None,
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
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
          (
            "selector",
            {
              type_: "string",
              description: Some("CSS selector"),
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
          (
            "value",
            {
              type_: "string",
              description: Some("Value to set"),
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
          (
            "tabId",
            {
              type_: "number",
              description: None,
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
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
    description: "List open tabs",
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
        Dict.fromArray([
          (
            "url",
            {
              type_: "string",
              description: None,
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
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
    name: "tabs_close",
    description: "Close a tab",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          (
            "tabId",
            {
              type_: "number",
              description: None,
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
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
    name: "get_page_text",
    description: "Get page text content",
    inputSchema: {
      type_: "object",
      properties: Some(
        Dict.fromArray([
          (
            "tabId",
            {
              type_: "number",
              description: None,
              properties: None,
              items: None,
              enum: None,
              required: None,
            },
          ),
        ]),
      ),
      items: None,
      description: None,
      enum: None,
      required: None,
    },
  },
]

// --- Mutable state ---

type pendingRequest = {
  resolve: Js.Json.t => unit,
  reject: exn => unit,
}

let extensionSocket: ref<option<WebSocket.t>> = ref(None)
let requestId: ref<int> = ref(0)
let pendingRequests: Dict.t<pendingRequest> = Dict.make()

// --- Helpers ---

// Write a JSON-RPC message to stdout for Claude Code to consume.
let writeStdout = (message: 'a): unit => {
  let json = jsonStringify(message)
  let encoder = TextEncoder.make()
  let bytes = TextEncoder.encode(encoder, json ++ "\n")
  let _ = Stdout.writeSync(bytes)
}

// Log to stderr so it does not interfere with the MCP protocol on stdout.
let log = (args: array<string>): unit => {
  Console.error2("[MCP Server]", Array.join(args, " "))
}

// --- Extension communication ---

// Send a tool call to the Firefox extension via WebSocket and return a promise
// that resolves when the extension responds with a matching request id.
let callExtension = async (toolName: string, args: Js.Json.t): Js.Json.t => {
  let socket = extensionSocket.contents
  switch socket {
  | None => Exn.raiseError("Firefox extension not connected. Start Firefox with the extension loaded.")
  | Some(ws) =>
    if WebSocket.readyState(ws) !== WebSocket._OPEN {
      Exn.raiseError("Firefox extension not connected. Start Firefox with the extension loaded.")
    }

    requestId := requestId.contents + 1
    let id = requestId.contents
    let idStr = Int.toString(id)

    let result = await Promise.make((resolve, reject) => {
      Dict.set(pendingRequests, idStr, {resolve, reject})

      // Timeout after 30 seconds
      let _ = setTimeout(() => {
        switch Dict.get(pendingRequests, idStr) {
        | Some(_) => {
            Dict.delete(pendingRequests, idStr)
            reject(Exn.anyToExnInternal("Extension request timed out"))
          }
        | None => ()
        }
      }, 30000)
    })

    WebSocket.send(
      ws,
      jsonStringify({
        "jsonrpc": "2.0",
        "id": id,
        "method": "tools/call",
        "params": {"name": toolName, "arguments": args},
      }),
    )

    result
  }
}

// --- MCP request types ---

type mcpRequest = {
  id: option<int>,
  method: string,
  params: option<Js.Json.t>,
}

type toolCallParams = {
  name: string,
  arguments: Js.Json.t,
}

// Handle an MCP JSON-RPC request arriving from Claude Code on stdin.
// Routes initialize, tools/list, and tools/call to the appropriate handler.
let handleMcpRequest = async (request: mcpRequest): unit => {
  let {id, method, params} = request

  try {
    let result = switch method {
    | "initialize" =>
      Some(
        Obj.magic({
          "protocolVersion": "2024-11-05",
          "capabilities": {"tools": Js.Dict.empty()},
          "serverInfo": {"name": "claude-firefox-mcp", "version": "1.0.0"},
        }),
      )

    | "notifications/initialized" => {
        log(["MCP initialized"])
        None // No response needed for notifications
      }

    | "tools/list" => Some(Obj.magic({"tools": mcpTools}))

    | "tools/call" =>
      switch params {
      | Some(p) => {
          let toolParams: toolCallParams = Obj.magic(p)
          log(["Tool call:", toolParams.name])
          let extensionResult = await callExtension(toolParams.name, toolParams.arguments)
          Some(extensionResult)
        }
      | None => Exn.raiseError("Missing params for tools/call")
      }

    | _ => {
        writeStdout({
          "jsonrpc": "2.0",
          "id": id,
          "error": {"code": -32601, "message": `Method not found: ${method}`},
        })
        None
      }
    }

    switch result {
    | Some(r) => writeStdout({"jsonrpc": "2.0", "id": id, "result": r})
    | None => ()
    }
  } catch {
  | Exn.Error(err) => {
      let msg = switch Exn.message(err) {
      | Some(m) => m
      | None => "Unknown error"
      }
      log(["Error:", msg])
      writeStdout({
        "jsonrpc": "2.0",
        "id": id,
        "error": {"code": -32000, "message": msg},
      })
    }
  | err => {
      let msg = Obj.magic(err)->Js.String.make
      log(["Error:", msg])
      writeStdout({
        "jsonrpc": "2.0",
        "id": id,
        "error": {"code": -32000, "message": msg},
      })
    }
  }
}

// --- Extension message handling ---

// Process a JSON message received from the Firefox extension over WebSocket.
// Matches the message id to a pending request and resolves or rejects it.
type extensionMessage = {
  id: option<int>,
  result: option<Js.Json.t>,
  error: option<{"message": string}>,
}

let handleExtensionMessage = (data: string): unit => {
  try {
    let message: extensionMessage = jsonParse(data)

    switch message.id {
    | Some(msgId) => {
        let idStr = Int.toString(msgId)
        switch Dict.get(pendingRequests, idStr) {
        | Some({resolve, reject}) => {
            Dict.delete(pendingRequests, idStr)
            switch message.error {
            | Some(err) => reject(Exn.anyToExnInternal(err["message"]))
            | None =>
              switch message.result {
              | Some(r) => resolve(r)
              | None => resolve(Obj.magic(Js.null))
              }
            }
          }
        | None => ()
        }
      }
    | None => ()
    }
  } catch {
  | _ => log(["Failed to parse extension message"])
  }
}

// --- WebSocket server ---

// Start the WebSocket server that the Firefox extension connects to.
// Accepts upgrade requests and wires up message/close/error handlers.
let startWebSocketServer = (): unit => {
  log(["Starting WebSocket server on port", Int.toString(wsPort)])

  serve({port: wsPort}, req => {
    let headers = Fetch.Request.headers(req)
    let upgradeHeader = RequestHeaders.get(headers, "upgrade")

    switch Js.Nullable.toOption(upgradeHeader) {
    | Some("websocket") => {
        let {socket, response} = upgradeWebSocket(req)

        WebSocket.setOnopen(socket, () => {
          log(["Firefox extension connected"])
          extensionSocket := Some(socket)
        })

        WebSocket.setOnmessage(socket, event => {
          handleExtensionMessage(event["data"])
        })

        WebSocket.setOnclose(socket, () => {
          log(["Firefox extension disconnected"])
          switch extensionSocket.contents {
          | Some(current) if current === socket => extensionSocket := None
          | _ => ()
          }
        })

        WebSocket.setOnerror(socket, _err => {
          log(["WebSocket error"])
        })

        response
      }
    | _ => makeResponse("WebSocket required", {"status": 400})
    }
  })

  log(["WebSocket server started"])
}

// --- Stdin reader ---

// Read newline-delimited JSON-RPC messages from stdin and dispatch each
// as an MCP request. Buffers partial reads until a complete line arrives.
let readStdin = async (): unit => {
  let decoder = TextDecoder.make()
  let reader = Stdin.getReader(Stdin.stdin.readable)
  let buffer = ref("")

  log(["Reading from stdin..."])

  let continue_ = ref(true)
  while continue_.contents {
    let readResult = await ReadableStreamReader.read(reader)
    if readResult.done {
      log(["Stdin closed"])
      continue_ := false
    } else {
      switch Js.Nullable.toOption(readResult.value) {
      | Some(value) => {
          buffer :=
            buffer.contents ++
            TextDecoder.decodeWithOptions(decoder, value, {stream: true})

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
                  log(["Received:", request.method])
                  await handleMcpRequest(request)
                } catch {
                | _ => {
                    log(["Parse error"])
                    writeStdout({
                      "jsonrpc": "2.0",
                      "id": Js.null,
                      "error": {"code": -32700, "message": "Parse error"},
                    })
                  }
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

// --- Main entry point ---

// Start the MCP server: launch the WebSocket listener for the Firefox
// extension and then block reading MCP requests from stdin.
let main = async (): unit => {
  log(["Claude Firefox MCP Server starting..."])

  // Start WebSocket server in background (non-blocking)
  startWebSocketServer()

  // Read MCP from stdin (blocks until stdin closes)
  await readStdin()
}

main()
->Promise.catch(err => {
  log(["Fatal error:", Obj.magic(err)->Js.String.make])
  DenoProcess.exit(1)
  Promise.resolve()
})
->ignore
