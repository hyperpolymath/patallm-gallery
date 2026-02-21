// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// Claude Firefox LSP - VS Code Extension
// Registers commands for Firefox browser control (navigate, click, type,
// screenshot, executeJs, getContent, detectBrowsers) and manages the
// language client lifecycle connecting to the claude-firefox-lsp server.

open Vscode

// --- Module-level state ---

let client: ref<option<LanguageClient.t>> = ref(None)

// --- Helper to push a disposable into the extension context ---

let pushSubscription = (context: ExtensionContext.t, disposable: Disposable.t): unit => {
  let subs = ExtensionContext.subscriptions(context)
  let _ = Js.Array2.push(subs, disposable)
}

// --- Activate ---

// Called by VS Code when the extension is activated.
// Sets up the language client and registers all browser control commands.
let activate = (context: ExtensionContext.t): unit => {
  Console.log("Claude Firefox LSP extension activated")

  // Read server path from configuration
  let config = Workspace.getConfiguration("claude-firefox-lsp")
  let serverPath = WorkspaceConfiguration.get(config, "serverPath", "claude-firefox-lsp")

  let serverOptions: ServerOptions.t = {
    command: serverPath,
    args: [],
  }

  let clientOptions: LanguageClientOptions.t = {
    documentSelector: [],
    synchronize: {
      fileEvents: Workspace.createFileSystemWatcher("**/*"),
    },
  }

  let lc = LanguageClient.make(
    "claude-firefox-lsp",
    "Claude Firefox LSP",
    serverOptions,
    clientOptions,
  )

  client := Some(lc)

  // --- Navigate command ---
  pushSubscription(
    context,
    Commands.registerCommand("claude-firefox-lsp.navigate", async () => {
      let urlResult = await Window.showInputBox({
        prompt: "Enter URL to navigate to",
        placeHolder: "https://example.com",
      })

      switch Js.Nullable.toOption(urlResult) {
      | Some(url) => {
          let _ = await LanguageClient.sendRequest(
            lc,
            "workspace/executeCommand",
            {
              command: "firefox.navigate",
              arguments: [Obj.magic(url)],
            },
          )
          let _ = await Window.showInformationMessage(`Navigated to ${url}`)
        }
      | None => ()
      }
    }),
  )

  // --- Click command ---
  pushSubscription(
    context,
    Commands.registerCommand("claude-firefox-lsp.click", async () => {
      let selectorResult = await Window.showInputBox({
        prompt: "Enter CSS selector or XPath",
        placeHolder: "#submit-button",
      })

      switch Js.Nullable.toOption(selectorResult) {
      | Some(selector) => {
          let _ = await LanguageClient.sendRequest(
            lc,
            "workspace/executeCommand",
            {
              command: "firefox.click",
              arguments: [Obj.magic(selector)],
            },
          )
          let _ = await Window.showInformationMessage(`Clicked element: ${selector}`)
        }
      | None => ()
      }
    }),
  )

  // --- Type text command ---
  pushSubscription(
    context,
    Commands.registerCommand("claude-firefox-lsp.typeText", async () => {
      let selectorResult = await Window.showInputBox({
        prompt: "Enter CSS selector or XPath",
        placeHolder: "#search-input",
      })

      switch Js.Nullable.toOption(selectorResult) {
      | Some(selector) => {
          let textResult = await Window.showInputBox({
            prompt: "Enter text to type",
            placeHolder: "Hello, world!",
          })

          switch Js.Nullable.toOption(textResult) {
          | Some(text) => {
              let _ = await LanguageClient.sendRequest(
                lc,
                "workspace/executeCommand",
                {
                  command: "firefox.typeText",
                  arguments: [Obj.magic(selector), Obj.magic(text)],
                },
              )
              let _ = await Window.showInformationMessage(`Typed text into: ${selector}`)
            }
          | None => ()
          }
        }
      | None => ()
      }
    }),
  )

  // --- Screenshot command ---
  pushSubscription(
    context,
    Commands.registerCommand("claude-firefox-lsp.screenshot", async () => {
      let result = await LanguageClient.sendRequest(
        lc,
        "workspace/executeCommand",
        {
          command: "firefox.screenshot",
          arguments: [],
        },
      )

      // Check if the result contains a screenshot field
      let resultDict: option<Dict.t<Js.Json.t>> = Obj.magic(Js.Nullable.toOption(Obj.magic(result)))
      switch resultDict {
      | Some(d) =>
        switch Dict.get(d, "screenshot") {
        | Some(_screenshot) =>
          let _ = await Window.showInformationMessage("Screenshot captured")
        // TODO: Display or save screenshot
        | None => ()
        }
      | None => ()
      }
    }),
  )

  // --- Execute JS command ---
  pushSubscription(
    context,
    Commands.registerCommand("claude-firefox-lsp.executeJs", async () => {
      let jsResult = await Window.showInputBox({
        prompt: "Enter JavaScript to execute",
        placeHolder: "return document.title;",
      })

      switch Js.Nullable.toOption(jsResult) {
      | Some(javascript) => {
          let result = await LanguageClient.sendRequest(
            lc,
            "workspace/executeCommand",
            {
              command: "firefox.executeJs",
              arguments: [Obj.magic(javascript)],
            },
          )
          let resultStr = Js.Json.stringify(result)
          let _ = await Window.showInformationMessage(`Result: ${resultStr}`)
        }
      | None => ()
      }
    }),
  )

  // --- Get content command ---
  pushSubscription(
    context,
    Commands.registerCommand("claude-firefox-lsp.getContent", async () => {
      let formatResult = await Window.showQuickPick(
        ["html", "text", "dom"],
        {"placeHolder": "Select content format"},
      )

      switch Js.Nullable.toOption(formatResult) {
      | Some(format) => {
          let result = await LanguageClient.sendRequest(
            lc,
            "workspace/executeCommand",
            {
              command: "firefox.getContent",
              arguments: [Obj.magic({"format": format})],
            },
          )

          let resultDict: option<Dict.t<Js.Json.t>> = Obj.magic(
            Js.Nullable.toOption(Obj.magic(result)),
          )
          switch resultDict {
          | Some(d) =>
            switch Dict.get(d, "content") {
            | Some(content) => {
                let contentStr: string = Obj.magic(content)
                let language = if format === "html" {
                  "html"
                } else {
                  "plaintext"
                }
                let doc = await Workspace.openTextDocument({
                  "content": contentStr,
                  "language": language,
                })
                let _ = await Window.showTextDocument(doc)
              }
            | None => ()
            }
          | None => ()
          }
        }
      | None => ()
      }
    }),
  )

  // --- Detect browsers command ---
  pushSubscription(
    context,
    Commands.registerCommand("claude-firefox-lsp.detectBrowsers", async () => {
      let result = await LanguageClient.sendRequest(
        lc,
        "workspace/executeCommand",
        {
          command: "firefox.detectBrowsers",
          arguments: [],
        },
      )

      let resultDict: option<Dict.t<Js.Json.t>> = Obj.magic(
        Js.Nullable.toOption(Obj.magic(result)),
      )
      switch resultDict {
      | Some(d) =>
        switch Dict.get(d, "browsers") {
        | Some(browsers) => {
            let browserArr: array<(string, {"name": string})> = Obj.magic(browsers)
            let names = Array.map(browserArr, ((_, b)) => b["name"])
            let joined = Array.join(names, ", ")
            let _ = await Window.showInformationMessage(`Available browsers: ${joined}`)
          }
        | None => ()
        }
      | None => ()
      }
    }),
  )

  // Start the language client
  let _ = LanguageClient.start(lc)
}

// --- Deactivate ---

// Called by VS Code when the extension is deactivated.
// Stops the language client if it is running.
let deactivate = (): option<promise<unit>> => {
  switch client.contents {
  | Some(lc) => Some(LanguageClient.stop(lc))
  | None => None
  }
}
