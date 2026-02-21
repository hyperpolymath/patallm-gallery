// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// FFI bindings for the VS Code extension API and vscode-languageclient.
// Used by the Claude Firefox LSP extension to register commands,
// manage the language client lifecycle, and interact with the VS Code UI.

// --- Core VSCode types ---

module Disposable = {
  type t
}

module Uri = {
  type t

  @module("vscode") @scope("Uri")
  external parse: string => t = "parse"
}

// --- ExtensionContext ---

module ExtensionContext = {
  type t

  @get external subscriptions: t => array<Disposable.t> = "subscriptions"
}

// --- Workspace ---

module WorkspaceConfiguration = {
  type t

  @send external get: (t, string, string) => string = "get"
}

module FileSystemWatcher = {
  type t
}

module TextDocument = {
  type t
}

module Workspace = {
  @module("vscode") @scope("workspace")
  external getConfiguration: string => WorkspaceConfiguration.t = "getConfiguration"

  @module("vscode") @scope("workspace")
  external createFileSystemWatcher: string => FileSystemWatcher.t = "createFileSystemWatcher"

  @module("vscode") @scope("workspace")
  external openTextDocument: {"content": string, "language": string} => promise<TextDocument.t> =
    "openTextDocument"
}

// --- Window ---

module InputBoxOptions = {
  type t = {
    prompt: string,
    placeHolder: string,
  }
}

module Window = {
  @module("vscode") @scope("window")
  external showInputBox: InputBoxOptions.t => promise<Js.Nullable.t<string>> = "showInputBox"

  @module("vscode") @scope("window")
  external showInformationMessage: string => promise<unit> = "showInformationMessage"

  @module("vscode") @scope("window")
  external showQuickPick: (
    array<string>,
    {"placeHolder": string},
  ) => promise<Js.Nullable.t<string>> = "showQuickPick"

  @module("vscode") @scope("window")
  external showTextDocument: TextDocument.t => promise<unit> = "showTextDocument"
}

// --- Commands ---

module Commands = {
  @module("vscode") @scope("commands")
  external registerCommand: (string, unit => promise<unit>) => Disposable.t = "registerCommand"
}

// --- LanguageClient (from vscode-languageclient/node) ---

module ServerOptions = {
  type t = {
    command: string,
    args: array<string>,
  }
}

module LanguageClientOptions = {
  type documentSelector

  type synchronize = {fileEvents: FileSystemWatcher.t}

  type t = {
    documentSelector: array<documentSelector>,
    synchronize: synchronize,
  }
}

module ExecuteCommandParams = {
  type t = {
    command: string,
    arguments: array<Js.Json.t>,
  }
}

module LanguageClient = {
  type t

  @new @module("vscode-languageclient/node")
  external make: (
    string,
    string,
    ServerOptions.t,
    LanguageClientOptions.t,
  ) => t = "LanguageClient"

  @send external start: t => promise<unit> = "start"
  @send external stop: t => promise<unit> = "stop"
  @send external sendRequest: (t, string, ExecuteCommandParams.t) => promise<Js.Json.t> =
    "sendRequest"
}
