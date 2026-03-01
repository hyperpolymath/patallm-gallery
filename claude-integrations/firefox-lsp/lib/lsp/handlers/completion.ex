# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

defmodule ClaudeFirefox.LSP.Handlers.Completion do
  @moduledoc """
  Provides auto-completion for Firefox Marionette automation.

  Supports:
  - Marionette protocol commands
  - WebDriver methods
  - CSS selectors and XPath
  """

  def handle(params, assigns) do
    uri = get_in(params, ["textDocument", "uri"])
    position = params["position"]

    # Get document text from state
    doc = get_in(assigns, [:documents, uri])
    text = if doc, do: doc.text, else: ""

    # Get line and character position
    line = position["line"]
    character = position["character"]

    # Get context around cursor
    context = get_line_context(text, line, character)

    # Provide completions based on context
    completions = complete_marionette(context, uri)

    completions
  end

  # Extract line context around cursor
  defp get_line_context(text, line, character) do
    lines = String.split(text, "\n")
    current_line = Enum.at(lines, line, "")
    before_cursor = String.slice(current_line, 0, character)

    %{
      line: current_line,
      before_cursor: before_cursor,
      trigger: get_trigger(before_cursor)
    }
  end

  # Detect completion trigger
  defp get_trigger(text) do
    cond do
      String.match?(text, ~r/find_element\($/) -> :locator_strategy
      String.match?(text, ~r/By\.$/) -> :webdriver_by
      String.match?(text, ~r/execute_script\($/) -> :javascript
      String.ends_with?(text, ".") -> :method
      true -> :none
    end
  end

  # Marionette/Firefox completions
  defp complete_marionette(context, uri) do
    case context.trigger do
      :locator_strategy ->
        ["css selector", "xpath", "id", "name", "tag name", "class name", "link text"]
        |> Enum.map(&create_completion_item(&1, "enum"))

      :webdriver_by ->
        ["CSS_SELECTOR", "XPATH", "ID", "NAME", "TAG_NAME", "CLASS_NAME", "LINK_TEXT"]
        |> Enum.map(&create_completion_item(&1, "enum"))

      :method ->
        [
          "navigate", "find_element", "find_elements",
          "click", "send_keys", "clear",
          "get_text", "get_attribute", "get_property",
          "execute_script", "execute_async_script",
          "screenshot", "get_page_source",
          "switch_to_frame", "switch_to_parent_frame",
          "get_window_handle", "get_window_handles",
          "switch_to_window", "close_window"
        ]
        |> Enum.map(&create_completion_item(&1, "function"))

      _ ->
        # Common Marionette/WebDriver methods
        methods = [
          "navigate_to", "get_url", "back", "forward", "refresh",
          "find_element", "find_elements", "click", "send_keys",
          "execute_script", "screenshot", "close"
        ]

        methods
        |> Enum.map(&create_completion_item(&1, "function"))
    end
  end

  # Create LSP completion item
  defp create_completion_item(label, kind_str) do
    kind = case kind_str do
      "function" -> 3    # Function
      "enum" -> 13       # Enum
      _ -> 1             # Text
    end

    %{
      "label" => label,
      "kind" => kind,
      "detail" => "#{kind_str}",
      "insertText" => label
    }
  end
end
