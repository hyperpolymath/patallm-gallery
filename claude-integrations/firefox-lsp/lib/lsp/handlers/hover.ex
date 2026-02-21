# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

defmodule ClaudeFirefox.LSP.Handlers.Hover do
  @moduledoc """
  Provides hover documentation for Firefox Marionette automation.

  Shows:
  - Marionette command documentation
  - WebDriver method descriptions
  - Locator strategy information
  """

  def handle(params, assigns) do
    uri = get_in(params, ["textDocument", "uri"])
    position = params["position"]

    # Get document text from state
    doc = get_in(assigns, [:documents, uri])
    text = if doc, do: doc.text, else: ""

    # Get word at cursor position
    word = get_word_at_position(text, position["line"], position["character"])

    if word do
      # Get documentation
      docs = get_marionette_docs(word)

      if docs do
        %{
          "contents" => %{
            "kind" => "markdown",
            "value" => docs
          }
        }
      else
        nil
      end
    else
      nil
    end
  end

  # Extract word at position
  defp get_word_at_position(text, line, character) do
    lines = String.split(text, "\n")
    current_line = Enum.at(lines, line, "")

    # Find word boundaries (including underscores)
    before = String.slice(current_line, 0, character) |> String.reverse()
    after_text = String.slice(current_line, character, String.length(current_line))

    start = Regex.run(~r/^[a-zA-Z0-9_]*/, before) |> List.first() |> String.reverse()
    end_part = Regex.run(~r/^[a-zA-Z0-9_]*/, after_text) |> List.first()

    word = start <> end_part
    if String.length(word) > 0, do: word, else: nil
  end

  # Marionette/Firefox documentation
  defp get_marionette_docs(word) do
    docs = %{
      "navigate" => "**navigate(url)** - Navigate to URL\n\nNavigates the browser to the specified URL.\n\nExample: `navigate(\"https://example.com\")`",
      "navigate_to" => "**navigate_to(url)** - Navigate to URL\n\nAlias for navigate. Navigates to the specified URL.",
      "get_url" => "**get_url()** - Get current URL\n\nReturns the current page URL.",
      "back" => "**back()** - Navigate back\n\nNavigates to the previous page in history.",
      "forward" => "**forward()** - Navigate forward\n\nNavigates to the next page in history.",
      "refresh" => "**refresh()** - Refresh page\n\nReloads the current page.",
      "find_element" => "**find_element(strategy, selector)** - Find element\n\nFinds the first element matching the selector.\n\nStrategies: 'css selector', 'xpath', 'id', 'name'\n\nExample: `find_element(\"css selector\", \"#submit-btn\")`",
      "find_elements" => "**find_elements(strategy, selector)** - Find elements\n\nFinds all elements matching the selector.\n\nReturns a list of elements.",
      "click" => "**click(element)** - Click element\n\nClicks on the specified element.",
      "send_keys" => "**send_keys(element, text)** - Send keystrokes\n\nSends text to the specified element.\n\nExample: `send_keys(input_elem, \"Hello World\")`",
      "clear" => "**clear(element)** - Clear element\n\nClears the text in an input or textarea element.",
      "get_text" => "**get_text(element)** - Get element text\n\nReturns the visible text content of the element.",
      "get_attribute" => "**get_attribute(element, name)** - Get attribute\n\nReturns the value of the specified attribute.\n\nExample: `get_attribute(elem, \"href\")`",
      "get_property" => "**get_property(element, name)** - Get property\n\nReturns the value of the specified DOM property.",
      "execute_script" => "**execute_script(script, args)** - Execute JavaScript\n\nExecutes JavaScript code in the page context.\n\nExample: `execute_script(\"return document.title\", [])`",
      "execute_async_script" => "**execute_async_script(script, args)** - Execute async JavaScript\n\nExecutes asynchronous JavaScript code.",
      "screenshot" => "**screenshot()** - Take screenshot\n\nCaptures a screenshot of the current page.\n\nReturns PNG image data.",
      "get_page_source" => "**get_page_source()** - Get page HTML\n\nReturns the HTML source of the current page.",
      "switch_to_frame" => "**switch_to_frame(frame)** - Switch to frame\n\nSwitches context to the specified frame.",
      "switch_to_parent_frame" => "**switch_to_parent_frame()** - Switch to parent frame\n\nSwitches context to the parent frame.",
      "get_window_handle" => "**get_window_handle()** - Get current window handle\n\nReturns the handle of the current window.",
      "get_window_handles" => "**get_window_handles()** - Get all window handles\n\nReturns handles of all open windows.",
      "switch_to_window" => "**switch_to_window(handle)** - Switch to window\n\nSwitches to the window with the specified handle.",
      "close_window" => "**close_window()** - Close current window\n\nCloses the current browser window.",
      "CSS_SELECTOR" => "**CSS_SELECTOR** - CSS selector locator\n\nUse CSS selectors to find elements.\n\nExample: `#id`, `.class`, `div > p`",
      "XPATH" => "**XPATH** - XPath locator\n\nUse XPath expressions to find elements.\n\nExample: `//div[@class='content']`",
      "ID" => "**ID** - ID locator\n\nFinds element by ID attribute.",
      "NAME" => "**NAME** - Name locator\n\nFinds element by name attribute.",
      "TAG_NAME" => "**TAG_NAME** - Tag name locator\n\nFinds elements by HTML tag name.",
      "CLASS_NAME" => "**CLASS_NAME** - Class name locator\n\nFinds elements by CSS class name.",
      "LINK_TEXT" => "**LINK_TEXT** - Link text locator\n\nFinds anchor elements by exact link text."
    }

    Map.get(docs, word)
  end
end
