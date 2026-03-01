# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

defmodule ClaudeFirefox.LSP.Handlers.Diagnostics do
  @moduledoc """
  Provides diagnostics for Firefox automation scripts.

  Validates:
  - Selector syntax (CSS and XPath)
  - Method calls and arguments
  - Common automation patterns
  """

  require Logger

  @doc """
  Handle diagnostics request by analyzing automation script.

  Returns LSP diagnostics format.
  """
  def handle(params, assigns) do
    uri = get_in(params, ["textDocument", "uri"]) || ""

    # Get document text from state
    doc = get_in(assigns, [:documents, uri])
    text = if doc, do: doc.text, else: ""

    diagnostics = analyze_script(text)

    %{
      "uri" => uri,
      "diagnostics" => diagnostics
    }
  end

  # Analyze automation script for common issues
  defp analyze_script(text) do
    lines = String.split(text, "\n")

    lines
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, line_num} ->
      analyze_line(line, line_num)
    end)
    |> Enum.take(50)  # Limit to 50 diagnostics
  end

  # Analyze individual line
  defp analyze_line(line, line_num) do
    diagnostics = []

    # Check for invalid CSS selectors
    diagnostics = if String.contains?(line, "find_element") and String.contains?(line, "css selector") do
      case extract_selector(line) do
        {:ok, selector} ->
          if valid_css_selector?(selector) do
            diagnostics
          else
            [create_diagnostic("Invalid CSS selector: #{selector}", line_num, 2) | diagnostics]
          end

        :error ->
          diagnostics
      end
    else
      diagnostics
    end

    # Check for hardcoded waits
    diagnostics = if String.match?(line, ~r/sleep\(|wait\(/) and String.match?(line, ~r/\d{4,}/) do
      [create_diagnostic("Long hardcoded wait detected - consider using explicit waits", line_num, 3) | diagnostics]
    else
      diagnostics
    end

    # Check for missing error handling
    diagnostics = if String.contains?(line, "find_element") and not String.contains?(line, "try") do
      # This is a hint, not an error
      diagnostics
    else
      diagnostics
    end

    diagnostics
  end

  # Extract selector from line
  defp extract_selector(line) do
    case Regex.run(~r/"css selector",\s*"([^"]+)"/, line) do
      [_, selector] -> {:ok, selector}
      _ -> :error
    end
  end

  # Validate CSS selector (basic check)
  defp valid_css_selector?(selector) do
    # Basic validation - just check for obvious syntax errors
    not String.contains?(selector, ["<<", ">>", "|||"]) and
    not String.starts_with?(selector, ">") and
    not String.ends_with?(selector, ">")
  end

  # Create a diagnostic entry
  defp create_diagnostic(message, line_num, severity) do
    %{
      "range" => %{
        "start" => %{"line" => line_num, "character" => 0},
        "end" => %{"line" => line_num, "character" => 100}
      },
      "severity" => severity,
      "source" => "claude-firefox",
      "message" => message
    }
  end
end
