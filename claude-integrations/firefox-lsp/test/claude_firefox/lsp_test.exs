# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

defmodule ClaudeFirefox.LSPTest do
  use ExUnit.Case
  doctest ClaudeFirefox.LSP

  test "version returns string" do
    assert is_binary(ClaudeFirefox.LSP.version())
  end

  test "adapters returns list" do
    adapters = ClaudeFirefox.LSP.adapters()
    assert is_list(adapters)
    assert ClaudeFirefox.Adapters.Firefox in adapters
  end
end
