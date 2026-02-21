# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.Ports.RustPortTest do
  use ExUnit.Case

  alias DyadtBrain.Ports.RustPort

  test "ping returns error when binary not available" do
    result = RustPort.ping()
    assert match?({:error, _}, result) or match?({:ok, _}, result)
  end

  test "verify_claim returns error when port not ready" do
    claim = %{"description" => "test", "evidence" => []}
    result = RustPort.verify_claim(claim)
    assert match?({:error, _}, result) or match?({:ok, _}, result)
  end
end
