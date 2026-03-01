# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrainTest do
  use ExUnit.Case

  test "health_check returns system status" do
    health = DyadtBrain.health_check()
    assert health.brain == :ok
    assert %DateTime{} = health.timestamp
  end
end
