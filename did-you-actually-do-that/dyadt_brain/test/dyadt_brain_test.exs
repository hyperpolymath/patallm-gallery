# SPDX-License-Identifier: MPL-2.0
# Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule DyadtBrainTest do
  use ExUnit.Case

  test "health_check returns system status" do
    health = DyadtBrain.health_check()
    assert health.brain == :ok
    assert %DateTime{} = health.timestamp
  end
end
