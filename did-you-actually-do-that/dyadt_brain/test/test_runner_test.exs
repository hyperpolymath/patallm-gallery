# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.TestRunnerTest do
  use ExUnit.Case

  alias DyadtBrain.TestRunner

  @project_root Path.expand("../..", __DIR__)

  describe "detect_framework/1" do
    test "detects Rust framework from Cargo.toml" do
      frameworks = TestRunner.detect_framework(@project_root)
      assert Enum.any?(frameworks, fn {name, _cmd} -> name == "rust" end)
    end

    test "detects Elixir framework from mix.exs" do
      elixir_root = Path.expand("..", __DIR__)
      frameworks = TestRunner.detect_framework(elixir_root)
      assert Enum.any?(frameworks, fn {name, _cmd} -> name == "elixir" end)
    end

    test "returns empty list for non-project directory" do
      frameworks = TestRunner.detect_framework("/tmp")
      assert frameworks == []
    end
  end

  describe "run_tests/2" do
    test "runs Rust tests successfully" do
      {:ok, result} = TestRunner.run_tests(@project_root, framework: "rust")
      assert result["verdict"] == "Confirmed"
      assert result["framework"] == "rust"
      assert result["exit_code"] == 0
      assert is_integer(result["duration_ms"])
      assert result["duration_ms"] > 0
    end

    @tag :skip
    # Skipped: running `mix test` on itself causes infinite recursion.
    # Test manually with: mix test --only skip
    test "runs Elixir tests successfully" do
      elixir_root = Path.expand("..", __DIR__)
      {:ok, result} = TestRunner.run_tests(elixir_root, framework: "elixir")
      assert result["verdict"] == "Confirmed"
      assert result["framework"] == "elixir"
      assert result["exit_code"] == 0
    end

    test "returns inconclusive for directory without tests" do
      {:ok, result} = TestRunner.run_tests("/tmp")
      assert result["verdict"] == "Inconclusive"
    end
  end

  describe "regression_check/2" do
    test "reports no regression when tests pass" do
      {:ok, result} = TestRunner.regression_check(@project_root, framework: "rust")
      assert result["layer"] == "RegressionGuard"
      assert result["regression_detected"] == false
    end
  end
end
