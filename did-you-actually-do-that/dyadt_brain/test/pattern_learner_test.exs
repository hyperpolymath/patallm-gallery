# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.PatternLearnerTest do
  use ExUnit.Case, async: false

  alias DyadtBrain.PatternLearner

  setup do
    # Clear ETS table between tests
    try do
      :ets.delete_all_objects(:dyadt_pattern_learner)
    rescue
      _ -> :ok
    end

    :ok
  end

  describe "record_violation/3" do
    test "records a violation" do
      PatternLearner.record_violation("stub_detected", "test-source", %{"details" => "todo!()"})
      # Give cast time to process
      Process.sleep(50)

      {:ok, patterns} = PatternLearner.get_patterns()
      assert length(patterns) == 1
      assert hd(patterns).category == "stub_detected"
      assert hd(patterns).source == "test-source"
      assert hd(patterns).frequency == 1
    end

    test "increments frequency on repeat violations" do
      PatternLearner.record_violation("scope_mismatch", "repo-a")
      PatternLearner.record_violation("scope_mismatch", "repo-a")
      PatternLearner.record_violation("scope_mismatch", "repo-a")
      Process.sleep(50)

      {:ok, patterns} = PatternLearner.get_patterns()
      assert length(patterns) == 1
      assert hd(patterns).frequency == 3
    end
  end

  describe "get_patterns/1" do
    test "filters by source" do
      PatternLearner.record_violation("stub", "source-a")
      PatternLearner.record_violation("stub", "source-b")
      Process.sleep(50)

      {:ok, patterns} = PatternLearner.get_patterns(source: "source-a")
      assert length(patterns) == 1
      assert hd(patterns).source == "source-a"
    end

    test "filters by category" do
      PatternLearner.record_violation("stub", "src")
      PatternLearner.record_violation("scope", "src")
      Process.sleep(50)

      {:ok, patterns} = PatternLearner.get_patterns(category: "stub")
      assert length(patterns) == 1
      assert hd(patterns).category == "stub"
    end

    test "filters by min_frequency" do
      PatternLearner.record_violation("a", "src")
      PatternLearner.record_violation("b", "src")
      PatternLearner.record_violation("b", "src")
      PatternLearner.record_violation("b", "src")
      Process.sleep(50)

      {:ok, patterns} = PatternLearner.get_patterns(min_frequency: 3)
      assert length(patterns) == 1
      assert hd(patterns).category == "b"
    end
  end

  describe "get_risk_score/1" do
    test "returns 0.0 for unknown source" do
      {:ok, score} = PatternLearner.get_risk_score("unknown-source")
      assert score == 0.0
    end

    test "increases with more violations" do
      for _ <- 1..10 do
        PatternLearner.record_violation("stub", "risky-source")
      end

      Process.sleep(50)

      {:ok, score} = PatternLearner.get_risk_score("risky-source")
      assert score == 0.5
    end

    test "caps at 1.0" do
      for i <- 1..25 do
        PatternLearner.record_violation("cat_#{i}", "very-risky")
      end

      Process.sleep(50)

      {:ok, score} = PatternLearner.get_risk_score("very-risky")
      assert score == 1.0
    end
  end

  describe "trending_violations/1" do
    test "returns recent violations" do
      PatternLearner.record_violation("recent", "src")
      Process.sleep(50)

      {:ok, trending} = PatternLearner.trending_violations(24)
      assert length(trending) == 1
    end
  end

  describe "status/0" do
    test "returns status map" do
      status = PatternLearner.status()
      assert status.status == :ok
      assert is_integer(status.pattern_count)
    end
  end
end
