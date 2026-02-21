# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.RegressionBaselineTest do
  use ExUnit.Case, async: false

  alias DyadtBrain.RegressionBaseline

  @test_repo_path System.tmp_dir!() |> Path.join("dyadt_baseline_test")

  setup do
    File.rm_rf!(@test_repo_path)
    File.mkdir_p!(@test_repo_path)

    on_exit(fn ->
      File.rm_rf!(@test_repo_path)
    end)

    :ok
  end

  describe "store_baseline/2" do
    test "stores baseline successfully" do
      results = %{
        "verdict" => "Confirmed",
        "test_summary" => %{"passed" => 10, "failed" => 0}
      }

      assert :ok = RegressionBaseline.store_baseline(@test_repo_path, results)
    end

    test "persists to disk" do
      results = %{"verdict" => "Confirmed"}
      :ok = RegressionBaseline.store_baseline(@test_repo_path, results)

      baseline_path = Path.join(@test_repo_path, ".dyadt/baseline.json")
      assert File.exists?(baseline_path)

      {:ok, contents} = File.read(baseline_path)
      {:ok, data} = Jason.decode(contents)
      assert data["results"]["verdict"] == "Confirmed"
    end
  end

  describe "load_baseline/1" do
    test "loads stored baseline" do
      results = %{"verdict" => "Refuted", "test_summary" => %{"failed" => 3}}
      :ok = RegressionBaseline.store_baseline(@test_repo_path, results)

      {:ok, baseline} = RegressionBaseline.load_baseline(@test_repo_path)
      assert baseline["results"]["verdict"] == "Refuted"
      assert baseline["repo"] == @test_repo_path
    end

    test "returns error for non-existent baseline" do
      fake_path = Path.join(System.tmp_dir!(), "dyadt_no_baseline_#{:rand.uniform(100000)}")
      assert {:error, :no_baseline} = RegressionBaseline.load_baseline(fake_path)
    end
  end

  describe "compare/2" do
    test "detects regression when passing tests start failing" do
      baseline = %{
        "verdict" => "Confirmed",
        "test_summary" => %{"passed" => 10, "failed" => 0}
      }

      :ok = RegressionBaseline.store_baseline(@test_repo_path, baseline)

      current = %{
        "verdict" => "Refuted",
        "test_summary" => %{"passed" => 7, "failed" => 3}
      }

      {:ok, comparison} = RegressionBaseline.compare(@test_repo_path, current)
      assert comparison["regression_detected"] == true
      assert length(comparison["regressions"]) > 0
    end

    test "detects improvement when failing tests start passing" do
      baseline = %{
        "verdict" => "Refuted",
        "test_summary" => %{"passed" => 5, "failed" => 5}
      }

      :ok = RegressionBaseline.store_baseline(@test_repo_path, baseline)

      current = %{
        "verdict" => "Confirmed",
        "test_summary" => %{"passed" => 10, "failed" => 0}
      }

      {:ok, comparison} = RegressionBaseline.compare(@test_repo_path, current)
      assert comparison["regression_detected"] == false
      assert length(comparison["improvements"]) > 0
    end

    test "handles no baseline gracefully" do
      fresh_path = Path.join(System.tmp_dir!(), "dyadt_fresh_#{:rand.uniform(100000)}")

      current = %{
        "verdict" => "Confirmed",
        "test_summary" => %{"passed" => 10, "failed" => 0}
      }

      {:ok, comparison} = RegressionBaseline.compare(fresh_path, current)
      assert comparison["regression_detected"] == false
      assert comparison["baseline_verdict"] == "None"
      assert comparison["note"] =~ "No baseline"
    end

    test "detects new failures increase" do
      baseline = %{
        "verdict" => "Inconclusive",
        "test_summary" => %{"passed" => 8, "failed" => 2}
      }

      :ok = RegressionBaseline.store_baseline(@test_repo_path, baseline)

      current = %{
        "verdict" => "Inconclusive",
        "test_summary" => %{"passed" => 5, "failed" => 5}
      }

      {:ok, comparison} = RegressionBaseline.compare(@test_repo_path, current)
      assert comparison["regression_detected"] == true
    end
  end
end
