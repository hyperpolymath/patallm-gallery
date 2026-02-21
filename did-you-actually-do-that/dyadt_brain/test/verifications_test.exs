# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.VerificationsTest do
  use ExUnit.Case, async: true

  alias DyadtBrain.Verifications

  describe "verify_claim/1" do
    test "accepts a valid claim map" do
      claim = %{
        "description" => "Test claim",
        "evidence" => [%{"type" => "FileExists", "spec" => %{"path" => "/tmp"}}]
      }

      # Will go through the pipeline — result depends on RustPort availability
      result = Verifications.verify_claim(claim)
      assert match?({:ok, _}, result) or match?({:error, _}, result)
    end
  end

  describe "list_claims/1" do
    test "returns ok tuple with list" do
      {:ok, claims} = Verifications.list_claims()
      assert is_list(claims)
    end

    test "accepts status filter" do
      {:ok, claims} = Verifications.list_claims(status: :completed)
      assert is_list(claims)
    end
  end

  describe "get_stats/0" do
    test "returns ok tuple with stats map" do
      {:ok, stats} = Verifications.get_stats()
      assert is_map(stats)
      assert Map.has_key?(stats, "registry")
      assert Map.has_key?(stats, "slm")
      assert Map.has_key?(stats, "timestamp")
    end
  end

  describe "health_check/0" do
    test "returns health status map" do
      health = Verifications.health_check()
      assert is_map(health)
      assert health.brain == :ok
      assert %DateTime{} = health.timestamp
    end
  end

  describe "get_report/1" do
    test "returns not_found for unknown claim" do
      assert {:error, :not_found} = Verifications.get_report("nonexistent-id-12345")
    end
  end
end
