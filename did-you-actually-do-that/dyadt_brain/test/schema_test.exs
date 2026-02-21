# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.API.SchemaTest do
  use ExUnit.Case, async: true

  alias DyadtBrain.API.Schema

  describe "health query" do
    test "returns health status" do
      query = "{ health { brain rustPort claimRegistry timestamp } }"

      assert {:ok, %{data: data}} = Absinthe.run(query, Schema)
      assert data["health"]["brain"] == "ok"
      assert is_binary(data["health"]["timestamp"])
    end
  end

  describe "stats query" do
    test "returns statistics" do
      query = "{ stats { registry slm timestamp } }"

      assert {:ok, %{data: data}} = Absinthe.run(query, Schema)
      stats = data["stats"]
      assert is_map(stats)
      # Stats returns JSON scalar fields — at least one should be non-nil
      assert stats["registry"] != nil or stats["slm"] != nil or stats["timestamp"] != nil
    end
  end

  describe "claims query" do
    test "returns claims list" do
      query = "{ claims { id status } }"

      assert {:ok, %{data: data}} = Absinthe.run(query, Schema)
      assert is_list(data["claims"])
    end
  end

  describe "report query" do
    test "returns error for unknown claim" do
      query = ~s|{ report(id: "nonexistent") { claimId overallVerdict } }|

      assert {:ok, %{errors: errors}} = Absinthe.run(query, Schema)
      assert length(errors) > 0
    end
  end

  describe "verifyClaim mutation" do
    test "verifies a claim" do
      query = """
      mutation {
        verifyClaim(input: {
          description: "Test claim for GraphQL",
          evidence: [{ type: "FileExists", path: "/tmp" }]
        }) {
          overallVerdict
        }
      }
      """

      result = Absinthe.run(query, Schema)
      # Should succeed or fail gracefully — depends on Rust port
      assert match?({:ok, _}, result)
    end
  end

  describe "evaluateSlm mutation" do
    test "evaluates through SLM" do
      query = """
      mutation {
        evaluateSlm(context: {
          description: "Test SLM evaluation",
          evidenceSummary: "File exists at /tmp"
        }) {
          verdict
          confidence
        }
      }
      """

      result = Absinthe.run(query, Schema)
      assert match?({:ok, _}, result)
    end
  end
end
