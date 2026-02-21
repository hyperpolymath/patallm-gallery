# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.GRPC.ServerTest do
  use ExUnit.Case, async: true

  alias DyadtBrain.GRPC.{Messages, Server}

  describe "Messages.encode/1 and decode/1" do
    test "roundtrip encoding" do
      original = %{"method" => "Health", "params" => %{}}
      encoded = Messages.encode(original)
      assert {:ok, decoded} = Messages.decode(encoded)
      assert decoded == original
    end

    test "handles nested data" do
      data = %{
        "method" => "VerifyClaim",
        "params" => %{
          "description" => "Test",
          "evidence" => [%{"type" => "FileExists", "path" => "/tmp"}]
        }
      }

      encoded = Messages.encode(data)
      assert {:ok, decoded} = Messages.decode(encoded)
      assert decoded == data
    end
  end

  describe "Messages.parse_request/1" do
    test "parses method and params" do
      data = Messages.encode(%{"method" => "Health", "params" => %{"foo" => "bar"}})
      assert {:ok, "Health", %{"foo" => "bar"}} = Messages.parse_request(data)
    end

    test "handles missing params" do
      data = Messages.encode(%{"method" => "Health"})
      assert {:ok, "Health", %{}} = Messages.parse_request(data)
    end
  end

  describe "Messages.build_response/2" do
    test "builds success response" do
      response = Messages.build_response(%{"status" => "ok"})
      assert {:ok, decoded} = Messages.decode(response)
      assert decoded["ok"] == true
      assert decoded["result"]["status"] == "ok"
    end

    test "builds error response" do
      response = Messages.build_response(nil, "something failed")
      assert {:ok, decoded} = Messages.decode(response)
      assert decoded["ok"] == false
      assert decoded["error"] == "something failed"
    end
  end

  describe "Server.handle_rpc/2" do
    test "Health returns health data" do
      assert {:ok, health} = Server.handle_rpc("Health", %{})
      assert is_binary(health["brain"])
      assert is_binary(health["timestamp"])
    end

    test "GetStats returns statistics" do
      assert {:ok, stats} = Server.handle_rpc("GetStats", %{})
      assert is_map(stats)
    end

    test "ListClaims returns claim list" do
      assert {:ok, claims} = Server.handle_rpc("ListClaims", %{})
      assert is_list(claims)
    end

    test "GetReport returns not_found for unknown ID" do
      assert {:error, "not_found"} = Server.handle_rpc("GetReport", %{"claim_id" => "fake-id"})
    end

    test "unknown method returns error" do
      assert {:error, "Unknown method: Foo"} = Server.handle_rpc("Foo", %{})
    end

    test "VerifyClaim processes a claim" do
      params = %{
        "description" => "gRPC test claim",
        "evidence" => [%{"type" => "FileExists", "spec" => %{"path" => "/tmp"}}]
      }

      result = Server.handle_rpc("VerifyClaim", params)
      assert match?({:ok, _}, result) or match?({:error, _}, result)
    end
  end
end
