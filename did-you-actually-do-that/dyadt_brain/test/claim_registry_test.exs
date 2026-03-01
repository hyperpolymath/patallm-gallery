# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.ClaimRegistryTest do
  use ExUnit.Case

  alias DyadtBrain.ClaimRegistry

  test "register and retrieve a claim" do
    claim = %{"id" => "test-1", "description" => "Test claim"}
    {:ok, claim_id} = ClaimRegistry.register(claim)
    assert claim_id == "test-1"

    {:pending, :pending} = ClaimRegistry.get_report(claim_id)
  end

  test "update claim with report" do
    claim = %{"id" => "test-2", "description" => "Test claim 2"}
    {:ok, claim_id} = ClaimRegistry.register(claim)

    report = %{"overall_verdict" => "Confirmed"}
    :ok = ClaimRegistry.update(claim_id, :verified, report)

    {:ok, retrieved} = ClaimRegistry.get_report(claim_id)
    assert retrieved["overall_verdict"] == "Confirmed"
  end

  test "get_report returns not_found for unknown claim" do
    {:error, :not_found} = ClaimRegistry.get_report("nonexistent-id")
  end

  test "list_all returns all registered claims" do
    claim = %{"id" => "test-list-1", "description" => "Listed claim"}
    {:ok, _} = ClaimRegistry.register(claim)

    all = ClaimRegistry.list_all()
    assert Enum.any?(all, fn c -> c.id == "test-list-1" end)
  end

  test "status reports table size" do
    {:ok, size} = ClaimRegistry.status()
    assert is_integer(size)
  end
end
