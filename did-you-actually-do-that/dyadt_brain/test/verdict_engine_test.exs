# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.VerdictEngineTest do
  use ExUnit.Case

  alias DyadtBrain.VerdictEngine

  test "empty results → Unverifiable" do
    assert VerdictEngine.aggregate([]) == "Unverifiable"
  end

  test "all Confirmed → Confirmed" do
    results = [
      %{"evidence_result" => %{"verdict" => "Confirmed"}},
      %{"evidence_result" => %{"verdict" => "Confirmed"}}
    ]

    assert VerdictEngine.aggregate(results) == "Confirmed"
  end

  test "any Refuted → Refuted" do
    results = [
      %{"evidence_result" => %{"verdict" => "Confirmed"}},
      %{"evidence_result" => %{"verdict" => "Refuted"}}
    ]

    assert VerdictEngine.aggregate(results) == "Refuted"
  end

  test "all Unverifiable → Unverifiable" do
    results = [
      %{"evidence_result" => %{"verdict" => "Unverifiable"}},
      %{"evidence_result" => %{"verdict" => "Unverifiable"}}
    ]

    assert VerdictEngine.aggregate(results) == "Unverifiable"
  end

  test "mixed without Refuted → Inconclusive" do
    results = [
      %{"evidence_result" => %{"verdict" => "Confirmed"}},
      %{"evidence_result" => %{"verdict" => "Inconclusive"}}
    ]

    assert VerdictEngine.aggregate(results) == "Inconclusive"
  end

  test "works with flat verdict keys" do
    results = [
      %{"verdict" => "Confirmed"},
      %{"verdict" => "Confirmed"}
    ]

    assert VerdictEngine.aggregate(results) == "Confirmed"
  end
end
