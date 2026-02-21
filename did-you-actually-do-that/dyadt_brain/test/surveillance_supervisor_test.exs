# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.SurveillanceSupervisorTest do
  use ExUnit.Case

  alias DyadtBrain.SurveillanceSupervisor

  describe "evaluate/1" do
    test "evaluates clean claim as confirmed" do
      context = %{
        "claim_text" => "Added authentication module",
        "evidence_summary" => "FileExists: Confirmed, ContentHash: Confirmed",
        "artifact_sample" =>
          "pub fn login(user: &str, pass: &str) -> Result<Token, Error> {\n    db.verify(user, hash(pass))\n}\n"
      }

      {:ok, result} = SurveillanceSupervisor.evaluate(context)
      assert result["verdict"] in ["Confirmed", "Escalate"]
      assert is_float(result["confidence"])
      assert is_binary(result["reasoning"])
      assert is_list(result["votes"])
      assert is_binary(result["eval_id"])
    end

    test "evaluates stub-laden claim as refuted" do
      context = %{
        "claim_text" => "Implemented full parser",
        "evidence_summary" => "FileExists: Confirmed",
        "artifact_sample" => "fn parse() { todo!() }\nfn lex() { unimplemented!() }\n"
      }

      {:ok, result} = SurveillanceSupervisor.evaluate(context)
      assert result["verdict"] in ["Refuted", "Escalate"]
    end

    test "evaluates overconfident claim with suspicion" do
      context = %{
        "claim_text" => "100% complete, everything works perfectly",
        "evidence_summary" => "",
        "artifact_sample" => "fn main() { println!(\"hello\"); }\n"
      }

      {:ok, result} = SurveillanceSupervisor.evaluate(context)
      # Should be refuted or escalated due to overconfidence
      assert result["verdict"] in ["Refuted", "Escalate"]
    end

    test "returns eval_id that can be retrieved" do
      context = %{
        "claim_text" => "Simple task",
        "evidence_summary" => "Confirmed",
        "artifact_sample" => "fn hello() { println!(\"world\"); } // real code with real logic\n"
      }

      {:ok, result} = SurveillanceSupervisor.evaluate(context)
      eval_id = result["eval_id"]

      {:ok, retrieved} = SurveillanceSupervisor.get_evaluation(eval_id)
      assert retrieved["eval_id"] == eval_id
    end

    test "unknown eval_id returns not_found" do
      assert {:error, :not_found} = SurveillanceSupervisor.get_evaluation("nonexistent")
    end
  end

  describe "stats/0" do
    test "returns statistics" do
      {:ok, stats} = SurveillanceSupervisor.stats()
      assert is_integer(stats.total)
      assert is_integer(stats.confirmed)
      assert is_integer(stats.refuted)
      assert is_integer(stats.escalated)
    end
  end
end
