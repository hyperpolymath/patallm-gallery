# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.API.Schema do
  @moduledoc """
  GraphQL schema for the DYADT Brain verification API.

  Provides the same functionality as the REST API through a GraphQL interface.
  All resolvers delegate to `DyadtBrain.Verifications`.

  ## Example Queries

      # Verify a claim
      mutation {
        verifyClaim(input: {
          description: "Created config file",
          evidence: [{ type: "FileExists", path: "/tmp/config.json" }]
        }) {
          overallVerdict
          evidenceResults { verdict details }
        }
      }

      # Get health status
      query { health { brain rustPort claimRegistry } }

      # Get statistics
      query { stats { registry slm timestamp } }
  """

  use Absinthe.Schema

  alias DyadtBrain.Verifications

  # Types

  object :verification_report do
    field :claim_id, :string
    field :overall_verdict, :string
    field :evidence_results, list_of(:evidence_result)
    field :verified_at, :string
    field :layer_results, list_of(:layer_result)
  end

  object :evidence_result do
    field :verdict, :string
    field :details, :string
    field :spec_type, :string
  end

  object :layer_result do
    field :layer, :string
    field :layer_number, :integer
    field :evidence_result, :evidence_result
  end

  object :claim do
    field :id, :string
    field :description, :string
    field :status, :string
    field :source, :string
  end

  object :health_status do
    field :brain, :string
    field :rust_port, :string
    field :claim_registry, :string
    field :pattern_learner, :string
    field :timestamp, :string
  end

  object :stats do
    field :registry, :json
    field :slm, :json
    field :trending_violations, :json
    field :timestamp, :string
  end

  object :slm_result do
    field :verdict, :string
    field :confidence, :float
    field :reasoning, :string
    field :votes, list_of(:slm_vote)
  end

  object :slm_vote do
    field :model, :string
    field :decision, :string
    field :confidence, :float
    field :reasoning, :string
  end

  # JSON scalar for unstructured data
  scalar :json, name: "JSON" do
    serialize fn value -> value end
    parse fn
      %Absinthe.Blueprint.Input.String{value: value} ->
        case Jason.decode(value) do
          {:ok, decoded} -> {:ok, decoded}
          _ -> {:ok, value}
        end

      %Absinthe.Blueprint.Input.Null{} ->
        {:ok, nil}

      value ->
        {:ok, value}
    end
  end

  # Inputs

  input_object :claim_input do
    field :description, non_null(:string)
    field :evidence, list_of(:evidence_input)
    field :source, :string
  end

  input_object :evidence_input do
    field :type, non_null(:string)
    field :path, :string
    field :sha256, :string
    field :substring, :string
    field :pattern, :string
    field :command, :string
    field :args, list_of(:string)
    field :name, :string
    field :expected, :string
  end

  input_object :slm_context_input do
    field :description, :string
    field :evidence_summary, :string
    field :layer_results, :json
    field :violation_signals, list_of(:string)
  end

  # Queries

  query do
    @desc "Get a verification report by claim ID"
    field :report, :verification_report do
      arg :id, non_null(:string)

      resolve fn %{id: id}, _ ->
        case Verifications.get_report(id) do
          {:ok, report} -> {:ok, normalize_report(report)}
          {:pending, status} -> {:ok, %{claim_id: id, overall_verdict: "pending_#{status}"}}
          {:error, :not_found} -> {:error, "Claim not found"}
        end
      end
    end

    @desc "List all claims"
    field :claims, list_of(:claim) do
      arg :status, :string

      resolve fn args, _ ->
        opts =
          case Map.get(args, :status) do
            nil -> []
            status -> [status: safe_status_atom(status)]
          end

        case Verifications.list_claims(opts) do
          {:ok, claims} -> {:ok, Enum.map(claims, &normalize_claim/1)}
          error -> error
        end
      end
    end

    @desc "System health check"
    field :health, :health_status do
      resolve fn _, _ ->
        health = Verifications.health_check()

        {:ok,
         %{
           brain: to_string(health.brain),
           rust_port: inspect(health.rust_port),
           claim_registry: inspect(health.claim_registry),
           pattern_learner: inspect(health.pattern_learner),
           timestamp: DateTime.to_iso8601(health.timestamp)
         }}
      end
    end

    @desc "Verification statistics"
    field :stats, :stats do
      resolve fn _, _ ->
        case Verifications.get_stats() do
          {:ok, stats} ->
            {:ok,
             %{
               registry: stats["registry"],
               slm: stats["slm"],
               trending_violations: stats["trending_violations"],
               timestamp: stats["timestamp"]
             }}

          error ->
            error
        end
      end
    end
  end

  # Mutations

  mutation do
    @desc "Verify a single claim"
    field :verify_claim, :verification_report do
      arg :input, non_null(:claim_input)

      resolve fn %{input: input}, _ ->
        claim = build_claim_map(input)

        case Verifications.verify_claim(claim) do
          {:ok, report} -> {:ok, normalize_report(report)}
          {:error, reason} -> {:error, to_string(reason)}
        end
      end
    end

    @desc "Verify a batch of claims"
    field :verify_batch, list_of(:verification_report) do
      arg :claims, non_null(list_of(non_null(:claim_input)))

      resolve fn %{claims: inputs}, _ ->
        claims = Enum.map(inputs, &build_claim_map/1)

        case Verifications.verify_batch(claims) do
          {:ok, reports} -> {:ok, Enum.map(reports, &normalize_report/1)}
          {:error, reason} -> {:error, to_string(reason)}
        end
      end
    end

    @desc "Evaluate a claim through the SLM ensemble"
    field :evaluate_slm, :slm_result do
      arg :context, non_null(:slm_context_input)

      resolve fn %{context: ctx}, _ ->
        context = Map.new(ctx, fn {k, v} -> {to_string(k), v} end)

        case Verifications.evaluate_slm(context) do
          {:ok, result} -> {:ok, normalize_slm_result(result)}
          {:error, reason} -> {:error, to_string(reason)}
        end
      end
    end
  end

  # Private helpers

  defp build_claim_map(input) do
    evidence =
      (input[:evidence] || [])
      |> Enum.map(fn ev ->
        ev_map = Map.new(ev, fn {k, v} -> {to_string(k), v} end)
        %{"type" => ev_map["type"], "spec" => Map.drop(ev_map, ["type"])}
      end)

    %{
      "description" => input.description,
      "evidence" => evidence,
      "source" => input[:source]
    }
  end

  defp normalize_report(report) when is_map(report) do
    %{
      claim_id: report["claim_id"] || report[:claim_id],
      overall_verdict: to_string(report["overall_verdict"] || report[:overall_verdict]),
      verified_at: report["verified_at"] || report[:verified_at],
      evidence_results:
        (report["evidence_results"] || report[:evidence_results] || [])
        |> Enum.map(fn er ->
          %{
            verdict: to_string(er["verdict"] || er[:verdict]),
            details: er["details"] || er[:details],
            spec_type: er["spec_type"] || er[:spec_type]
          }
        end),
      layer_results:
        (report["layer_results"] || report[:layer_results] || [])
        |> Enum.map(fn lr ->
          %{
            layer: lr["layer"] || lr[:layer],
            layer_number: lr["layer_number"] || lr[:layer_number],
            evidence_result: %{
              verdict: to_string(get_nested(lr, "evidence_result", "verdict")),
              details: get_nested(lr, "evidence_result", "details")
            }
          }
        end)
    }
  end

  defp normalize_claim(claim) when is_map(claim) do
    %{
      id: claim[:id] || claim["id"],
      description: claim[:description] || claim["description"],
      status: to_string(claim[:status] || claim["status"]),
      source: claim[:source] || claim["source"]
    }
  end

  defp normalize_slm_result(result) when is_map(result) do
    %{
      verdict: to_string(result["verdict"] || result[:verdict]),
      confidence: result["confidence"] || result[:confidence],
      reasoning: result["reasoning"] || result[:reasoning],
      votes:
        (result["votes"] || result[:votes] || [])
        |> Enum.map(fn v ->
          %{
            model: v["model"] || v[:model],
            decision: to_string(v["decision"] || v[:decision]),
            confidence: v["confidence"] || v[:confidence],
            reasoning: v["reasoning"] || v[:reasoning]
          }
        end)
    }
  end

  @valid_statuses %{
    "pending" => :pending,
    "running" => :running,
    "completed" => :completed,
    "failed" => :failed
  }

  defp safe_status_atom(status) when is_binary(status) do
    Map.get(@valid_statuses, status, :pending)
  end

  defp get_nested(map, key1, key2) when is_map(map) do
    atom_key1 = safe_key_to_atom(key1)
    atom_key2 = safe_key_to_atom(key2)
    inner = map[key1] || map[atom_key1] || %{}
    inner[key2] || inner[atom_key2]
  end

  # Only convert known stat keys — prevents atom exhaustion from untrusted input
  @known_keys ~w(registry slm total_claims verified_claims pending_claims
                 evaluations backends timestamp)
  defp safe_key_to_atom(key) when is_binary(key) do
    if key in @known_keys do
      String.to_existing_atom(key)
    else
      key
    end
  rescue
    ArgumentError -> key
  end

  defp safe_key_to_atom(key), do: key
end
