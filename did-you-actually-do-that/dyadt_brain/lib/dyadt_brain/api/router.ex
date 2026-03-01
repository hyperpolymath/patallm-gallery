# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.API.Router do
  @moduledoc """
  HTTP REST API for the DYADT Brain.

  All endpoints delegate to `DyadtBrain.Verifications` — the single business
  logic context module. GraphQL and gRPC surfaces use the same context.

  ## Endpoints

  - `POST /api/verify`       — Verify a single claim
  - `POST /api/verify/batch` — Verify multiple claims
  - `GET  /api/report/:id`   — Get verification report by claim ID
  - `GET  /api/claims`       — List all claims
  - `GET  /api/stats`        — Verification statistics
  - `GET  /api/health`       — System health check
  - `POST /api/slm/evaluate` — Evaluate through SLM ensemble
  """

  use Plug.Router

  alias DyadtBrain.Verifications

  plug :match
  plug Plug.Parsers, parsers: [:json], json_decoder: Jason
  plug :dispatch

  # GraphQL endpoint
  forward "/api/graphql",
    to: Absinthe.Plug,
    init_opts: [schema: DyadtBrain.API.Schema, json_codec: Jason]

  post "/api/verify" do
    case Verifications.verify_claim(conn.body_params) do
      {:ok, report} ->
        send_json(conn, 200, report)

      {:error, reason} ->
        send_json(conn, 422, %{error: to_string(reason)})
    end
  end

  post "/api/verify/batch" do
    claims = Map.get(conn.body_params, "claims", [])

    case Verifications.verify_batch(claims) do
      {:ok, reports} ->
        send_json(conn, 200, %{reports: reports})

      {:error, reason} ->
        send_json(conn, 422, %{error: to_string(reason)})
    end
  end

  get "/api/report/:id" do
    case Verifications.get_report(id) do
      {:ok, report} ->
        send_json(conn, 200, report)

      {:pending, status} ->
        send_json(conn, 202, %{status: status, message: "Verification in progress"})

      {:error, :not_found} ->
        send_json(conn, 404, %{error: "Claim not found"})
    end
  end

  get "/api/claims" do
    {:ok, claims} = Verifications.list_claims()
    send_json(conn, 200, %{claims: claims})
  end

  get "/api/stats" do
    {:ok, stats} = Verifications.get_stats()
    send_json(conn, 200, stats)
  end

  post "/api/slm/evaluate" do
    case Verifications.evaluate_slm(conn.body_params) do
      {:ok, result} ->
        send_json(conn, 200, result)

      {:error, reason} ->
        send_json(conn, 422, %{error: to_string(reason)})
    end
  end

  get "/api/health" do
    health = Verifications.health_check()
    send_json(conn, 200, health)
  end

  match _ do
    send_json(conn, 404, %{error: "Not found"})
  end

  defp send_json(conn, status, body) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, Jason.encode!(body))
  end
end
