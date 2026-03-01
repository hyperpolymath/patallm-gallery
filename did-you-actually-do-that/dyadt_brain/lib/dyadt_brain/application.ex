# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.Application do
  @moduledoc """
  OTP Application for the DYADT Brain.

  Supervision tree:

      DyadtBrain.Supervisor (one_for_one)
      ├── DyadtBrain.ClaimRegistry           — ETS-backed claim tracking
      ├── DyadtBrain.VerdictEngine            — Aggregates layer results
      ├── DyadtBrain.Ports.RustPort           — Port to Rust verifier binary
      ├── DyadtBrain.SurveillanceSupervisor   — Tier 3 consensus arbiter
      ├── DyadtBrain.VerificationManager      — Orchestrates 12-layer pipeline
      ├── DyadtBrain.PatternLearner           — Layer 12: violation pattern tracking
      ├── DyadtBrain.RegressionBaseline       — Layer 10: baseline storage/comparison
      ├── Bandit                              — HTTP server (REST + GraphQL)
      └── DyadtBrain.GRPC.Endpoint           — TCP gRPC server
  """

  use Application

  @impl true
  def start(_type, _args) do
    port = Application.get_env(:dyadt_brain, :api_port, 4200)

    children = [
      DyadtBrain.ClaimRegistry,
      DyadtBrain.VerdictEngine,
      DyadtBrain.Ports.RustPort,
      DyadtBrain.SurveillanceSupervisor,
      DyadtBrain.VerificationManager,
      DyadtBrain.PatternLearner,
      DyadtBrain.RegressionBaseline,
      {Bandit, plug: DyadtBrain.API.Router, port: port},
      DyadtBrain.GRPC.Endpoint
    ]

    opts = [strategy: :one_for_one, name: DyadtBrain.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
