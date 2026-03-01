# DyadtBrain

Elixir/OTP application providing the distributed brain for
[DYADT](../README.adoc) (Did You Actually Do That?). Handles orchestration,
pattern learning, regression baselines, and the triple API surface
(REST, GraphQL, gRPC) that drives consensus-based LLM output verification.

## Components

| Module | Layer | Purpose |
|--------|-------|---------|
| `SurveillanceSupervisor` | — | OTP supervisor coordinating verification workers |
| `ClaimRegistry` | — | ETS-backed registry of active verification claims |
| `VerificationManager` | — | Dispatches claims through the 12-layer pipeline |
| `VerdictEngine` | — | Aggregates per-layer verdicts into final GO/NO-GO |
| `PatternLearner` | 12 | Records violation patterns and learns recurring signatures |
| `RegressionBaseline` | 10 | Tracks test baselines and detects regressions |
| `TestRunner` | 5/10 | Detects frameworks and runs test suites |
| `Ports.RustPort` | — | Erlang Port bridge to the Rust SLM ensemble |
| `GRPC.Endpoint` | — | gRPC service endpoint (port 4201) |
| `VerisimDB.Client` | — | Optional persistence via VerisimDB |

## Installation

Add `dyadt_brain` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:dyadt_brain, "~> 0.1.0"}
  ]
end
```

