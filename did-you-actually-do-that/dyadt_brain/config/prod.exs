# SPDX-License-Identifier: PMPL-1.0-or-later
import Config

config :logger, level: :info

# Container-friendly configuration — read from environment at startup
config :dyadt_brain,
  rust_binary: System.get_env("DYADT_BINARY") || "/app/bin/dyadt",
  api_port: String.to_integer(System.get_env("DYADT_API_PORT") || "4200")

# VeriSimDB connection (for containerized deployment)
config :dyadt_brain, :verisimdb,
  base_url: System.get_env("DYADT_VERISIMDB_URL") || "http://localhost:8080",
  api_key: System.get_env("DYADT_VERISIMDB_API_KEY"),
  timeout: 10_000
