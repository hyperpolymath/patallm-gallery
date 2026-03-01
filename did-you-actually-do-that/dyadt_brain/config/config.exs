# SPDX-License-Identifier: PMPL-1.0-or-later
import Config

config :dyadt_brain,
  rust_binary: System.get_env("DYADT_BINARY") || "dyadt",
  api_port: 4200,
  grpc_port: 4201

import_config "#{config_env()}.exs"
