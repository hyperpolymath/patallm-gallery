# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

defmodule ClaudeFirefox.LSP.Application do
  @moduledoc false
  use Application

  require Logger

  @impl true
  def start(_type, _args) do
    children = [
      # Adapter supervisor (manages browser adapter processes)
      {ClaudeFirefox.Adapters.Supervisor, []},

      # LSP server (GenLSP)
      {ClaudeFirefox.LSP.Server, []}
    ]

    opts = [strategy: :one_for_one, name: ClaudeFirefox.LSP.Supervisor]

    Logger.info("Starting Claude Firefox LSP server v#{ClaudeFirefox.LSP.version()}")

    Supervisor.start_link(children, opts)
  end
end
