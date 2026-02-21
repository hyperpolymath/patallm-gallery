# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

defmodule ClaudeFirefox.Adapters.Supervisor do
  @moduledoc """
  Supervisor for browser adapter processes.

  Manages the lifecycle of browser adapter GenServers, ensuring they are
  restarted on failure with a :one_for_one strategy.
  """

  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    children = [
      {ClaudeFirefox.Adapters.Firefox, []}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
