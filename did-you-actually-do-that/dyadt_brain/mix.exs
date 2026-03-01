# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.MixProject do
  use Mix.Project

  def project do
    [
      app: :dyadt_brain,
      version: "0.1.0",
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),
      description: "Elixir brain for did-you-actually-do-that verification platform",
      package: package()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {DyadtBrain.Application, []}
    ]
  end

  defp deps do
    [
      {:jason, "~> 1.4"},
      {:plug, "~> 1.16"},
      {:bandit, "~> 1.6"},
      {:req, "~> 0.5"},
      # GraphQL
      {:absinthe, "~> 1.7"},
      {:absinthe_plug, "~> 1.5"}
    ]
  end

  defp aliases do
    [
      test: "test --trace"
    ]
  end

  defp package do
    [
      licenses: ["PMPL-1.0-or-later"],
      links: %{
        "GitLab" => "https://gitlab.com/hyperpolymath/did-you-actually-do-that",
        "GitHub" => "https://github.com/hyperpolymath/did-you-actually-do-that"
      }
    ]
  end
end
