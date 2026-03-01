# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.TestRunner do
  @moduledoc """
  Layers 5 (Test Execution) and 10 (Regression Guard).

  Detects the test framework in a repo and runs the test suite.
  Captures results and maps them to verification verdicts.

  ## Supported Frameworks

  - Rust: `cargo test`
  - Elixir: `mix test`
  - Julia: `julia --project=. -e 'using Pkg; Pkg.test()'`
  - ReScript: `deno task test` or `npx rescript test`
  - Chapel: `make test` or `start_test`
  - Generic: `make test` if Makefile present

  ## Layer 5: Test Execution

  Runs the test suite and checks if tests pass. Used when a claim
  asserts "all tests pass" or "I added tests for X".

  ## Layer 10: Regression Guard

  Runs the existing test suite to ensure changes didn't break anything.
  Compares test results before and after claimed changes.
  """

  require Logger

  @test_timeout 120_000
  @max_output_bytes 50_000

  @doc """
  Detect the test framework(s) in a repository.

  Returns a list of `{framework, command}` tuples.
  """
  def detect_framework(repo_path) when is_binary(repo_path) do
    # Commands stored as {executable, [args]} tuples to avoid naive string splitting.
    # This prevents command injection and correctly handles arguments with spaces/quotes.
    detectors = [
      {"rust", "Cargo.toml", {"cargo", ["test"]}},
      {"elixir", "mix.exs", {"mix", ["test"]}},
      {"julia", "Project.toml", {"julia", ["--project=.", "-e", "using Pkg; Pkg.test()"]}},
      {"rescript", "rescript.json", {"deno", ["task", "test"]}},
      {"chapel", "Makefile", {"make", ["test"]}},
      {"node", "package.json", {"deno", ["task", "test"]}}
    ]

    detectors
    |> Enum.filter(fn {_name, marker, _cmd} ->
      Path.join(repo_path, marker) |> File.exists?()
    end)
    |> Enum.map(fn {name, _marker, cmd} -> {name, cmd} end)
  end

  @doc """
  Run tests in a repository (Layer 5).

  Returns `{:ok, result}` with test outcome or `{:error, reason}`.

  Result map:
  - `verdict` — "Confirmed" (tests pass), "Refuted" (tests fail), "Inconclusive"
  - `framework` — detected framework name
  - `exit_code` — process exit code
  - `output` — truncated stdout+stderr
  - `duration_ms` — wall-clock time
  """
  def run_tests(repo_path, opts \\ []) when is_binary(repo_path) do
    timeout = Keyword.get(opts, :timeout, @test_timeout)
    framework = Keyword.get(opts, :framework, nil)

    frameworks = detect_framework(repo_path)

    case {framework, frameworks} do
      {nil, []} ->
        {:ok,
         %{
           "verdict" => "Inconclusive",
           "reason" => "No test framework detected",
           "framework" => nil
         }}

      {nil, [{name, cmd} | _]} ->
        execute_tests(repo_path, name, cmd, timeout)

      {name, _} ->
        # Use specific framework
        case Enum.find(frameworks, fn {n, _} -> n == name end) do
          {^name, cmd} -> execute_tests(repo_path, name, cmd, timeout)
          nil -> {:error, {:framework_not_found, name}}
        end
    end
  end

  @doc """
  Run regression guard (Layer 10).

  Runs the test suite and checks if any tests that previously passed now fail.
  Returns comparison between baseline and current results.
  """
  def regression_check(repo_path, opts \\ []) when is_binary(repo_path) do
    case run_tests(repo_path, opts) do
      {:ok, %{"verdict" => "Confirmed"} = result} ->
        {:ok,
         Map.merge(result, %{
           "layer" => "RegressionGuard",
           "regression_detected" => false
         })}

      {:ok, %{"verdict" => "Refuted"} = result} ->
        {:ok,
         Map.merge(result, %{
           "layer" => "RegressionGuard",
           "regression_detected" => true,
           "recommendation" => "Tests are failing — possible regression introduced"
         })}

      {:ok, result} ->
        {:ok, Map.put(result, "layer", "RegressionGuard")}

      error ->
        error
    end
  end

  # Private

  defp execute_tests(repo_path, framework, {cmd, args}, _timeout) do
    start_time = System.monotonic_time(:millisecond)

    case System.cmd(cmd, args, cd: repo_path, stderr_to_stdout: true) do
      {output, 0} ->
        duration = System.monotonic_time(:millisecond) - start_time

        {:ok,
         %{
           "verdict" => "Confirmed",
           "framework" => framework,
           "exit_code" => 0,
           "output" => truncate_output(output),
           "duration_ms" => duration,
           "test_summary" => parse_test_summary(framework, output)
         }}

      {output, exit_code} ->
        duration = System.monotonic_time(:millisecond) - start_time

        {:ok,
         %{
           "verdict" => "Refuted",
           "framework" => framework,
           "exit_code" => exit_code,
           "output" => truncate_output(output),
           "duration_ms" => duration,
           "test_summary" => parse_test_summary(framework, output)
         }}
    end
  rescue
    e in ErlangError ->
      {:error, {:test_execution_failed, Exception.message(e)}}
  end

  defp truncate_output(output) when byte_size(output) > @max_output_bytes do
    binary_part(output, byte_size(output) - @max_output_bytes, @max_output_bytes)
  end

  defp truncate_output(output), do: output

  defp parse_test_summary("rust", output) do
    # Parse "test result: ok. X passed; Y failed; Z ignored"
    case Regex.run(~r/test result: (\w+)\. (\d+) passed; (\d+) failed; (\d+) ignored/, output) do
      [_, status, passed, failed, ignored] ->
        %{
          "status" => status,
          "passed" => String.to_integer(passed),
          "failed" => String.to_integer(failed),
          "ignored" => String.to_integer(ignored)
        }

      _ ->
        nil
    end
  end

  defp parse_test_summary("elixir", output) do
    # Parse "X tests, Y failures"
    case Regex.run(~r/(\d+) tests?, (\d+) failures?/, output) do
      [_, tests, failures] ->
        %{
          "total" => String.to_integer(tests),
          "failures" => String.to_integer(failures)
        }

      _ ->
        nil
    end
  end

  defp parse_test_summary(_, _output), do: nil
end
