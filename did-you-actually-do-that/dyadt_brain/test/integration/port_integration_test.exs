# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.Integration.PortIntegrationTest do
  @moduledoc """
  Integration tests for Rust verifier Port communication.

  These tests require the `dyadt` binary to be built.
  They are skipped if the binary is not found.
  """
  use ExUnit.Case

  alias DyadtBrain.Ports.RustPort

  @binary_path Path.expand("../../../target/release/dyadt", __DIR__)

  setup_all do
    if File.exists?(@binary_path) do
      # Wait for port to be ready
      Process.sleep(100)
      :ok
    else
      {:skip, "Rust binary not built. Run: cargo build --release"}
    end
  end

  test "ping gets pong response" do
    result = RustPort.ping()
    assert {:ok, %{"pong" => true}} = result
  end

  test "verify_claim with FileExists for real file" do
    claim = %{
      "description" => "Cargo.toml exists",
      "evidence" => [
        %{
          "type" => "FileExists",
          "spec" => %{
            "path" => Path.expand("../../../Cargo.toml", __DIR__)
          }
        }
      ]
    }

    {:ok, enriched} = RustPort.verify_claim(claim)
    report = enriched["report"]
    assert report["overall_verdict"] == "Confirmed"
  end

  test "verify_claim with FileExists for nonexistent file" do
    claim = %{
      "description" => "Nonexistent file",
      "evidence" => [
        %{
          "type" => "FileExists",
          "spec" => %{
            "path" => "/tmp/dyadt-test-nonexistent-abc123.txt"
          }
        }
      ]
    }

    {:ok, enriched} = RustPort.verify_claim(claim)
    report = enriched["report"]
    assert report["overall_verdict"] == "Refuted"
  end

  test "verify_claim returns layer results" do
    claim = %{
      "description" => "Check Cargo.toml",
      "evidence" => [
        %{
          "type" => "FileExists",
          "spec" => %{
            "path" => Path.expand("../../../Cargo.toml", __DIR__)
          }
        }
      ]
    }

    {:ok, enriched} = RustPort.verify_claim(claim)
    layer_results = enriched["layer_results"]
    assert is_list(layer_results)
    assert length(layer_results) > 0

    first = hd(layer_results)
    assert first["layer"] == "FileExistence"
  end

  test "check_evidence for a single evidence spec" do
    evidence = %{
      "type" => "DirectoryExists",
      "spec" => %{
        "path" => Path.expand("../../..", __DIR__)
      }
    }

    {:ok, result} = RustPort.check_evidence(evidence)
    assert result["evidence_result"]["verdict"] == "Confirmed"
    assert result["layer"] == "FileExistence"
  end

  test "extract_claims from text" do
    text = "I created the file /tmp/test.txt and committed changes to git"

    {:ok, claims} = RustPort.extract_claims(text)
    assert is_list(claims)
    # The extractor should find at least one claim
    assert length(claims) >= 1
  end
end
