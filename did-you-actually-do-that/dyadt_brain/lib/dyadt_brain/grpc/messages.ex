# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.GRPC.Messages do
  @moduledoc """
  Protobuf-compatible message definitions for the DYADT gRPC service.

  Messages are encoded as length-delimited JSON over TCP for cross-language
  compatibility. The wire format is:

      [4-byte big-endian length][JSON payload]

  This matches the Erlang Port protocol used by the Rust verifier, allowing
  the same encoding/decoding logic on both sides.

  ## Service Definition (proto3-equivalent)

      service VerificationService {
        rpc VerifyClaim (VerifyClaimRequest) returns (VerificationReport);
        rpc VerifyBatch (VerifyBatchRequest) returns (VerifyBatchResponse);
        rpc GetReport (GetReportRequest) returns (VerificationReport);
        rpc EvaluateSlm (EvaluateSlmRequest) returns (SlmResult);
        rpc Health (HealthRequest) returns (HealthResponse);
        rpc GetStats (StatsRequest) returns (StatsResponse);
      }
  """

  defmodule VerifyClaimRequest do
    @moduledoc false
    defstruct [:description, :evidence, :source]
  end

  defmodule VerifyBatchRequest do
    @moduledoc false
    defstruct [:claims]
  end

  defmodule GetReportRequest do
    @moduledoc false
    defstruct [:claim_id]
  end

  defmodule EvaluateSlmRequest do
    @moduledoc false
    defstruct [:description, :evidence_summary, :layer_results, :violation_signals]
  end

  defmodule HealthRequest do
    @moduledoc false
    defstruct []
  end

  defmodule StatsRequest do
    @moduledoc false
    defstruct []
  end

  @doc "Encode a message to the wire format (4-byte length prefix + JSON)."
  def encode(message) do
    json = Jason.encode!(message)
    length = byte_size(json)
    <<length::big-unsigned-32, json::binary>>
  end

  @doc "Decode a message from the wire format."
  def decode(<<length::big-unsigned-32, json::binary-size(length)>>) do
    Jason.decode(json)
  end

  def decode(<<length::big-unsigned-32, json::binary>>) when byte_size(json) >= length do
    <<payload::binary-size(length), _rest::binary>> = json
    Jason.decode(payload)
  end

  def decode(_), do: {:error, :incomplete_message}

  @doc "Parse an RPC method name from a request envelope."
  def parse_request(data) do
    case decode(data) do
      {:ok, %{"method" => method, "params" => params}} ->
        {:ok, method, params}

      {:ok, %{"method" => method}} ->
        {:ok, method, %{}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc "Build a response envelope."
  def build_response(result, error \\ nil) do
    response =
      if error do
        %{"ok" => false, "error" => to_string(error)}
      else
        %{"ok" => true, "result" => result}
      end

    encode(response)
  end
end
