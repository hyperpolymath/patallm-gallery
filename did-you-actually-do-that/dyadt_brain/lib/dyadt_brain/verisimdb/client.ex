# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule DyadtBrain.VeriSimDB.Client do
  @moduledoc """
  HTTP client for VeriSimDB's REST API (Axum-based).

  VeriSimDB is the 6-modal database backing DYADT's verification data.
  This module handles all HTTP communication with the VeriSimDB API server.

  ## Configuration

      config :dyadt_brain, :verisimdb,
        base_url: "http://localhost:8080",
        api_key: nil,
        timeout: 10_000
  """

  require Logger

  @default_base_url "http://localhost:8080"
  @default_timeout 10_000

  defp config do
    Application.get_env(:dyadt_brain, :verisimdb, [])
  end

  defp base_url, do: Keyword.get(config(), :base_url, @default_base_url)
  defp timeout, do: Keyword.get(config(), :timeout, @default_timeout)

  defp headers do
    base = [{"content-type", "application/json"}, {"accept", "application/json"}]

    case Keyword.get(config(), :api_key) do
      nil -> base
      key -> [{"authorization", "Bearer #{key}"} | base]
    end
  end

  @doc "Check if VeriSimDB is reachable."
  def health_check do
    case Req.get("#{base_url()}/health", headers: headers(), receive_timeout: timeout()) do
      {:ok, %{status: 200, body: body}} -> {:ok, body}
      {:ok, %{status: status}} -> {:error, {:http_error, status}}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc "Store a document in a collection."
  def store(collection, id, document) when is_binary(collection) and is_map(document) do
    url = "#{base_url()}/api/v1/collections/#{collection}/documents"
    body = Map.put(document, "id", id)

    case Req.post(url, json: body, headers: headers(), receive_timeout: timeout()) do
      {:ok, %{status: status, body: body}} when status in 200..201 -> {:ok, body}
      {:ok, %{status: status, body: body}} -> {:error, {:http_error, status, body}}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc "Retrieve a document by ID."
  def get(collection, id) when is_binary(collection) and is_binary(id) do
    url = "#{base_url()}/api/v1/collections/#{collection}/documents/#{id}"

    case Req.get(url, headers: headers(), receive_timeout: timeout()) do
      {:ok, %{status: 200, body: body}} -> {:ok, body}
      {:ok, %{status: 404}} -> {:error, :not_found}
      {:ok, %{status: status, body: body}} -> {:error, {:http_error, status, body}}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc "Query documents in a collection."
  def query(collection, query_params) when is_binary(collection) and is_map(query_params) do
    url = "#{base_url()}/api/v1/collections/#{collection}/query"

    case Req.post(url, json: query_params, headers: headers(), receive_timeout: timeout()) do
      {:ok, %{status: 200, body: body}} -> {:ok, body}
      {:ok, %{status: status, body: body}} -> {:error, {:http_error, status, body}}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc "Delete a document by ID."
  def delete(collection, id) when is_binary(collection) and is_binary(id) do
    url = "#{base_url()}/api/v1/collections/#{collection}/documents/#{id}"

    case Req.delete(url, headers: headers(), receive_timeout: timeout()) do
      {:ok, %{status: status}} when status in [200, 204] -> :ok
      {:ok, %{status: 404}} -> {:error, :not_found}
      {:ok, %{status: status, body: body}} -> {:error, {:http_error, status, body}}
      {:error, reason} -> {:error, reason}
    end
  end
end
