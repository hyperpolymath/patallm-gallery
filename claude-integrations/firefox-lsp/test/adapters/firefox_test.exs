# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule PolyLSP.Adapters.FirefoxTest do
  use ExUnit.Case
  alias PolyLSP.Adapters.Firefox

  describe "detect/1" do
    test "returns true when config exists" do
      assert {:ok, true} = Firefox.detect(".")
    end
  end

  describe "version/0" do
    test "returns version string" do
      case Firefox.version() do
        {:ok, version} -> assert is_binary(version)
        {:error, _} -> :ok  # CLI not installed
      end
    end
  end

  describe "metadata/0" do
    test "returns valid metadata" do
      meta = Firefox.metadata()
      assert is_map(meta)
      assert Map.has_key?(meta, :name)
    end
  end
end
