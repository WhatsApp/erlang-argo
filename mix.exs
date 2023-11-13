# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

defmodule ArgoElixirTests.MixProject do
  use Mix.Project

  def project() do
    [
      app: :argo_elixir_tests,
      version: "1.0.0",
      elixir: "~> 1.14",
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  def application() do
    [extra_applications: [:logger, :observer, :runtime_tools, :argo]]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps() do
    [
      {:argo, path: "apps/argo", only: :test, override: true},
      {:argo_test, path: "apps/argo_test", only: :test, override: true}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end
