# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

defmodule Argo.MixProject do
  use Mix.Project

  def project() do
    {app, desc} = load_app()
    [
      app: app,
      version: to_string(Keyword.fetch!(desc, :vsn)),
      description: to_string(Keyword.fetch!(desc, :description)),
      elixir: "~> 1.15",
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package(),
      compilers: [:leex, :yecc] ++ Mix.compilers()
    ]
  end

  def application() do
    []
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps() do
    []
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp package() do
    {_app, desc} = load_app()
    [
      build_tools: Enum.map(Keyword.fetch!(desc, :build_tools), &to_string/1),
      description: to_string(Keyword.fetch!(desc, :description)),
      exclude_patterns: Enum.map(Keyword.fetch!(desc, :exclude_patterns), fn pattern ->
        Regex.compile!(to_string(pattern))
      end),
      files: Enum.map(Keyword.fetch!(desc, :files), &to_string/1),
      licenses: Enum.map(Keyword.fetch!(desc, :licenses), &to_string/1),
      links: Enum.into(Keyword.fetch!(desc, :links), Map.new(), fn {key, value} ->
        {to_string(key), to_string(value)}
      end),
      name: to_string(Keyword.fetch!(desc, :pkg_name))
    ]
  end

  defp load_app() do
    {:ok, [{:application, name, desc}]} = :file.consult(~c"src/argo.app.src")
    {name, desc}
  end
end
