# `argo`

[![Build Status](https://github.com/WhatsApp/erlang-argo/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/WhatsApp/erlang-argo/actions) [![Hex.pm](https://img.shields.io/hexpm/v/argo_graphql.svg)](https://hex.pm/packages/argo_graphql)

`argo` is a compact binary serialization format for [GraphQL](https://spec.graphql.org/).

This library provides support for [Erlang](https://www.erlang.org/) and [Elixir](https://elixir-lang.org/) following the [Argo 1.2.0 specifications](https://msolomon.github.io/argo/versions/1.2/spec#sec-v1-2-0).

See the [CONTRIBUTING](CONTRIBUTING.md) file for how to help out.

## Installation

Add `argo_graphql` to your project's dependencies in `mix.exs`

```elixir
defp deps() do
  [
    {:argo, "~> 1.0", hex: :argo_graphql}
  ]
end
```

Add `argo_graphql` to your project's dependencies in your `Makefile` for [`erlang.mk`](https://github.com/ninenines/erlang.mk) or the following to your `rebar.config`

```erlang
{deps, [
    {argo, "~> 1.0", {pkg, argo_graphql}}
]}.
```

## License

`argo` is MIT licensed, as found in the [LICENSE](LICENSE.md) file.
