# `argo`

`argo` for GraphQL.

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
