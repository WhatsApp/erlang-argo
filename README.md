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
    {:argo, "~> 1.1", hex: :argo_graphql}
  ]
end
```

Add `argo_graphql` to your project's dependencies in your `Makefile` for [`erlang.mk`](https://github.com/ninenines/erlang.mk) or the following to your `rebar.config`

```erlang
{deps, [
    {argo, "~> 1.1", {pkg, argo_graphql}}
]}.
```

## Usage

Using the ["Introduction to GraphQL" example](https://graphql.org/learn/):

```erlang
% Load the GraphQL Service Document.
SD = argo_graphql_service_document:from_string("
  type Query {
    me: User
  }

  type User {
    id: ID
    name: String
  }
"),
% Load the GraphQL Executable Document.
ED = argo_graphql_executable_document:from_string("
  query MyQuery {
    me {
      name
    }
  }
"),
% Derive the Argo Wire Type based on the GraphQL Service Document
% and the GraphQL Executable Document.
{{some, <<"MyQuery">>}, ArgoWireType} = argo_typer:derive_wire_type(SD, ED, none).

% Convert a JSON response to an Argo Value.
JsonValue = #{<<"data">> => #{<<"me">> => #{<<"name">> => <<"Luke Skywalker">>}}},
ArgoValue = argo_value:from_json(ArgoWireType, JsonValue),
% Encode Argo Value using default settings.
ArgoEncoded = argo_value:to_writer(ArgoValue),
% Compare output size to the JSON encoding.
JsonEncoded = argo_json:encode(JsonValue),
41 = byte_size(JsonEncoded),
22 = byte_size(ArgoEncoded),
% Argo encoding is roughly 46% smaller than JSON encoding in this case.
46 = trunc((1 - (byte_size(ArgoEncoded) / byte_size(JsonEncoded))) * 100).

% For decoding, use the Argo Wire Type and the Argo encoding bytes.
{<<>>, ArgoValue} = argo_value:from_reader(ArgoWireType, ArgoEncoded).

% Optionally convert back to JSON representation.
JsonResponse = argo_value:to_json(ArgoValue).
```

Use `argo:display/1` or `argo:display_with_lines/1` to inspect Argo Wire Types, Argo Values, and GraphQL related data structures:

```erlang
argo:display(ArgoWireType).
% {
%   data: {
%     me: {
%       name: STRING<String>?
%     }?
%   }?
%   errors?: ERROR[]
%   extensions?: EXTENSIONS
% }

argo:display(ArgoValue).
% {
%   data: NON_NULL({
%     me: NON_NULL({
%       name: NON_NULL(STRING(<<"Luke Skywalker">>)<String>)
%     })
%   })
%   errors?: ABSENT
%   extensions?: ABSENT
% }
```

## License

`argo` is MIT licensed, as found in the [LICENSE](LICENSE.md) file.
