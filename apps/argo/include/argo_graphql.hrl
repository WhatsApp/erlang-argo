%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% @doc
%%%
%%% @end
%%% Created :  16 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
%% @oncall whatsapp_clr
-ifndef(ARGO_GRAPHQL_HRL).
-define(ARGO_GRAPHQL_HRL, 1).

-include_lib("argo/include/argo_graphql_language.hrl").

-record(argo_graphql_argument, {
    name :: argo_types:name(),
    value :: argo_graphql_value:t()
}).

-record(argo_graphql_argument_const, {
    name :: argo_types:name(),
    value :: argo_graphql_value_const:t()
}).

-record(argo_graphql_arguments, {
    arguments :: argo_index_map:t(argo_types:name(), argo_graphql_argument:t())
}).

-record(argo_graphql_arguments_const, {
    arguments :: argo_index_map:t(argo_types:name(), argo_graphql_argument_const:t())
}).

-record(argo_graphql_arguments_definition, {
    inputs :: argo_index_map:t(argo_types:name(), argo_graphql_input_value_definition:t())
}).

-record(argo_graphql_directive, {
    name :: argo_types:name(),
    arguments :: argo_graphql_arguments:t()
}).

-record(argo_graphql_directive_const, {
    name :: argo_types:name(),
    arguments :: argo_graphql_arguments_const:t()
}).

-record(argo_graphql_directive_definition, {
    name :: argo_types:name(),
    locations :: argo_index_set:t(argo_graphql_directive_definition:directive_location()),
    description :: none | {some, unicode:unicode_binary()},
    arguments :: argo_graphql_arguments_definition:t(),
    repeatable :: boolean()
}).

-record(argo_graphql_directives, {
    directives :: [argo_graphql_directive:t()]
}).

-record(argo_graphql_directives_const, {
    directives :: [argo_graphql_directive_const:t()]
}).

-record(argo_graphql_enum_type_definition, {
    values :: argo_index_map:t(argo_types:name(), argo_graphql_enum_value_definition:t())
}).

-record(argo_graphql_enum_value_definition, {
    value :: argo_types:name(),
    description :: none | {some, unicode:unicode_binary()},
    directives :: argo_graphql_directives_const:t()
}).

-record(argo_graphql_executable_document, {
    operation_definitions ::
        none
        | {single, argo_graphql_operation_definition:t()}
        | {multiple, #{argo_types:name() => argo_graphql_operation_definition:t()}},
    fragment_definitions :: #{argo_types:name() => argo_graphql_fragment_definition:t()}
}).

-record(argo_graphql_field, {
    name :: argo_types:name(),
    'alias' :: none | {some, argo_types:name()},
    arguments :: argo_graphql_arguments:t(),
    directives :: argo_graphql_directives:t(),
    selection_set :: argo_graphql_selection_set:t()
}).

-record(argo_graphql_field_definition, {
    name :: argo_types:name(),
    type :: argo_graphql_type:t(),
    arguments :: argo_graphql_arguments_definition:t(),
    description :: none | {some, unicode:unicode_binary()},
    directives :: argo_graphql_directives_const:t()
}).

-record(argo_graphql_fragment_definition, {
    name :: argo_types:name(),
    type_condition :: argo_types:name(),
    selection_set :: argo_graphql_selection_set:t(),
    directives :: argo_graphql_directives:t()
}).

-record(argo_graphql_fragment_spread, {
    name :: argo_types:name(),
    directives :: argo_graphql_directives:t()
}).

-record(argo_graphql_inline_fragment, {
    selection_set :: argo_graphql_selection_set:t(),
    type_condition :: none | {some, argo_types:name()},
    directives :: argo_graphql_directives:t()
}).

-record(argo_graphql_input_object_type_definition, {
    inputs :: argo_index_map:t(argo_types:name(), argo_graphql_input_value_definition:t())
}).

-record(argo_graphql_input_type_graph, {
    inputs :: argo_graphql_input_type_graph:inputs()
}).

-record(argo_graphql_input_value_definition, {
    name :: argo_types:name(),
    type :: argo_graphql_type:t(),
    description :: none | {some, unicode:unicode_binary()},
    directives :: argo_graphql_directives_const:t(),
    default_value :: none | {some, argo_graphql_value_const:t()}
}).

-record(argo_graphql_interface_type_definition, {
    implements :: argo_index_set:t(argo_types:name()),
    fields :: argo_index_map:t(argo_types:name(), argo_graphql_field_definition:t())
}).

-record(argo_graphql_list_type, {
    type :: argo_graphql_type:t()
}).

-record(argo_graphql_non_null_type, {
    type :: argo_types:name() | argo_graphql_list_type:t()
}).

-record(argo_graphql_object_type_definition, {
    implements :: argo_index_set:t(argo_types:name()),
    fields :: argo_index_map:t(argo_types:name(), argo_graphql_field_definition:t())
}).

-record(argo_graphql_operation_definition, {
    operation :: argo_graphql_operation_definition:operation_type(),
    name :: none | {some, argo_types:name()},
    variables_definition :: argo_graphql_variables_definition:t(),
    directives :: argo_graphql_directives:t(),
    selection_set :: argo_graphql_selection_set:t(),
    shorthand :: boolean()
}).

-record(argo_graphql_scalar_type_definition, {}).

-record(argo_graphql_selection_set, {
    selections :: [argo_graphql_selection_set:selection()]
}).

-record(argo_graphql_service_document, {
    description :: none | {some, unicode:unicode_binary()},
    directives :: argo_graphql_directives_const:t(),
    schema_defined :: boolean(),
    'query' :: none | {some, argo_types:name()},
    mutation :: none | {some, argo_types:name()},
    subscription :: none | {some, argo_types:name()},
    directive_definitions :: #{argo_types:name() => argo_graphql_directive_definition:t()},
    type_definitions :: #{argo_types:name() => argo_graphql_type_definition:t()}
}).

-record(argo_graphql_type, {
    inner :: argo_graphql_type:inner()
}).

-record(argo_graphql_type_definition, {
    name :: argo_types:name(),
    description :: none | {some, unicode:unicode_binary()},
    directives :: argo_graphql_directives_const:t(),
    kind :: argo_graphql_type_definition:kind()
}).

-record(argo_graphql_union_type_definition, {
    types :: argo_index_set:t(argo_types:name())
}).

-record(argo_graphql_value, {
    inner :: argo_graphql_value:inner()
}).

-record(argo_graphql_value_const, {
    inner :: argo_graphql_value_const:inner()
}).

-record(argo_graphql_variable_definition, {
    variable :: argo_types:name(),
    type :: argo_graphql_type:t(),
    default_value :: none | {some, argo_graphql_value_const:t()},
    directives :: argo_graphql_directives_const:t()
}).

-record(argo_graphql_variables_definition, {
    variables :: argo_index_map:t(argo_types:name(), argo_graphql_variable_definition:t())
}).

-endif.
