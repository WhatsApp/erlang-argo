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
-ifndef(ARGO_GRAPHQL_LANGUAGE_HRL).
-define(ARGO_GRAPHQL_LANGUAGE_HRL, 1).

-record(argo_graphql_language_arguments, {
    location :: erl_anno:location(),
    arguments :: [argo_graphql_language_argument:t()]
}).
-record(argo_graphql_language_argument, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    value :: argo_graphql_language_value:t()
}).

-record(argo_graphql_language_arguments_const, {
    location :: erl_anno:location(),
    arguments :: [argo_graphql_language_argument_const:t()]
}).
-record(argo_graphql_language_argument_const, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    value :: argo_graphql_language_value_const:t()
}).

-record(argo_graphql_language_arguments_definition, {
    location :: erl_anno:location(),
    inputs :: [argo_graphql_language_input_value_definition:t()]
}).
-record(argo_graphql_language_input_value_definition, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    type :: argo_graphql_language_type:t(),
    description :: none | {some, unicode:unicode_binary()},
    directives :: none | {some, argo_graphql_language_directives_const:t()},
    default_value :: none | {some, argo_graphql_language_value_const:t()}
}).

-record(argo_graphql_language_definition, {
    location :: erl_anno:location(),
    inner :: argo_graphql_language_definition:inner()
}).

-record(argo_graphql_language_directive_definition, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    locations :: [argo_graphql_language_directive_location:t()],
    description :: none | {some, unicode:unicode_binary()},
    arguments :: none | {some, argo_graphql_language_arguments_definition:t()},
    repeatable :: boolean()
}).
-record(argo_graphql_language_directive_location, {
    location :: erl_anno:location(),
    inner :: argo_graphql_language_directive_location:inner()
}).
-record(argo_graphql_language_executable_directive_location, {
    location :: erl_anno:location(),
    name :: argo_graphql_language_executable_directive_location:name()
}).
-record(argo_graphql_language_type_system_directive_location, {
    location :: erl_anno:location(),
    name :: argo_graphql_language_type_system_directive_location:name()
}).

-record(argo_graphql_language_directives_const, {
    location :: erl_anno:location(),
    directives :: [argo_graphql_language_directive_const:t()]
}).
-record(argo_graphql_language_directive_const, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    arguments :: none | {some, argo_graphql_language_arguments_const:t()}
}).

-record(argo_graphql_language_directives, {
    location :: erl_anno:location(),
    directives :: [argo_graphql_language_directive:t()]
}).
-record(argo_graphql_language_directive, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    arguments :: none | {some, argo_graphql_language_arguments:t()}
}).

-record(argo_graphql_language_document, {
    location :: erl_anno:location(),
    definitions :: [argo_graphql_language_definition:t()]
}).

-record(argo_graphql_language_enum_type_definition, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    description :: none | {some, unicode:unicode_binary()},
    directives :: none | {some, argo_graphql_language_directives_const:t()},
    values :: none | {some, argo_graphql_language_enum_values_definition:t()}
}).
-record(argo_graphql_language_enum_values_definition, {
    location :: erl_anno:location(),
    values :: [argo_graphql_language_enum_value_definition:t()]
}).
-record(argo_graphql_language_enum_value_definition, {
    location :: erl_anno:location(),
    value :: argo_graphql_language_enum_value:t(),
    description :: none | {some, unicode:unicode_binary()},
    directives :: none | {some, argo_graphql_language_directives_const:t()}
}).

-record(argo_graphql_language_enum_type_extension, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    directives :: none | {some, argo_graphql_language_directives_const:t()},
    values :: none | {some, argo_graphql_language_enum_values_definition:t()}
}).

-record(argo_graphql_language_executable_definition, {
    location :: erl_anno:location(),
    inner :: argo_graphql_language_executable_definition:inner()
}).

-record(argo_graphql_language_field, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    'alias' :: none | {some, argo_types:name()},
    arguments :: none | {some, argo_graphql_language_arguments:t()},
    directives :: none | {some, argo_graphql_language_directives:t()},
    selection_set :: none | {some, argo_graphql_language_selection_set:t()}
}).

-record(argo_graphql_language_fields_definition, {
    location :: erl_anno:location(),
    fields :: [argo_graphql_language_field_definition:t()]
}).
-record(argo_graphql_language_field_definition, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    type :: argo_graphql_language_type:t(),
    arguments :: none | {some, argo_graphql_language_arguments_definition:t()},
    description :: none | {some, unicode:unicode_binary()},
    directives :: none | {some, argo_graphql_language_directives_const:t()}
}).

-record(argo_graphql_language_fragment_definition, {
    location :: erl_anno:location(),
    name :: argo_graphql_language_fragment_name:t(),
    type_condition :: argo_graphql_language_type_condition:t(),
    selection_set :: argo_graphql_language_selection_set:t(),
    directives :: none | {some, argo_graphql_language_directives:t()}
}).
-record(argo_graphql_language_fragment_name, {
    location :: erl_anno:location(),
    name :: argo_types:name()
}).
-record(argo_graphql_language_type_condition, {
    location :: erl_anno:location(),
    type :: argo_graphql_language_named_type:t()
}).

-record(argo_graphql_language_fragment_spread, {
    location :: erl_anno:location(),
    name :: argo_graphql_language_fragment_name:t(),
    directives :: none | {some, argo_graphql_language_directives:t()}
}).

-record(argo_graphql_language_implements_interfaces, {
    location :: erl_anno:location(),
    interfaces :: [argo_graphql_language_named_type:t()]
}).

-record(argo_graphql_language_inline_fragment, {
    location :: erl_anno:location(),
    selection_set :: argo_graphql_language_selection_set:t(),
    type_condition :: none | {some, argo_graphql_language_type_condition:t()},
    directives :: none | {some, argo_graphql_language_directives:t()}
}).

-record(argo_graphql_language_input_object_type_definition, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    description :: none | {some, unicode:unicode_binary()},
    directives :: none | {some, argo_graphql_language_directives_const:t()},
    fields :: none | {some, argo_graphql_language_input_fields_definition:t()}
}).
-record(argo_graphql_language_input_fields_definition, {
    location :: erl_anno:location(),
    inputs :: [argo_graphql_language_input_value_definition:t()]
}).

-record(argo_graphql_language_input_object_type_extension, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    directives :: none | {some, argo_graphql_language_directives_const:t()},
    fields :: none | {some, argo_graphql_language_input_fields_definition:t()}
}).

-record(argo_graphql_language_interface_type_definition, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    description :: none | {some, unicode:unicode_binary()},
    implements :: none | {some, argo_graphql_language_implements_interfaces:t()},
    directives :: none | {some, argo_graphql_language_directives_const:t()},
    fields :: none | {some, argo_graphql_language_fields_definition:t()}
}).

-record(argo_graphql_language_interface_type_extension, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    implements :: none | {some, argo_graphql_language_implements_interfaces:t()},
    directives :: none | {some, argo_graphql_language_directives_const:t()},
    fields :: none | {some, argo_graphql_language_fields_definition:t()}
}).

-record(argo_graphql_language_object_type_definition, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    description :: none | {some, unicode:unicode_binary()},
    implements :: none | {some, argo_graphql_language_implements_interfaces:t()},
    directives :: none | {some, argo_graphql_language_directives_const:t()},
    fields :: none | {some, argo_graphql_language_fields_definition:t()}
}).

-record(argo_graphql_language_object_type_extension, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    implements :: none | {some, argo_graphql_language_implements_interfaces:t()},
    directives :: none | {some, argo_graphql_language_directives_const:t()},
    fields :: none | {some, argo_graphql_language_fields_definition:t()}
}).

-record(argo_graphql_language_operation_definition, {
    location :: erl_anno:location(),
    operation :: argo_graphql_language_root_operation_type_definition:operation_type(),
    name :: none | {some, argo_types:name()},
    variables_definition :: none | {some, argo_graphql_language_variables_definition:t()},
    directives :: none | {some, argo_graphql_language_directives:t()},
    selection_set :: argo_graphql_language_selection_set:t(),
    shorthand :: boolean()
}).

-record(argo_graphql_language_root_operation_types_definition, {
    location :: erl_anno:location(),
    operations :: [argo_graphql_language_root_operation_type_definition:t()]
}).
-record(argo_graphql_language_root_operation_type_definition, {
    location :: erl_anno:location(),
    operation_type :: argo_graphql_language_root_operation_type_definition:operation_type(),
    named_type :: argo_graphql_language_named_type:t()
}).

-record(argo_graphql_language_scalar_type_definition, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    description :: none | {some, unicode:unicode_binary()},
    directives :: none | {some, argo_graphql_language_directives_const:t()}
}).

-record(argo_graphql_language_scalar_type_extension, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    directives :: none | {some, argo_graphql_language_directives_const:t()}
}).

-record(argo_graphql_language_schema_definition, {
    location :: erl_anno:location(),
    description :: none | {some, unicode:unicode_binary()},
    directives :: none | {some, argo_graphql_language_directives_const:t()},
    operations :: none | {some, argo_graphql_language_root_operation_types_definition:t()}
}).

-record(argo_graphql_language_schema_extension, {
    location :: erl_anno:location(),
    directives :: none | {some, argo_graphql_language_directives_const:t()},
    operations :: none | {some, argo_graphql_language_root_operation_types_definition:t()}
}).

-record(argo_graphql_language_selection_set, {
    location :: erl_anno:location(),
    selections :: [argo_graphql_language_selection:t()]
}).
-record(argo_graphql_language_selection, {
    location :: erl_anno:location(),
    inner :: argo_graphql_language_selection:inner()
}).

-record(argo_graphql_language_type, {
    location :: erl_anno:location(),
    inner :: argo_graphql_language_type:inner()
}).
-record(argo_graphql_language_named_type, {
    location :: erl_anno:location(),
    name :: argo_types:name()
}).
-record(argo_graphql_language_list_type, {
    location :: erl_anno:location(),
    type :: argo_graphql_language_type:t()
}).
-record(argo_graphql_language_non_null_type, {
    location :: erl_anno:location(),
    type :: argo_graphql_language_named_type:t() | argo_graphql_language_list_type:t()
}).

-record(argo_graphql_language_type_definition, {
    location :: erl_anno:location(),
    inner :: argo_graphql_language_type_definition:inner()
}).

-record(argo_graphql_language_type_extension, {
    location :: erl_anno:location(),
    inner :: argo_graphql_language_type_extension:inner()
}).

-record(argo_graphql_language_type_system_definition, {
    location :: erl_anno:location(),
    inner :: argo_graphql_language_type_system_definition:inner()
}).

-record(argo_graphql_language_type_system_extension, {
    location :: erl_anno:location(),
    inner :: argo_graphql_language_type_system_extension:inner()
}).

-record(argo_graphql_language_union_type_definition, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    description :: none | {some, unicode:unicode_binary()},
    directives :: none | {some, argo_graphql_language_directives_const:t()},
    types :: none | {some, [argo_graphql_language_named_type:t()]}
}).

-record(argo_graphql_language_union_type_extension, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    directives :: none | {some, argo_graphql_language_directives_const:t()},
    types :: none | {some, [argo_graphql_language_named_type:t()]}
}).

-record(argo_graphql_language_value, {
    location :: erl_anno:location(),
    inner :: argo_graphql_language_value:inner()
}).
-record(argo_graphql_language_enum_value, {
    location :: erl_anno:location(),
    name :: argo_types:name()
}).
-record(argo_graphql_language_list_value, {
    location :: erl_anno:location(),
    list :: [argo_graphql_language_value:t()]
}).
-record(argo_graphql_language_object_field, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    value :: argo_graphql_language_value:t()
}).
-record(argo_graphql_language_object_value, {
    location :: erl_anno:location(),
    fields :: [argo_graphql_language_object_field:t()]
}).

-record(argo_graphql_language_value_const, {
    location :: erl_anno:location(),
    inner :: argo_graphql_language_value_const:inner()
}).
-record(argo_graphql_language_list_value_const, {
    location :: erl_anno:location(),
    list :: [argo_graphql_language_value_const:t()]
}).
-record(argo_graphql_language_object_field_const, {
    location :: erl_anno:location(),
    name :: argo_types:name(),
    value :: argo_graphql_language_value_const:t()
}).
-record(argo_graphql_language_object_value_const, {
    location :: erl_anno:location(),
    fields :: [argo_graphql_language_object_field_const:t()]
}).

-record(argo_graphql_language_variables_definition, {
    location :: erl_anno:location(),
    variables :: [argo_graphql_language_variable_definition:t()]
}).
-record(argo_graphql_language_variable_definition, {
    location :: erl_anno:location(),
    variable :: argo_graphql_language_variable:t(),
    type :: argo_graphql_language_type:t(),
    default_value :: none | {some, argo_graphql_language_value_const:t()},
    directives :: none | {some, argo_graphql_language_directives_const:t()}
}).
-record(argo_graphql_language_variable, {
    location :: erl_anno:location(),
    name :: argo_types:name()
}).

-endif.
