Nonterminals
  Arguments ArgumentList Argument
  ArgumentsConst ArgumentConstList ArgumentConst
  ArgumentsDefinition InputValueDefinitionList InputValueDefinition
  DefinitionList Definition
  Directives DirectiveList Directive
  DirectivesConst DirectiveConstList DirectiveConst
  DefaultValue
  DescriptionDefinition
  DirectiveDefinition DirectiveLocations DirectiveLocation ExecutableDirectiveLocation TypeSystemDirectiveLocation
  Document
  EnumTypeDefinition EnumValuesDefinition EnumValueDefinitionList EnumValueDefinition
  EnumTypeExtension
  ExecutableDefinition
  Field Alias
  FieldsDefinition FieldDefinitionList FieldDefinition
  FragmentDefinition FragmentName TypeCondition
  FragmentSpread
  ImplementsInterfaces ImplementsInterfacesList
  InlineFragment
  InputObjectTypeDefinition InputFieldsDefinition
  InputObjectTypeExtension
  InterfaceTypeDefinition
  InterfaceTypeExtension
  NameWithoutOn Name
  ObjectTypeDefinition
  ObjectTypeExtension
  OperationDefinition
  OperationType
  RootOperationTypesDefinition RootOperationTypeDefinitionList RootOperationTypeDefinition
  ScalarTypeDefinition
  ScalarTypeExtension
  SchemaDefinition
  SchemaExtension
  SelectionSet SelectionList Selection
  Type NamedType ListType NonNullType
  TypeDefinition
  TypeExtension
  TypeSystemDefinition
  TypeSystemExtension
  UnionTypeDefinition UnionMemberTypes
  UnionTypeExtension
  Value EnumValue ListValue ValueList ObjectValue ObjectFieldList ObjectField
  ValueConst ListValueConst ValueConstList ObjectValueConst ObjectFieldConstList ObjectFieldConst
  VariablesDefinition VariableDefinitionList VariableDefinition Variable.

Terminals
  '{' '}' '(' ')' '[' ']' '!' ':' '@' '$' '=' '|' '&' '...'
  'query' 'mutation' 'subscription' 'fragment' 'on' 'directive' 'repeatable'
  'type' 'implements' 'interface' 'union' 'scalar' 'enum' 'input' 'extend' 'schema'
  'executable_directive_location' 'type_system_directive_location'
  name int_value float_value string_value block_string_value boolean_value null.

Rootsymbol Document.

Arguments -> '(' ArgumentList ')' : argo_graphql_language_arguments:parse(#{'arguments' => '$2'}, extract_location('$2')).
ArgumentList -> Argument : ['$1'].
ArgumentList -> Argument ArgumentList : ['$1'|'$2'].
Argument -> NameWithoutOn ':' Value : argo_graphql_language_argument:parse(#{'name' => extract_binary('$1'), 'value' => '$3'}, extract_location('$1')).
Argument -> 'on' ':' Value : argo_graphql_language_argument:parse(#{'name' => extract_binary('$1'), 'value' => '$3'}, extract_location('$1')).

ArgumentsConst -> '(' ArgumentConstList ')' : argo_graphql_language_arguments_const:parse(#{'arguments' => '$2'}, extract_location('$2')).
ArgumentConstList -> ArgumentConst : ['$1'].
ArgumentConstList -> ArgumentConst ArgumentConstList : ['$1'|'$2'].
ArgumentConst -> NameWithoutOn ':' ValueConst : argo_graphql_language_argument_const:parse(#{name => extract_binary('$1'), value => '$3'}, extract_location('$1')).
ArgumentConst -> 'on' ':' ValueConst : argo_graphql_language_argument_const:parse(#{name => extract_binary('$1'), value => '$3'}, extract_location('$1')).

ArgumentsDefinition -> '(' InputValueDefinitionList ')' : argo_graphql_language_arguments_definition:parse(#{'inputs' => '$2'}, extract_location('$1')).
InputValueDefinitionList -> InputValueDefinition : ['$1'].
InputValueDefinitionList -> InputValueDefinition InputValueDefinitionList : ['$1'|'$2'].
InputValueDefinition -> Name ':' Type : argo_graphql_language_input_value_definition:parse(#{'name' => extract_binary('$1'), 'type' => '$3'}, extract_location('$1')).
InputValueDefinition -> Name ':' Type DirectivesConst : argo_graphql_language_input_value_definition:parse(#{'name' => extract_binary('$1'), 'type' => '$3', 'directives' => '$4'}, extract_location('$1')).
InputValueDefinition -> Name ':' Type DefaultValue : argo_graphql_language_input_value_definition:parse(#{'name' => extract_binary('$1'), 'type' => '$3', 'default_value' => '$4'}, extract_location('$1')).
InputValueDefinition -> Name ':' Type DefaultValue DirectivesConst : argo_graphql_language_input_value_definition:parse(#{'name' => extract_binary('$1'), 'type' => '$3', 'default_value' => '$4', 'directives' => '$5'}, extract_location('$1')).
InputValueDefinition -> DescriptionDefinition Name ':' Type : argo_graphql_language_input_value_definition:parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'type' => '$4'}, extract_location('$2')).
InputValueDefinition -> DescriptionDefinition Name ':' Type DirectivesConst : argo_graphql_language_input_value_definition:parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'type' => '$4', 'directives' => '$5'}, extract_location('$2')).
InputValueDefinition -> DescriptionDefinition Name ':' Type DefaultValue : argo_graphql_language_input_value_definition:parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'type' => '$4', 'default_value' => '$5'}, extract_location('$2')).
InputValueDefinition -> DescriptionDefinition Name ':' Type DefaultValue DirectivesConst : argo_graphql_language_input_value_definition:parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'type' => '$4', 'default_value' => '$5', 'directives' => '$6'}, extract_location('$2')).

DefaultValue -> '=' ValueConst : '$2'.

DefinitionList -> Definition : ['$1'].
DefinitionList -> Definition DefinitionList : ['$1'|'$2'].
Definition -> ExecutableDefinition : argo_graphql_language_definition:executable_definition('$1', extract_location('$1')).
Definition -> TypeSystemDefinition : argo_graphql_language_definition:type_system_definition('$1', extract_location('$1')).
Definition -> TypeSystemExtension : argo_graphql_language_definition:type_system_extension('$1', extract_location('$1')).

DescriptionDefinition -> string_value : extract_quoted_string_token('$1').
DescriptionDefinition -> block_string_value : extract_quoted_block_string_token('$1').

Directives -> DirectiveList : argo_graphql_language_directives:parse(#{'directives' => '$1'}, extract_location('$1')).
DirectiveList -> Directive : ['$1'].
DirectiveList -> Directive DirectiveList : ['$1'|'$2'].
Directive -> '@' NameWithoutOn : argo_graphql_language_directive:parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
Directive -> '@' NameWithoutOn Arguments : argo_graphql_language_directive:parse(#{'name' => extract_binary('$2'), 'arguments' => '$3'}, extract_location('$1')).
Directive -> '@' 'on' : argo_graphql_language_directive:parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
Directive -> '@' 'on' Arguments : argo_graphql_language_directive:parse(#{'name' => extract_binary('$2'), 'arguments' => '$3'}, extract_location('$1')).

DirectivesConst -> DirectiveConstList : argo_graphql_language_directives_const:parse(#{'directives' => '$1'}, extract_location('$1')).
DirectiveConstList -> DirectiveConst : ['$1'].
DirectiveConstList -> DirectiveConst DirectiveConstList : ['$1'|'$2'].
DirectiveConst -> '@' NameWithoutOn : argo_graphql_language_directive_const:parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
DirectiveConst -> '@' NameWithoutOn ArgumentsConst : argo_graphql_language_directive_const:parse(#{'name' => extract_binary('$2'), 'arguments' => '$3'}, extract_location('$1')).
DirectiveConst -> '@' 'on' : argo_graphql_language_directive_const:parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
DirectiveConst -> '@' 'on' ArgumentsConst : argo_graphql_language_directive_const:parse(#{'name' => extract_binary('$2'), 'arguments' => '$3'}, extract_location('$1')).

DirectiveDefinition -> 'directive' '@' Name 'on' DirectiveLocations : argo_graphql_language_directive_definition:parse(#{'name' => extract_binary('$3'), 'locations' => '$5'}, extract_location('$1')).
DirectiveDefinition -> 'directive' '@' Name ArgumentsDefinition 'on' DirectiveLocations : argo_graphql_language_directive_definition:parse(#{'name' => extract_binary('$3'), 'arguments' => '$4', 'locations' => '$6'}, extract_location('$1')).
DirectiveDefinition -> 'directive' '@' Name 'repeatable' 'on' DirectiveLocations : argo_graphql_language_directive_definition:parse(#{'name' => extract_binary('$3'), 'locations' => '$6', 'repeatable' => true}, extract_location('$1')).
DirectiveDefinition -> 'directive' '@' Name ArgumentsDefinition 'repeatable' 'on' DirectiveLocations : argo_graphql_language_directive_definition:parse(#{'name' => extract_binary('$3'), 'arguments' => '$4', 'locations' => '$7', 'repeatable' => true}, extract_location('$1')).
DirectiveDefinition -> DescriptionDefinition 'directive' '@' Name 'on' DirectiveLocations : argo_graphql_language_directive_definition:parse(#{'description' => '$1', 'name' => extract_binary('$4'), 'locations' => '$6'}, extract_location('$2')).
DirectiveDefinition -> DescriptionDefinition 'directive' '@' Name ArgumentsDefinition 'on' DirectiveLocations : argo_graphql_language_directive_definition:parse(#{'description' => '$1', 'name' => extract_binary('$4'), 'arguments' => '$5', 'locations' => '$7'}, extract_location('$2')).
DirectiveDefinition -> DescriptionDefinition 'directive' '@' Name 'repeatable' 'on' DirectiveLocations : argo_graphql_language_directive_definition:parse(#{'description' => '$1', 'name' => extract_binary('$4'), 'locations' => '$7', 'repeatable' => true}, extract_location('$2')).
DirectiveDefinition -> DescriptionDefinition 'directive' '@' Name ArgumentsDefinition 'repeatable' 'on' DirectiveLocations : argo_graphql_language_directive_definition:parse(#{'description' => '$1', 'name' => extract_binary('$4'), 'arguments' => '$5', 'locations' => '$8', 'repeatable' => true}, extract_location('$2')).
DirectiveLocations -> DirectiveLocation : ['$1'].
DirectiveLocations -> DirectiveLocation '|' DirectiveLocations : ['$1'|'$3'].
DirectiveLocations -> '|' DirectiveLocation '|' DirectiveLocations : ['$2'|'$4'].
DirectiveLocation -> ExecutableDirectiveLocation : argo_graphql_language_directive_location:executable_directive_location('$1', extract_location('$1')).
DirectiveLocation -> TypeSystemDirectiveLocation : argo_graphql_language_directive_location:type_system_directive_location('$1', extract_location('$1')).
ExecutableDirectiveLocation -> 'executable_directive_location' : argo_graphql_language_executable_directive_location:parse(#{'name' => extract_atom('$1')}, extract_location('$1')).
TypeSystemDirectiveLocation -> 'type_system_directive_location' : argo_graphql_language_type_system_directive_location:parse(#{'name' => extract_atom('$1')}, extract_location('$1')).

Document -> DefinitionList : argo_graphql_language_document:parse(#{'definitions' => '$1'}, extract_location('$1')).

EnumTypeDefinition -> 'enum' Name : argo_graphql_language_enum_type_definition:parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
EnumTypeDefinition -> 'enum' Name DirectivesConst : argo_graphql_language_enum_type_definition:parse(#{'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
EnumTypeDefinition -> 'enum' Name EnumValuesDefinition : argo_graphql_language_enum_type_definition:parse(#{'name' => extract_binary('$2'), 'values' => '$3'}, extract_location('$1')).
EnumTypeDefinition -> 'enum' Name DirectivesConst EnumValuesDefinition : argo_graphql_language_enum_type_definition:parse(#{'name' => extract_binary('$2'), 'directives' => '$3', 'values' => '$4'}, extract_location('$1')).
EnumTypeDefinition -> DescriptionDefinition 'enum' Name : argo_graphql_language_enum_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3')}, extract_location('$2')).
EnumTypeDefinition -> DescriptionDefinition 'enum' Name DirectivesConst : argo_graphql_language_enum_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$2')).
EnumTypeDefinition -> DescriptionDefinition 'enum' Name EnumValuesDefinition : argo_graphql_language_enum_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'values' => '$4'}, extract_location('$2')).
EnumTypeDefinition -> DescriptionDefinition 'enum' Name DirectivesConst EnumValuesDefinition : argo_graphql_language_enum_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4', 'values' => '$5'}, extract_location('$2')).
EnumValuesDefinition -> '{' '}' : argo_graphql_language_enum_values_definition:parse(#{'values' => []}, extract_location('$1')).
EnumValuesDefinition -> '{' EnumValueDefinitionList '}' : argo_graphql_language_enum_values_definition:parse(#{'values' => '$2'}, extract_location('$1')).
EnumValueDefinitionList -> EnumValueDefinition : ['$1'].
EnumValueDefinitionList -> EnumValueDefinition EnumValueDefinitionList : ['$1'|'$2'].
EnumValueDefinition -> EnumValue : argo_graphql_language_enum_value_definition:parse(#{'value' => '$1'}, extract_location('$1')).
EnumValueDefinition -> EnumValue DirectivesConst : argo_graphql_language_enum_value_definition:parse(#{'value' => '$1', 'directives' => '$2'}, extract_location('$1')).
EnumValueDefinition -> DescriptionDefinition EnumValue : argo_graphql_language_enum_value_definition:parse(#{'description' => '$1', 'value' => '$2'}, extract_location('$2')).
EnumValueDefinition -> DescriptionDefinition EnumValue DirectivesConst : argo_graphql_language_enum_value_definition:parse(#{'description' => '$1', 'value' => '$2', 'directives' => '$3'}, extract_location('$2')).

EnumTypeExtension -> 'extend' 'enum' Name DirectivesConst : argo_graphql_language_enum_type_extension:parse(#{'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$1')).
EnumTypeExtension -> 'extend' 'enum' Name EnumValuesDefinition : argo_graphql_language_enum_type_extension:parse(#{'name' => extract_binary('$3'), 'values' => '$4'}, extract_location('$1')).
EnumTypeExtension -> 'extend' 'enum' Name DirectivesConst EnumValuesDefinition : argo_graphql_language_enum_type_extension:parse(#{'name' => extract_binary('$3'), 'directives' => '$4', 'values' => '$5'}, extract_location('$1')).

ExecutableDefinition -> OperationDefinition : argo_graphql_language_executable_definition:operation_definition('$1', extract_location('$1')).
ExecutableDefinition -> FragmentDefinition : argo_graphql_language_executable_definition:fragment_definition('$1', extract_location('$1')).

Field -> Name : argo_graphql_language_field:parse(#{'name' => extract_binary('$1')}, extract_location('$1')).
Field -> Name Arguments : argo_graphql_language_field:parse(#{'name' => extract_binary('$1'), 'arguments' => '$2'}, extract_location('$1')).
Field -> Name Directives : argo_graphql_language_field:parse(#{'name' => extract_binary('$1'), 'directives' => '$2'}, extract_location('$1')).
Field -> Name SelectionSet : argo_graphql_language_field:parse(#{'name' => extract_binary('$1'), 'selection_set' => '$2'}, extract_location('$1')).
Field -> Name Directives SelectionSet : argo_graphql_language_field:parse(#{'name' => extract_binary('$1'), 'directives' => '$2', 'selection_set' => '$3'}, extract_location('$1')).
Field -> Name Arguments SelectionSet : argo_graphql_language_field:parse(#{'name' => extract_binary('$1'), 'arguments' => '$2', 'selection_set' => '$3'}, extract_location('$1')).
Field -> Name Arguments Directives : argo_graphql_language_field:parse(#{'name' => extract_binary('$1'), 'arguments' => '$2', 'directives' => '$3'}, extract_location('$1')).
Field -> Name Arguments Directives SelectionSet : argo_graphql_language_field:parse(#{'name' => extract_binary('$1'), 'arguments' => '$2', 'directives' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
Field -> Alias Name : argo_graphql_language_field:parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2')}, extract_location('$1')).
Field -> Alias Name Arguments : argo_graphql_language_field:parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'arguments' => '$3'}, extract_location('$1')).
Field -> Alias Name SelectionSet : argo_graphql_language_field:parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'selection_set' => '$3'}, extract_location('$1')).
Field -> Alias Name Arguments SelectionSet : argo_graphql_language_field:parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'arguments' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
Field -> Alias Name Directives : argo_graphql_language_field:parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
Field -> Alias Name Arguments Directives : argo_graphql_language_field:parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'arguments' => '$3', 'directives' => '$4'}, extract_location('$1')).
Field -> Alias Name Directives SelectionSet : argo_graphql_language_field:parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'directives' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
Field -> Alias Name Arguments Directives SelectionSet : argo_graphql_language_field:parse(#{'alias' => extract_binary('$1'), 'name' => extract_binary('$2'), 'arguments' => '$3', 'directives' => '$4', 'selection_set' => '$5'}, extract_location('$1')).
Alias -> Name ':' : '$1'.

FieldsDefinition -> '{' '}' : argo_graphql_language_fields_definition:parse(#{'fields' => []}, extract_location('$1')).
FieldsDefinition -> '{' FieldDefinitionList '}' : argo_graphql_language_fields_definition:parse(#{'fields' => '$2'}, extract_location('$1')).
FieldDefinitionList -> FieldDefinition : ['$1'].
FieldDefinitionList -> FieldDefinition FieldDefinitionList : ['$1'|'$2'].
FieldDefinition -> Name ':' Type : argo_graphql_language_field_definition:parse(#{'name' => extract_binary('$1'), 'type' => '$3'}, extract_location('$1')).
FieldDefinition -> Name ':' Type DirectivesConst : argo_graphql_language_field_definition:parse(#{'name' => extract_binary('$1'), 'type' => '$3', 'directives' => '$4'}, extract_location('$1')).
FieldDefinition -> Name ArgumentsDefinition ':' Type : argo_graphql_language_field_definition:parse(#{'name' => extract_binary('$1'), 'arguments' => '$2', 'type' => '$4'}, extract_location('$1')).
FieldDefinition -> Name ArgumentsDefinition ':' Type DirectivesConst : argo_graphql_language_field_definition:parse(#{'name' => extract_binary('$1'), 'arguments' => '$2', 'type' => '$4', 'directives' => '$5'}, extract_location('$1')).
FieldDefinition -> DescriptionDefinition Name ':' Type : argo_graphql_language_field_definition:parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'type' => '$4'}, extract_location('$2')).
FieldDefinition -> DescriptionDefinition Name ':' Type DirectivesConst : argo_graphql_language_field_definition:parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'type' => '$4', 'directives' => '$5'}, extract_location('$2')).
FieldDefinition -> DescriptionDefinition Name ArgumentsDefinition ':' Type : argo_graphql_language_field_definition:parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'arguments' => '$3', 'type' => '$5'}, extract_location('$2')).
FieldDefinition -> DescriptionDefinition Name ArgumentsDefinition ':' Type DirectivesConst : argo_graphql_language_field_definition:parse(#{'description' => '$1', 'name' => extract_binary('$2'), 'arguments' => '$3', 'type' => '$5', 'directives' => '$6'}, extract_location('$2')).

FragmentDefinition -> 'fragment' FragmentName TypeCondition SelectionSet : argo_graphql_language_fragment_definition:parse(#{'name' => '$2', 'type_condition' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
FragmentDefinition -> 'fragment' FragmentName TypeCondition Directives SelectionSet : argo_graphql_language_fragment_definition:parse(#{'name' => '$2', 'type_condition' => '$3', 'directives' => '$4', 'selection_set' => '$5'}, extract_location('$1')).
FragmentName -> NameWithoutOn : argo_graphql_language_fragment_name:parse(#{'name' => extract_binary('$1')}, extract_location('$1')).
TypeCondition -> 'on' NamedType : argo_graphql_language_type_condition:parse(#{'type' => '$2'}, extract_location('$1')).

FragmentSpread -> '...' FragmentName : argo_graphql_language_fragment_spread:parse(#{'name' => '$2'}, extract_location('$1')).
FragmentSpread -> '...' FragmentName Directives : argo_graphql_language_fragment_spread:parse(#{'name' => '$2', 'directives' => '$3'}, extract_location('$1')).

ImplementsInterfaces -> 'implements' ImplementsInterfacesList : argo_graphql_language_implements_interfaces:parse(#{'interfaces' => '$2'}, extract_location('$1')).
ImplementsInterfacesList -> NamedType : ['$1'].
ImplementsInterfacesList -> NamedType '&' ImplementsInterfacesList : ['$1'|'$3'].
ImplementsInterfacesList -> '&' NamedType '&' ImplementsInterfacesList : ['$2'|'$4'].

InlineFragment -> '...' TypeCondition SelectionSet : argo_graphql_language_inline_fragment:parse(#{'type_condition' => '$2', 'selection_set' => '$3'}, extract_location('$1')).
InlineFragment -> '...' TypeCondition Directives SelectionSet : argo_graphql_language_inline_fragment:parse(#{'type_condition' => '$2', 'directives' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
InlineFragment -> '...' Directives SelectionSet : argo_graphql_language_inline_fragment:parse(#{'directives' => '$2', 'selection_set' => '$3'}, extract_location('$1')).
InlineFragment -> '...' SelectionSet : argo_graphql_language_inline_fragment:parse(#{'selection_set' => '$2'}, extract_location('$1')).

InputObjectTypeDefinition -> 'input' Name : argo_graphql_language_input_object_type_definition:parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
InputObjectTypeDefinition -> 'input' Name DirectivesConst : argo_graphql_language_input_object_type_definition:parse(#{'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
InputObjectTypeDefinition -> 'input' Name InputFieldsDefinition : argo_graphql_language_input_object_type_definition:parse(#{'name' => extract_binary('$2'), 'fields' => '$3'}, extract_location('$1')).
InputObjectTypeDefinition -> 'input' Name DirectivesConst InputFieldsDefinition : argo_graphql_language_input_object_type_definition:parse(#{'name' => extract_binary('$2'), 'directives' => '$3', 'fields' => '$4'}, extract_location('$1')).
InputObjectTypeDefinition -> DescriptionDefinition 'input' Name : argo_graphql_language_input_object_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3')}, extract_location('$2')).
InputObjectTypeDefinition -> DescriptionDefinition 'input' Name DirectivesConst : argo_graphql_language_input_object_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$2')).
InputObjectTypeDefinition -> DescriptionDefinition 'input' Name InputFieldsDefinition : argo_graphql_language_input_object_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'fields' => '$4'}, extract_location('$2')).
InputObjectTypeDefinition -> DescriptionDefinition 'input' Name DirectivesConst InputFieldsDefinition : argo_graphql_language_input_object_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4', 'fields' => '$5'}, extract_location('$2')).
InputFieldsDefinition -> '{' '}' : argo_graphql_language_input_fields_definition:parse(#{'inputs' => []}, extract_location('$1')).
InputFieldsDefinition -> '{' InputValueDefinitionList '}' : argo_graphql_language_input_fields_definition:parse(#{'inputs' => '$2'}, extract_location('$1')).

InputObjectTypeExtension -> 'extend' 'input' Name DirectivesConst : argo_graphql_language_input_object_type_extension:parse(#{'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$1')).
InputObjectTypeExtension -> 'extend' 'input' Name InputFieldsDefinition : argo_graphql_language_input_object_type_extension:parse(#{'name' => extract_binary('$3'), 'fields' => '$4'}, extract_location('$1')).
InputObjectTypeExtension -> 'extend' 'input' Name DirectivesConst InputFieldsDefinition : argo_graphql_language_input_object_type_extension:parse(#{'name' => extract_binary('$3'), 'directives' => '$4', 'fields' => '$5'}, extract_location('$1')).

InterfaceTypeDefinition -> 'interface' Name : argo_graphql_language_interface_type_definition:parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name ImplementsInterfaces : argo_graphql_language_interface_type_definition:parse(#{'name' => extract_binary('$2'), 'implements' => '$3'}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name DirectivesConst : argo_graphql_language_interface_type_definition:parse(#{'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name FieldsDefinition : argo_graphql_language_interface_type_definition:parse(#{'name' => extract_binary('$2'), 'fields' => '$3'}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name ImplementsInterfaces DirectivesConst : argo_graphql_language_interface_type_definition:parse(#{'name' => extract_binary('$2'), 'implements' => '$3', 'directives' => '$4'}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name ImplementsInterfaces FieldsDefinition : argo_graphql_language_interface_type_definition:parse(#{'name' => extract_binary('$2'), 'implements' => '$3', 'fields' => '$4'}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name DirectivesConst FieldsDefinition : argo_graphql_language_interface_type_definition:parse(#{'name' => extract_binary('$2'), 'directives' => '$3', 'fields' => '$4'}, extract_location('$1')).
InterfaceTypeDefinition -> 'interface' Name ImplementsInterfaces DirectivesConst FieldsDefinition : argo_graphql_language_interface_type_definition:parse(#{'name' => extract_binary('$2'), 'implements' => '$3', 'directives' => '$4', 'fields' => '$5'}, extract_location('$1')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name : argo_graphql_language_interface_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3')}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name ImplementsInterfaces : argo_graphql_language_interface_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4'}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name DirectivesConst : argo_graphql_language_interface_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name FieldsDefinition : argo_graphql_language_interface_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'fields' => '$4'}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name ImplementsInterfaces DirectivesConst : argo_graphql_language_interface_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5'}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name ImplementsInterfaces FieldsDefinition : argo_graphql_language_interface_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4', 'fields' => '$5'}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name DirectivesConst FieldsDefinition : argo_graphql_language_interface_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4', 'fields' => '$5'}, extract_location('$2')).
InterfaceTypeDefinition -> DescriptionDefinition 'interface' Name ImplementsInterfaces DirectivesConst FieldsDefinition : argo_graphql_language_interface_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5', 'fields' => '$6'}, extract_location('$2')).

InterfaceTypeExtension -> 'extend' 'interface' Name DirectivesConst : argo_graphql_language_interface_type_extension:parse(#{'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$1')).
InterfaceTypeExtension -> 'extend' 'interface' Name FieldsDefinition : argo_graphql_language_interface_type_extension:parse(#{'name' => extract_binary('$3'), 'fields' => '$4'}, extract_location('$1')).
InterfaceTypeExtension -> 'extend' 'interface' Name ImplementsInterfaces : argo_graphql_language_interface_type_extension:parse(#{'name' => extract_binary('$3'), 'implements' => '$4'}, extract_location('$1')).
InterfaceTypeExtension -> 'extend' 'interface' Name DirectivesConst FieldsDefinition : argo_graphql_language_interface_type_extension:parse(#{'name' => extract_binary('$3'), 'directives' => '$4', 'fields' => '$5'}, extract_location('$1')).
InterfaceTypeExtension -> 'extend' 'interface' Name ImplementsInterfaces DirectivesConst : argo_graphql_language_interface_type_extension:parse(#{'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5'}, extract_location('$1')).
InterfaceTypeExtension -> 'extend' 'interface' Name ImplementsInterfaces FieldsDefinition : argo_graphql_language_interface_type_extension:parse(#{'name' => extract_binary('$3'), 'implements' => '$4', 'fields' => '$5'}, extract_location('$1')).
InterfaceTypeExtension -> 'extend' 'interface' Name ImplementsInterfaces DirectivesConst FieldsDefinition : argo_graphql_language_interface_type_extension:parse(#{'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5', 'fields' => '$6'}, extract_location('$1')).

NameWithoutOn -> 'name' : '$1'.
NameWithoutOn -> 'query' : '$1'.
NameWithoutOn -> 'mutation' : '$1'.
NameWithoutOn -> 'subscription' : '$1'.
NameWithoutOn -> 'fragment' : '$1'.
NameWithoutOn -> 'type' : '$1'.
NameWithoutOn -> 'implements' : '$1'.
NameWithoutOn -> 'interface' : '$1'.
NameWithoutOn -> 'union' : '$1'.
NameWithoutOn -> 'scalar' : '$1'.
NameWithoutOn -> 'schema' : '$1'.
NameWithoutOn -> 'enum' : '$1'.
NameWithoutOn -> 'input' : '$1'.
NameWithoutOn -> 'extend' : '$1'.
NameWithoutOn -> 'directive' : '$1'.
Name -> NameWithoutOn : '$1'.
Name -> 'on' : '$1'.

ObjectTypeDefinition -> 'type' Name : argo_graphql_language_object_type_definition:parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name ImplementsInterfaces : argo_graphql_language_object_type_definition:parse(#{'name' => extract_binary('$2'), 'implements' => '$3'}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name DirectivesConst : argo_graphql_language_object_type_definition:parse(#{'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name FieldsDefinition : argo_graphql_language_object_type_definition:parse(#{'name' => extract_binary('$2'), 'fields' => '$3'}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name ImplementsInterfaces DirectivesConst : argo_graphql_language_object_type_definition:parse(#{'name' => extract_binary('$2'), 'implements' => '$3', 'directives' => '$4'}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name ImplementsInterfaces FieldsDefinition : argo_graphql_language_object_type_definition:parse(#{'name' => extract_binary('$2'), 'implements' => '$3', 'fields' => '$4'}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name DirectivesConst FieldsDefinition : argo_graphql_language_object_type_definition:parse(#{'name' => extract_binary('$2'), 'directives' => '$3', 'fields' => '$4'}, extract_location('$1')).
ObjectTypeDefinition -> 'type' Name ImplementsInterfaces DirectivesConst FieldsDefinition : argo_graphql_language_object_type_definition:parse(#{'name' => extract_binary('$2'), 'implements' => '$3', 'directives' => '$4', 'fields' => '$5'}, extract_location('$1')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name : argo_graphql_language_object_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3')}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name ImplementsInterfaces : argo_graphql_language_object_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4'}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name DirectivesConst : argo_graphql_language_object_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name FieldsDefinition : argo_graphql_language_object_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'fields' => '$4'}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name ImplementsInterfaces DirectivesConst : argo_graphql_language_object_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5'}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name ImplementsInterfaces FieldsDefinition : argo_graphql_language_object_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4', 'fields' => '$5'}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name DirectivesConst FieldsDefinition : argo_graphql_language_object_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4', 'fields' => '$5'}, extract_location('$2')).
ObjectTypeDefinition -> DescriptionDefinition 'type' Name ImplementsInterfaces DirectivesConst FieldsDefinition : argo_graphql_language_object_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5', 'fields' => '$6'}, extract_location('$2')).

ObjectTypeExtension -> 'extend' 'type' Name DirectivesConst : argo_graphql_language_object_type_extension:parse(#{'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$1')).
ObjectTypeExtension -> 'extend' 'type' Name FieldsDefinition : argo_graphql_language_object_type_extension:parse(#{'name' => extract_binary('$3'), 'fields' => '$4'}, extract_location('$1')).
ObjectTypeExtension -> 'extend' 'type' Name ImplementsInterfaces : argo_graphql_language_object_type_extension:parse(#{'name' => extract_binary('$3'), 'implements' => '$4'}, extract_location('$1')).
ObjectTypeExtension -> 'extend' 'type' Name DirectivesConst FieldsDefinition : argo_graphql_language_object_type_extension:parse(#{'name' => extract_binary('$3'), 'directives' => '$4', 'fields' => '$5'}, extract_location('$1')).
ObjectTypeExtension -> 'extend' 'type' Name ImplementsInterfaces DirectivesConst : argo_graphql_language_object_type_extension:parse(#{'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5'}, extract_location('$1')).
ObjectTypeExtension -> 'extend' 'type' Name ImplementsInterfaces FieldsDefinition : argo_graphql_language_object_type_extension:parse(#{'name' => extract_binary('$3'), 'implements' => '$4', 'fields' => '$5'}, extract_location('$1')).
ObjectTypeExtension -> 'extend' 'type' Name ImplementsInterfaces DirectivesConst FieldsDefinition : argo_graphql_language_object_type_extension:parse(#{'name' => extract_binary('$3'), 'implements' => '$4', 'directives' => '$5', 'fields' => '$6'}, extract_location('$1')).

OperationDefinition -> SelectionSet : argo_graphql_language_operation_definition:parse(#{'operation' => 'query', 'selection_set' => '$1', 'shorthand' => true}, extract_location('$1')).
OperationDefinition -> OperationType SelectionSet : argo_graphql_language_operation_definition:parse(#{'operation' => extract_atom('$1'), 'selection_set' => '$2'}, extract_location('$1')).
OperationDefinition -> OperationType VariablesDefinition SelectionSet : argo_graphql_language_operation_definition:parse(#{'operation' => extract_atom('$1'), 'variables_definition' => '$2', 'selection_set' => '$3'}, extract_location('$1')).
OperationDefinition -> OperationType Directives SelectionSet : argo_graphql_language_operation_definition:parse(#{'operation' => extract_atom('$1'), 'directives' => '$2', 'selection_set' => '$3'}, extract_location('$1')).
OperationDefinition -> OperationType VariablesDefinition Directives SelectionSet : argo_graphql_language_operation_definition:parse(#{'operation' => extract_atom('$1'), 'variables_definition' => '$2', 'directives' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
OperationDefinition -> OperationType Name SelectionSet : argo_graphql_language_operation_definition:parse(#{'operation' => extract_atom('$1'), 'name' => extract_binary('$2'), 'selection_set' => '$3'}, extract_location('$1')).
OperationDefinition -> OperationType Name VariablesDefinition SelectionSet : argo_graphql_language_operation_definition:parse(#{'operation' => extract_atom('$1'), 'name' => extract_binary('$2'), 'variables_definition' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
OperationDefinition -> OperationType Name Directives SelectionSet : argo_graphql_language_operation_definition:parse(#{'operation' => extract_atom('$1'), 'name' => extract_binary('$2'), 'directives' => '$3', 'selection_set' => '$4'}, extract_location('$1')).
OperationDefinition -> OperationType Name VariablesDefinition Directives SelectionSet : argo_graphql_language_operation_definition:parse(#{'operation' => extract_atom('$1'), 'name' => extract_binary('$2'), 'variables_definition' => '$3', 'directives' => '$4', 'selection_set' => '$5'}, extract_location('$1')).

OperationType -> 'query' : '$1'.
OperationType -> 'mutation' : '$1'.
OperationType -> 'subscription' : '$1'.

RootOperationTypesDefinition -> '{' '}' : argo_graphql_language_root_operation_types_definition:parse(#{'operations' => []}, extract_location('$1')).
RootOperationTypesDefinition -> '{' RootOperationTypeDefinitionList '}' : argo_graphql_language_root_operation_types_definition:parse(#{'operations' => '$2'}, extract_location('$1')).
RootOperationTypeDefinitionList -> RootOperationTypeDefinition : ['$1'].
RootOperationTypeDefinitionList -> RootOperationTypeDefinition RootOperationTypeDefinitionList : ['$1'|'$2'].
RootOperationTypeDefinition -> OperationType ':' NamedType : argo_graphql_language_root_operation_type_definition:parse(#{'operation_type' => extract_atom('$1'), 'named_type' => '$3'}, extract_location('$1')).

ScalarTypeDefinition -> 'scalar' Name : argo_graphql_language_scalar_type_definition:parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
ScalarTypeDefinition -> 'scalar' Name DirectivesConst : argo_graphql_language_scalar_type_definition:parse(#{'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
ScalarTypeDefinition -> DescriptionDefinition 'scalar' Name : argo_graphql_language_scalar_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3')}, extract_location('$2')).
ScalarTypeDefinition -> DescriptionDefinition 'scalar' Name DirectivesConst : argo_graphql_language_scalar_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$2')).

ScalarTypeExtension -> 'extend' 'scalar' Name DirectivesConst : argo_graphql_language_scalar_type_extension:parse(#{'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$1')).

SchemaDefinition -> 'schema' DirectivesConst : argo_graphql_language_schema_definition:parse(#{'directives' => '$2'}, extract_location('$1')).
SchemaDefinition -> 'schema' RootOperationTypesDefinition : argo_graphql_language_schema_definition:parse(#{'operations' => '$2'}, extract_location('$1')).
SchemaDefinition -> 'schema' DirectivesConst RootOperationTypesDefinition : argo_graphql_language_schema_definition:parse(#{'directives' => '$2', 'operations' => '$3'}, extract_location('$1')).
SchemaDefinition -> DescriptionDefinition 'schema' DirectivesConst : argo_graphql_language_schema_definition:parse(#{'description' => '$1', 'directives' => '$3'}, extract_location('$2')).
SchemaDefinition -> DescriptionDefinition 'schema' RootOperationTypesDefinition : argo_graphql_language_schema_definition:parse(#{'description' => '$1', 'operations' => '$3'}, extract_location('$2')).
SchemaDefinition -> DescriptionDefinition 'schema' DirectivesConst RootOperationTypesDefinition : argo_graphql_language_schema_definition:parse(#{'description' => '$1', 'directives' => '$3', 'operations' => '$4'}, extract_location('$2')).

SchemaExtension -> 'extend' 'schema' DirectivesConst : argo_graphql_language_schema_extension:parse(#{'directives' => '$3'}, extract_location('$1')).
SchemaExtension -> 'extend' 'schema' RootOperationTypesDefinition : argo_graphql_language_schema_extension:parse(#{'operations' => '$3'}, extract_location('$1')).
SchemaExtension -> 'extend' 'schema' DirectivesConst RootOperationTypesDefinition : argo_graphql_language_schema_extension:parse(#{'directives' => '$3', 'operations' => '$4'}, extract_location('$1')).

SelectionSet -> '{' SelectionList '}' : argo_graphql_language_selection_set:parse(#{'selections' => '$2'}, extract_location('$1')).
SelectionList -> Selection : ['$1'].
SelectionList -> Selection SelectionList : ['$1'|'$2'].
Selection -> Field : argo_graphql_language_selection:field('$1', extract_location('$1')).
Selection -> FragmentSpread : argo_graphql_language_selection:fragment_spread('$1', extract_location('$1')).
Selection -> InlineFragment : argo_graphql_language_selection:inline_fragment('$1', extract_location('$1')).

Type -> NamedType : argo_graphql_language_type:named_type('$1', extract_location('$1')).
Type -> ListType : argo_graphql_language_type:list_type('$1', extract_location('$1')).
Type -> NonNullType : argo_graphql_language_type:non_null_type('$1', extract_location('$1')).
NamedType -> Name : argo_graphql_language_named_type:parse(#{'name' => extract_binary('$1')}, extract_location('$1')).
ListType -> '[' Type ']' : argo_graphql_language_list_type:parse(#{'type' => '$2'}, extract_location('$1')).
NonNullType -> NamedType '!' : argo_graphql_language_non_null_type:parse(#{'type' => '$1'}, extract_location('$1')).
NonNullType -> ListType '!' : argo_graphql_language_non_null_type:parse(#{'type' => '$1'}, extract_location('$1')).

TypeDefinition -> ScalarTypeDefinition : argo_graphql_language_type_definition:scalar_type_definition('$1', extract_location('$1')).
TypeDefinition -> ObjectTypeDefinition : argo_graphql_language_type_definition:object_type_definition('$1', extract_location('$1')).
TypeDefinition -> InterfaceTypeDefinition : argo_graphql_language_type_definition:interface_type_definition('$1', extract_location('$1')).
TypeDefinition -> UnionTypeDefinition : argo_graphql_language_type_definition:union_type_definition('$1', extract_location('$1')).
TypeDefinition -> EnumTypeDefinition : argo_graphql_language_type_definition:enum_type_definition('$1', extract_location('$1')).
TypeDefinition -> InputObjectTypeDefinition : argo_graphql_language_type_definition:input_object_type_definition('$1', extract_location('$1')).

TypeExtension -> ScalarTypeExtension : argo_graphql_language_type_extension:scalar_type_extension('$1', extract_location('$1')).
TypeExtension -> ObjectTypeExtension : argo_graphql_language_type_extension:object_type_extension('$1', extract_location('$1')).
TypeExtension -> InterfaceTypeExtension : argo_graphql_language_type_extension:interface_type_extension('$1', extract_location('$1')).
TypeExtension -> UnionTypeExtension : argo_graphql_language_type_extension:union_type_extension('$1', extract_location('$1')).
TypeExtension -> EnumTypeExtension : argo_graphql_language_type_extension:enum_type_extension('$1', extract_location('$1')).
TypeExtension -> InputObjectTypeExtension : argo_graphql_language_type_extension:input_object_type_extension('$1', extract_location('$1')).

TypeSystemDefinition -> SchemaDefinition : argo_graphql_language_type_system_definition:schema_definition('$1', extract_location('$1')).
TypeSystemDefinition -> TypeDefinition : argo_graphql_language_type_system_definition:type_definition('$1', extract_location('$1')).
TypeSystemDefinition -> DirectiveDefinition : argo_graphql_language_type_system_definition:directive_definition('$1', extract_location('$1')).

TypeSystemExtension -> SchemaExtension : argo_graphql_language_type_system_extension:schema_extension('$1', extract_location('$1')).
TypeSystemExtension -> TypeExtension : argo_graphql_language_type_system_extension:type_extension('$1', extract_location('$1')).

UnionTypeDefinition -> 'union' Name : argo_graphql_language_union_type_definition:parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
UnionTypeDefinition -> 'union' Name DirectivesConst : argo_graphql_language_union_type_definition:parse(#{'name' => extract_binary('$2'), 'directives' => '$3'}, extract_location('$1')).
UnionTypeDefinition -> 'union' Name '=' UnionMemberTypes : argo_graphql_language_union_type_definition:parse(#{'name' => extract_binary('$2'), 'types' => '$4'}, extract_location('$1')).
UnionTypeDefinition -> 'union' Name DirectivesConst '=' UnionMemberTypes : argo_graphql_language_union_type_definition:parse(#{'name' => extract_binary('$2'), 'directives' => '$3', 'types' => '$5'}, extract_location('$1')).
UnionTypeDefinition -> DescriptionDefinition 'union' Name : argo_graphql_language_union_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3')}, extract_location('$2')).
UnionTypeDefinition -> DescriptionDefinition 'union' Name DirectivesConst : argo_graphql_language_union_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$2')).
UnionTypeDefinition -> DescriptionDefinition 'union' Name '=' UnionMemberTypes : argo_graphql_language_union_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'types' => '$5'}, extract_location('$2')).
UnionTypeDefinition -> DescriptionDefinition 'union' Name DirectivesConst '=' UnionMemberTypes : argo_graphql_language_union_type_definition:parse(#{'description' => '$1', 'name' => extract_binary('$3'), 'directives' => '$4', 'types' => '$6'}, extract_location('$2')).
UnionMemberTypes -> NamedType : ['$1'].
UnionMemberTypes -> NamedType '|' UnionMemberTypes : ['$1'|'$3'].
UnionMemberTypes -> '|' NamedType '|' UnionMemberTypes : ['$2'|'$4'].

UnionTypeExtension -> 'extend' 'union' Name DirectivesConst : argo_graphql_language_union_type_extension:parse(#{'name' => extract_binary('$3'), 'directives' => '$4'}, extract_location('$1')).
UnionTypeExtension -> 'extend' 'union' Name '=' UnionMemberTypes : argo_graphql_language_union_type_extension:parse(#{'name' => extract_binary('$3'), 'types' => '$5'}, extract_location('$1')).
UnionTypeExtension -> 'extend' 'union' Name DirectivesConst '=' UnionMemberTypes : argo_graphql_language_union_type_extension:parse(#{'name' => extract_binary('$3'), 'directives' => '$4', 'types' => '$6'}, extract_location('$1')).

Value -> Variable : argo_graphql_language_value:variable('$1', extract_location('$1')).
Value -> int_value : argo_graphql_language_value:int(extract_integer('$1'), extract_location('$1')).
Value -> float_value : argo_graphql_language_value:float(extract_float('$1'), extract_location('$1')).
Value -> block_string_value : argo_graphql_language_value:string(extract_quoted_block_string_token('$1'), extract_location('$1')).
Value -> string_value : argo_graphql_language_value:string(extract_quoted_string_token('$1'), extract_location('$1')).
Value -> boolean_value : argo_graphql_language_value:boolean(extract_boolean('$1'), extract_location('$1')).
Value -> null : argo_graphql_language_value:null(extract_location('$1')).
Value -> EnumValue : argo_graphql_language_value:enum('$1', extract_location('$1')).
Value -> ListValue : argo_graphql_language_value:list('$1', extract_location('$1')).
Value -> ObjectValue : argo_graphql_language_value:object('$1', extract_location('$1')).
EnumValue -> Name : argo_graphql_language_enum_value:parse(#{'name' => extract_binary('$1')}, extract_location('$1')).
ListValue -> '[' ']' : argo_graphql_language_list_value:parse(#{'list' => []}, extract_location('$1')).
ListValue -> '[' ValueList ']' : argo_graphql_language_list_value:parse(#{'list' => '$2'}, extract_location('$1')).
ValueList -> Value : ['$1'].
ValueList -> Value ValueList : ['$1'|'$2'].
ObjectValue -> '{' '}' : argo_graphql_language_object_value:parse(#{'fields' => []}, extract_location('$1')).
ObjectValue -> '{' ObjectFieldList '}' : argo_graphql_language_object_value:parse(#{'fields' => '$2'}, extract_location('$1')).
ObjectFieldList -> ObjectField : ['$1'].
ObjectFieldList -> ObjectField ObjectFieldList : ['$1'|'$2'].
ObjectField -> Name ':' Value : argo_graphql_language_object_field:parse(#{'name' => extract_binary('$1'), 'value' => '$3'}, extract_location('$1')).

ValueConst -> int_value : argo_graphql_language_value_const:int(extract_integer('$1'), extract_location('$1')).
ValueConst -> float_value : argo_graphql_language_value_const:float(extract_float('$1'), extract_location('$1')).
ValueConst -> block_string_value : argo_graphql_language_value_const:string(extract_quoted_block_string_token('$1'), extract_location('$1')).
ValueConst -> string_value : argo_graphql_language_value_const:string(extract_quoted_string_token('$1'), extract_location('$1')).
ValueConst -> boolean_value : argo_graphql_language_value_const:boolean(extract_boolean('$1'), extract_location('$1')).
ValueConst -> null : argo_graphql_language_value_const:null(extract_location('$1')).
ValueConst -> EnumValue : argo_graphql_language_value_const:enum('$1', extract_location('$1')).
ValueConst -> ListValueConst : argo_graphql_language_value_const:list('$1', extract_location('$1')).
ValueConst -> ObjectValueConst : argo_graphql_language_value_const:object('$1', extract_location('$1')).
ListValueConst -> '[' ']' : argo_graphql_language_list_value_const:parse(#{'list' => []}, extract_location('$1')).
ListValueConst -> '[' ValueConstList ']' : argo_graphql_language_list_value_const:parse(#{'list' => '$2'}, extract_location('$1')).
ValueConstList -> ValueConst : ['$1'].
ValueConstList -> ValueConst ValueConstList : ['$1'|'$2'].
ObjectValueConst -> '{' '}' : argo_graphql_language_object_value_const:parse(#{'fields' => []}, extract_location('$1')).
ObjectValueConst -> '{' ObjectFieldConstList '}' : argo_graphql_language_object_value_const:parse(#{'fields' => '$2'}, extract_location('$1')).
ObjectFieldConstList -> ObjectFieldConst : ['$1'].
ObjectFieldConstList -> ObjectFieldConst ObjectFieldConstList : ['$1'|'$2'].
ObjectFieldConst -> Name ':' ValueConst : argo_graphql_language_object_field_const:parse(#{'name' => extract_binary('$1'), 'value' => '$3'}, extract_location('$1')).

VariablesDefinition -> '(' VariableDefinitionList ')' : argo_graphql_language_variables_definition:parse(#{'variables' => '$2'}, extract_location('$1')).
VariableDefinitionList -> VariableDefinition : ['$1'].
VariableDefinitionList -> VariableDefinition VariableDefinitionList : ['$1'|'$2'].
VariableDefinition -> Variable ':' Type : argo_graphql_language_variable_definition:parse(#{'variable' => '$1', 'type' => '$3'}, extract_location('$1')).
VariableDefinition -> Variable ':' Type DirectivesConst: argo_graphql_language_variable_definition:parse(#{'variable' => '$1', 'type' => '$3', 'directives' => '$4'}, extract_location('$1')).
VariableDefinition -> Variable ':' Type DefaultValue : argo_graphql_language_variable_definition:parse(#{'variable' => '$1', 'type' => '$3', 'default_value' => '$4'}, extract_location('$1')).
VariableDefinition -> Variable ':' Type DefaultValue DirectivesConst : argo_graphql_language_variable_definition:parse(#{'variable' => '$1', 'type' => '$3', 'default_value' => '$4', 'directives' => '$5'}, extract_location('$1')).
Variable -> '$' NameWithoutOn : argo_graphql_language_variable:parse(#{'name' => extract_binary('$2')}, extract_location('$1')).
Variable -> '$' 'on' : argo_graphql_language_variable:parse(#{'name' => extract_binary('$2')}, extract_location('$1')).

Expect 35.

Erlang code.

-ignore_xref([return_error/2]).

% Line-Level Utilities

extract_location({_Token, {Line, Column}}) ->
  erl_anno:new({Line, Column});
extract_location({_Token, {Line, Column}, _Value}) ->
  erl_anno:new({Line, Column});
extract_location(Record) when is_tuple(Record) andalso tuple_size(Record) >= 2 andalso is_atom(element(1, Record)) andalso is_tuple(element(2, Record)) andalso tuple_size(element(2, Record)) =:= 2 ->
  {Line, Column} = element(2, Record),
  erl_anno:new({Line, Column});
extract_location([Record | _]) when is_tuple(Record) ->
  extract_location(Record).

% Value-level Utilities

extract_atom({Value, _Loc}) ->
  Value;
extract_atom({executable_directive_location, _Loc, Value}) ->
  argo_graphql_language_executable_directive_location:name_from_string(Value);
extract_atom({type_system_directive_location, _Loc, Value}) ->
  argo_graphql_language_type_system_directive_location:name_from_string(Value).

extract_binary(Value) when is_binary(Value) ->
  Value;
extract_binary({Token, _Loc}) when is_atom(Token) ->
  erlang:atom_to_binary(Token, utf8);
extract_binary({_Token, _Loc, Value}) ->
  argo_types:unicode_binary(Value).

% String

extract_quoted_string_token({_Token, _Loc, Value}) ->
  argo_types:unicode_binary(lists:sublist(Value, 2, length(Value) - 2)).

% Block String

extract_quoted_block_string_token({_Token, _Loc, Value}) ->
  argo_types:unicode_binary(process_block_string(lists:sublist(Value, 4, length(Value) - 6))).

-spec process_block_string(string()) -> string().
process_block_string(Escaped) ->
  process_block_string(Escaped, []).

-spec process_block_string(string(), string()) -> string().
process_block_string([], Acc) ->
  block_string_value(lists:reverse(Acc));
process_block_string([$\r, $\n | T], Acc) -> process_block_string(T, [$\n | Acc]);
process_block_string([$\\, $", $", $" | T], Acc) -> process_block_string(T, [$", $", $"] ++ Acc);
process_block_string([H | T], Acc) -> process_block_string(T, [H | Acc]).

-spec block_string_value(string()) -> string().
block_string_value(Value) ->
  [FirstLine | Rest] = string:split(Value, "\n", all),
  Prefix = indentation_prefix(common_indent(Rest)),
  UnindentedLines = unindent(Rest, Prefix),
  Lines = trim_blank_lines([FirstLine | UnindentedLines]),
  string:join(Lines, "\n").

-spec trim_blank_lines([string()]) -> [string()].
trim_blank_lines(Lines) ->
  trim_blank_lines(trim_blank_lines(Lines, leading), trailing).

-spec trim_blank_lines([string()], leading | trailing) -> [string()].
trim_blank_lines(Lines, leading) ->
  lists:dropwhile(fun is_blank/1, Lines);
trim_blank_lines(Lines, trailing) ->
  lists:reverse(trim_blank_lines(lists:reverse(Lines), leading)).

-spec indentation_prefix(non_neg_integer()) -> string().
indentation_prefix(Indent) ->
  lists:map(fun(_) -> 32 end, lists:seq(1, Indent)).

-spec unindent([string()], string()) -> [string()].
unindent(Lines, Prefix) ->
  unindent(Lines, Prefix, []).

-spec unindent([string()], string(), [string()]) -> [string()].
unindent([], _Prefix, Result) ->
  lists:reverse(Result);
unindent([H | T], Prefix, Result) ->
  Processed = prefix(H, Prefix),
  unindent(T, Prefix, [Processed | Result]).

-spec prefix(string(), string()) -> string().
prefix(Line, []) ->
  Line;
prefix(Line, Prefix) ->
  Prefixed = lists:prefix(Prefix, Line),
  if
    Prefixed ->
      string:substr(Line, length(Prefix) + 1);
    true ->
      Line
  end.

-spec common_indent([string()]) -> non_neg_integer().
common_indent(Lines) ->
  case common_indent(Lines, noindent) of
    noindent ->
      0;
    Indent ->
      Indent
  end.

-spec common_indent([string()], noindent | non_neg_integer()) -> noindent | non_neg_integer().
common_indent([], Indent) ->
    Indent;
common_indent([H | T], Indent) ->
  CurrentIndent = leading_whitespace(H),
  if
    (CurrentIndent < length(H)) and ((Indent == noindent) or (CurrentIndent < Indent)) ->
      common_indent(T, CurrentIndent);
    true ->
      common_indent(T, Indent)
  end.

-spec leading_whitespace(string()) -> non_neg_integer().
leading_whitespace(BlockStringValue) ->
  leading_whitespace(BlockStringValue, 0).

-spec leading_whitespace(string(), non_neg_integer()) -> non_neg_integer().
leading_whitespace([], N) ->
  N;
leading_whitespace([32 | T], N) ->
  leading_whitespace(T, N + 1);
leading_whitespace([$\t | T], N) ->
  leading_whitespace(T, N + 1);
leading_whitespace([_H | _T], N) ->
  N.

-spec is_blank(string()) -> boolean().
is_blank(BlockStringValue) ->
    leading_whitespace(BlockStringValue) == length(BlockStringValue).

% Integer

extract_integer({int_value, _Loc, Value}) when is_integer(Value) ->
  Value.

% Float

extract_float({float_value, _Loc, Value}) when is_float(Value) ->
  Value.

% Boolean

extract_boolean({boolean_value, _Loc, Value}) when is_boolean(Value) ->
  Value.
