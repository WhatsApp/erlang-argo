-module(argo_graphql).

-export([
	display/1,
	display/2,
	format/1
]).

%% Macros
-define(is_record(X), (is_tuple((X)) andalso tuple_size((X)) > 0 andalso is_atom(element(1, (X))))).

-spec display(dynamic()) -> ok.
display(Type) when ?is_record(Type) ->
	display(standard_io, Type).

-spec display(io:device(), dynamic()) -> ok.
display(IoDevice, Type) when not is_list(IoDevice) andalso ?is_record(Type) ->
	Module = element(1, Type),
	Printer1 = argo_graphql_printer:new_io_device(IoDevice),
	Printer2 = Module:format(Printer1, Type),
	case argo_graphql_printer:finalize(Printer2) of
		ok ->
			ok
	end.

-spec format(dynamic()) -> binary().
format(Type) when ?is_record(Type) ->
	Module = element(1, Type),
	Printer1 = argo_graphql_printer:new_string(),
	Printer2 = Module:format(Printer1, Type),
	case argo_graphql_printer:finalize(Printer2) of
		Output when is_list(Output) ->
			erlang:iolist_to_binary(Output)
	end.
