-module(constraint_boolean).

-export([check/4]).

check(Element, Schema, Errors, Parent) ->
	io:format("~nChecking BOOLEAN: ~p with schema ~p~n", [Element, Schema]),

	Id = proplists:get_value(<<"id">>, Schema),
	IsBoolean = is_boolean(Element),

	if 
		IsBoolean -> Errors;

		true ->
			[list_to_binary(io_lib:format("Object \"~s~s\" must be an boolean", [Parent, Id])) | Errors]
	end.