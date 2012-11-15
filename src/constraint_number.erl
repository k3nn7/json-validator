-module(constraint_number).

-export([check/4]).

check(Element, Schema, Errors, Parent) ->
	Id = proplists:get_value(<<"id">>, Schema),
	IsInteger = is_integer(Element),

	if 
		IsInteger -> Errors;
		true ->
			[list_to_binary(io_lib:format("Object \"~s~s\" must be an integer", [Parent, Id])) | Errors]
	end.