-module(constraint_string).

-export([validate/4]).

validate(Element, Schema, Errors, Parent) ->
	Id = proplists:get_value(<<"id">>, Schema),
	IsBinary = is_binary(Element),

	if 
		IsBinary -> Errors;
		true ->
			[list_to_binary(io_lib:format("Object \"~s~s\" must be a string", [Parent, Id])) | Errors]
	end.