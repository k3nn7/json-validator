-module(constraint_boolean).

-export([validate/4]).

validate(Element, Schema, Errors, Parent) ->
	Id = proplists:get_value(<<"id">>, Schema),
	IsBoolean = is_boolean(Element),

	if 
		IsBoolean -> Errors;

		true ->
			[list_to_binary(io_lib:format("Object \"~s~s\" must be an boolean", [Parent, Id])) | Errors]
	end.