-module(constraint_array).

-export([validate/4]).

validate(Element, Schema, Errors, Parent) ->
	{struct, ItemsSchema} = proplists:get_value(<<"items">>, Schema),
	MyName = proplists:get_value(<<"id">>, Schema),
	IsRequired  = proplists:get_value(<<"required">>, ItemsSchema, false),

	case Element of
		[] when IsRequired ->
			[list_to_binary(io_lib:format("Array \"~s~s\" has no items", [Parent, MyName])) | Errors];

		[] ->
			Errors;

		Element when is_list(Element) ->
			check_array_items(Element, {struct, ItemsSchema}, Errors, 
								 <<Parent/binary, MyName/binary, ".">>);

		_ -> 
			error
	end.

check_array_items([], _, Errors, _Parent) ->
	Errors;

check_array_items([Item | Rest], ItemsSchema, Errors, Parent) ->
	Errors2 = jvalidator_validator:validate(Item, ItemsSchema, Errors, Parent),
	check_array_items(Rest, ItemsSchema, Errors2, Parent).