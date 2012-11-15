-module(constraint_object).

-export([check/4]).

check(Element, Schema, Errors, Parent) ->
	{struct, Properties} = proplists:get_value(<<"properties">>, Schema),
	MyName = proplists:get_value(<<"id">>, Schema),

	case Element of
		{struct, ObjProps} ->
			check_obj_properties(ObjProps, Properties, Errors, 
								 <<Parent/binary, MyName/binary, ".">>);

		_ -> 
			error
	end.

check_obj_properties(_, [], Errors, Parent) ->
	Errors;

check_obj_properties(ObjProps, [{Name, {struct, Details}} | Rest], Errors, Parent) ->
	HasProperty = proplists:is_defined(Name, ObjProps),
	IsRequired  = proplists:get_value(<<"required">>, Details, false),

	if
		HasProperty ->
			Errors2 = jvalidator_validator:check(proplists:get_value(Name, ObjProps), 
							{struct, Details}, Errors, Parent);

		(not HasProperty) and IsRequired ->
			Errors2 = [list_to_binary(io_lib:format("Property \"~s~s\" is not defined", [Parent, Name])) | Errors];
			
		true -> Errors2 = Errors
	end,
	check_obj_properties(ObjProps, Rest, Errors2, Parent).