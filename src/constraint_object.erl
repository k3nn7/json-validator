-module(constraint_object).

-export([check/4]).

check(Element, Schema, Errors, Parent) ->
	{struct, Properties} = proplists:get_value(<<"properties">>, Schema),
	MyName = proplists:get_value(<<"id">>, Schema),

	io:format("~nChecking OBJECT: ~p with properties ~p~n", [MyName, Properties]),	

	case Element of
		{struct, ObjProps} ->
			check_obj_properties(ObjProps, Properties, Errors, 
								 <<Parent/binary, MyName/binary, ".">>);

		_ -> 
			io:format("WEIRD ERROR!!!!!!!!!!~n"),
			error
	end.

check_obj_properties(_, [], Errors, Parent) ->
	Errors;

check_obj_properties(ObjProps, [{Name, {struct, Details}} | Rest], Errors, Parent) ->
	HasProperty = proplists:is_defined(Name, ObjProps),
	IsRequired  = proplists:get_value(<<"required">>, Details, false),

	io:format("Checking object property ~p for ~p~n", [Name, ObjProps]),
	io:format("HasProperty: ~p, IsRequired: ~p~n", [HasProperty, IsRequired]),

	if
		HasProperty ->
			io:format("HASPROPERTY~n"),
			Errors2 = jvalidator_validator:check(proplists:get_value(Name, ObjProps), 
							{struct, Details}, Errors, Parent);

		(not HasProperty) and IsRequired ->
			io:format("SHOULD BE ERROR~n"),
			Errors2 = [list_to_binary(io_lib:format("Property \"~s~s\" is not defined", [Parent, Name])) | Errors];
			
		true ->
			io:format("SOMETHING ELSE~n"),
			Errors2 = Errors
	end,
	check_obj_properties(ObjProps, Rest, Errors2, Parent).