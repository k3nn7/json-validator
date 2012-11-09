-module(jvalidator_helper).

-export([check/3]).

-export([test/0]).



%%
%% Check entry point
%%
check(Json, {struct, Schema}, Errors) ->
	io:format("Start!~n"),
	case proplists:get_value(<<"type">>, Schema) of
		<<"object">> ->
			io:format("Begin to check object...~n"),
			check_object(Json, 
						 proplists:get_value(<<"properties">>, Schema),
						 Errors);

		<<"string">> ->
			io:format("Begin to check string...~n"),
			check_string(Json, Schema, Errors);

		<<"number">> ->
			io:format("Begin to check number...~n"),
			check_number(Json, Schema, Errors);

		Type -> 
			io:format("Error, type ~p...~n", [Type])
	end.


check_string(Object, Properties, Errors) ->
	Id = proplists:get_value(<<"id">>, Properties),
	IsBinary = is_binary(Object),

	if 
		IsBinary -> [];
		true ->
			list_to_binary(io_lib:format("Object \"~s\" must be a string", [Id]))
	end.

check_number(Object, Properties, Errors) ->
	Id = proplists:get_value(<<"id">>, Properties),
	IsInteger = is_integer(Object),

	if 
		IsInteger -> [];
		true ->
			list_to_binary(io_lib:format("Object \"~s\" must be an integer", [Id]))
	end.


check_object(Object, {struct, Properties}, Errors) ->
	io:format("Checking object... ~p with properties ~p~n~n", [Object, Properties]),

	case Object of
		{struct, ObjProps} ->
			check_obj_properties(ObjProps, Properties, Errors);

		_ -> error
	end.

check_obj_properties(_, [], Errors) ->
	Errors;

check_obj_properties(ObjProps, [{Name, {struct, Details}} | Rest], Errors) ->
	HasProperty = proplists:is_defined(Name, ObjProps),
	IsRequired  = proplists:get_value(<<"required">>, Details, false),

	if
		HasProperty ->
			Errors2 = check(proplists:get_value(Name, ObjProps), 
							{struct, Details}, Errors);

		(not HasProperty) and IsRequired ->
			Errors2 = [io_lib:format("Property ~s is not defined", [Name]) | Errors];
			
		true ->
			Errors2 = Errors
	end,
	check_obj_properties(ObjProps, Rest, Errors2).


test() ->
	Json = {struct, [
					 {<<"name">>, <<"lukasz">>},
					 {<<"age">>, 10}
					]},

	% Schema fields
	NameProp = {struct, [
						 {<<"id">>, <<"name">>},
						 {<<"required">>, true},
						 {<<"type">>, <<"string">>}
						]},

	AgeProp = {struct, [
						 {<<"id">>, <<"age">>},
						 {<<"required">>, false},
						 {<<"type">>, <<"number">>}
						]},

	% All props
	Properties = {struct, [
						   {<<"name">>, NameProp},
						   {<<"age">>, AgeProp}
						  ]},

	% Full schema
	Schema = {struct, [
					   {<<"type">>, <<"object">>},
		   	   		   {<<"properties">>, Properties}
					  ]},

	check(Json, Schema, []).