-module(jvalidator_validator).

-export([check/4]).

-export([test/0]).



%%
%% Check entry point
%%
check(Json, {struct, Schema}, Errors, Parent) ->
	io:format("Start!~n"),
	case proplists:get_value(<<"type">>, Schema) of
		<<"object">> ->
			constraint_object:check(Json, Schema, Errors, Parent);

		<<"array">> ->
			constraint_array:check(Json, Schema, Errors, Parent);

		<<"string">> ->
			constraint_string:check(Json, Schema, Errors, Parent);

		<<"number">> ->
			constraint_number:check(Json, Schema, Errors, Parent);

		<<"boolean">> ->
			constraint_boolean:check(Json, Schema, Errors, Parent);

		Type -> 
			io:format("Error, type ~p...~n", [Type])
	end.



test() ->
	Likes = {struct, [
					  {<<"id">>, 10},
					  {<<"name">>, 20}
					 ]},

	Json = {struct, [
					 {<<"name">>, <<"lukasz">>}
					 %{<<"likes">>, Likes}
					]},

	% Schema fields
	NameProp = {struct, [
						 {<<"id">>, <<"name">>},
						 {<<"required">>, true},
						 {<<"type">>, <<"string">>}
						]},

	IdProps = {struct, [
						{<<"id">>, <<"id">>},
						{<<"required">>, true},
						{<<"type">>, <<"number">>}
					   ]},

	LikesProp2 = {struct, [
						   {<<"id">>, IdProps},
						   {<<"name">>, NameProp}
						  ]},

	LikesProp = {struct, [
						 {<<"id">>, <<"likes">>},
						 {<<"required">>, true},
						 {<<"type">>, <<"object">>},
						 {<<"properties">>, LikesProp2}
						]},

	% All props
	Properties = {struct, [
						   {<<"name">>, NameProp},
						   {<<"likes">>, LikesProp}
						  ]},

	% Full schema
	Schema = {struct, [
					   {<<"type">>, <<"object">>},
					   {<<"id">>, <<"#">>},
		   	   		   {<<"properties">>, Properties}
					  ]},

	check(Json, Schema, [], <<"">>).