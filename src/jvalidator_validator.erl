-module(jvalidator_validator).

-export([validate/2, validate/4]).

validate(Json, Schema) ->
	validate(Json, Schema, [], <<"">>).

%%
%% Check entry point
%%
validate(Json, {struct, Schema}, Errors, Parent) ->
	case proplists:get_value(<<"type">>, Schema) of
		<<"object">> ->
			constraint_object:validate(Json, Schema, Errors, Parent);

		<<"array">> ->
			constraint_array:validate(Json, Schema, Errors, Parent);

		<<"string">> ->
			constraint_string:validate(Json, Schema, Errors, Parent);

		<<"number">> ->
			constraint_number:validate(Json, Schema, Errors, Parent);

		<<"boolean">> ->
			constraint_boolean:valiate(Json, Schema, Errors, Parent);

		Type -> 
			io:format("Error, type ~p...~n", [Type])
	end.