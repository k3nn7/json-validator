-module(jvalidator_builder).
-export([build_schema/2]).

build_schema(Schema, Dir) ->
	case jsonx:decode(Schema) of
		{error, _, _} -> throw(invalid);

		{Schema2} ->
			Builded = build(Schema2, Dir),
			jsonx:encode(Builded)
	end.

build(Schema, Dir) ->
	case proplists:get_value(<<"extends">>, Schema) of
		undefined ->
			Schema2 = Schema;
		Extends ->
			Schema2 = extend(Schema, Extends, Dir)
	end,
	% Set default type
	% Validate properties
	case proplists:get_value(<<"properties">>, Schema2) of
		undefined ->
			Schema3 = Schema2;
		{Properties} ->
			Properties2 = process_properties(Properties, Dir, []),
			Schema22 = proplists:delete(<<"properties">>, Schema2),
			Schema3 = [{<<"properties">>, {Properties2}} | Schema22]
	end,
	case proplists:get_value(<<"items">>, Schema3) of
		undefined ->
			Schema4 = Schema3;
		{Items} ->
			Items2 = process_items(Items, Dir),
			Schema33 = proplists:delete(<<"items">>, Schema3),
			Schema4 = [{<<"items">>, {Items2}} | Schema33]
	end.

extend(Schema, [], Dir) ->
	Schema;
extend(Schema, [Extends | Rest], Dir) ->
	Schema2 = extend(Schema, Extends, Dir),
	extend(Schema, Rest, Dir); 
	
extend(Schema, Extends, Dir) ->
	try jvalidator_provider:get_schema(Extends) of
		ESchema ->
			{ESchema2} = jsonx:decode(ESchema),
			Schema2 = proplists:delete(<<"extends">>, Schema),
			Schema3 = extend_fields(Schema2, ESchema2),
			Schema4 = extend_properties(Schema3, ESchema2)
	catch
		throw:_ -> throw(invalid)
	end.

extend_fields(Schema, []) ->
	Schema;
extend_fields(Schema, [{Key, _} | Rest]) when Key == <<"properties">> ->
	extend_fields(Schema, Rest);
extend_fields(Schema, [{Key, Value} | Rest])  ->
	case proplists:get_value(Key, Schema) of
		undefined ->
			Schema2 = [{Key, Value} | Schema],
			extend_fields(Schema2, Rest);
		_ ->
			extend_fields(Schema, Rest)
	end.

extend_property_field(Properties, []) ->
	Properties;
extend_property_field(Properties, [{Key, EValue} | Rest]) ->
	case proplists:get_value(Key, Properties) of
		undefined ->
			Properties2 = [{Key, EValue} | Properties],
			extend_property_field(Properties2, Rest);
		{Value} ->
			{EValue2} = EValue,
			Value2 = extend_property_field(Value, EValue2),
			Properties2 = proplists:delete(Key, Properties),
			Properties3 = [{Key, {Value2}} | Properties2],
			extend_property_field(Properties3, Rest)
	end.

extend_properties(Schema, ESchema) ->
	case proplists:get_value(<<"properties">>, ESchema) of
		undefined -> Schema;
		{EProperties} ->
			case proplists:get_value(<<"properties">>, Schema) of
				undefined ->
					[{<<"properties">>, {EProperties}} | Schema];
				{Properties} ->
					Properties2 = extend_property_field(Properties, EProperties),
					Schema2 = proplists:delete(<<"properties">>, Schema),
					[{<<"properties">>, {Properties2}} | Schema2]
			end
	end.

process_properties([], _, Acc) ->
	Acc;
process_properties([{Key, {Value}} | Rest], Dir, Acc) ->
	Value2 = build(Value, Dir),
	process_properties(Rest, Dir, [{Key, {Value2}} | Acc]).	

process_items(Schema, Dir) ->
	build(Schema, Dir).
