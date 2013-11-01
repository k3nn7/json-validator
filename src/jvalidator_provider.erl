-module(jvalidator_provider).

-export([get_raw_schema/1, get_schema/1]).

get_schema(Filename) ->
	Rawschema = get_raw_schema(Filename),
	Dir = filename:dirname(Filename),
	jvalidator_builder:build_schema(Rawschema, Dir).

get_raw_schema(Filename) ->
	Path = <<"webroot/", Filename/binary>>,
	io:format("Get schema ~p~n", [Path]),
	case file:read_file(Path) of
		{ok, Schema} ->
			case jsonx:decode(Schema) of
				{error, _, _} -> throw(invalid);
				_ -> Schema
			end;
		_ ->
			throw(notfound) 
	end.
