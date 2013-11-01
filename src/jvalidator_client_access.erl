-module(jvalidator_client_access).
-author('lukasz.lalik@gmail.com').

-export([start_link/0, loop/1]).

start_link() ->
	Loop = fun(Req) ->
				?MODULE:loop(Req)
		   end,

	{ok, Port} = application:get_env(jvalidator, listen_port),

	mochiweb_http:start_link([
							  {name, jvalidator_client_access},
							  {loop, Loop},
							  {port, Port}
							 ]).

loop(Req) ->
	"/" ++ Path = Req:get(path),
	Method = Req:get(method),
	Body   = Req:recv_body(),
	Query = Req:parse_qs(),

	case Method of
		Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			case string:tokens(Path, "/") of
				[] ->
					format_response(Req, "Json-Validator");

				_ ->
					Filename = list_to_binary(Path),
					
					try jvalidator_provider:get_schema(Filename) of
						Schema ->
							format_response(Req, Schema)
					catch
						throw:notfound ->
							Req:respond({404, [], <<"Schema not found">>});

						throw:invalid ->
							Req:respond({500, [], <<"Invalid schema">>})		
					end
			end;

		'POST' ->
			case string:tokens(Path, "/") of
				[] ->
					format_response(Req, "Json-Validator");

				_ ->
					case file:read_file("webroot/" ++ Path) of
						{ok, Data} ->
							Schema = mochijson2:decode(Data),
							Json = mochijson2:decode(Body),
							Result = jvalidator_validator:validate(Json, Schema, [], <<"">>),
							format_response(Req, mochijson2:encode(Result));

						{error, _} ->
							Req:not_found()
					end
			end
	end.

format_response(Req, Body) ->
	Req:ok({_ContentType = "text/plain; charset=UTF-8",
			_Headers = [
						{"Server", "Json-Validator"}
					   ],
			Body}).
