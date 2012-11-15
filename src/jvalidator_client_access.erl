-module(jvalidator_client_access).
-author('lukasz.lalik@gmail.com').

-export([start_link/0, loop/1]).

start_link() ->
	Loop = fun(Req) ->
				?MODULE:loop(Req)
		   end,

	mochiweb_http:start_link([
							  {name, jvalidator_client_access},
							  {loop, Loop},
							  {port, 5000}
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
					Req:serve_file(Path, "webroot/")
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
							Result = jvalidator_validator:check(Json, Schema, [], <<"">>),
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
