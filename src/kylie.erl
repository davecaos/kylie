-module(kylie).

-export([ start/0
	    , stop/0
	    , add/1
	    , delete/1
	    ]).

-define(WRITE_URI, "/api/v1/write").
-define(WRITE_NQUAD_URI, "/api/v1/write/file/nquad").
-define(DELETE_URI, "/api/v1/delete").
-define(QUERY_GREMBLIN_URI, "/api/v1/shape/gremlin").

start() ->
  {ok, _} = shotgun:start().

stop() ->
  ok = shotgun:stop().

add([_Head| _Tail] = Squads) ->
  JsonBody = jsx:encode(Squads),
  cayley_http_call(?WRITE_NQUAD_URI, JsonBody);
add(Squad) ->
  JsonBody = jsx:encode([Squad]),
  cayley_http_call(?WRITE_URI, JsonBody).

delete(Squad) ->
  JsonBody = jsx:encode([Squad]),
  cayley_http_call(?DELETE_URI, JsonBody).

-spec cayley_http_call(string(), iodata()) -> map().
cayley_http_call(Uri, Body) ->
  Port = application:get_env(kylie, port),
  Url  = application:get_env(kylie, host),
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  Opts = #{timeout => 30000},
  try 
    {ok, Pid} = shotgun:open(Url, Port),
    Response = shotgun:request(Pid, post, Uri, Headers, Body, Opts),
    shotgun:close(Pid),
    Response
   catch
    _:Exception ->
    	{error, Exception}
  end.