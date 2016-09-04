-module(kylie).

-export([ start/0
	      , stop/0
	      , add/1
	      , delete/1
	      , query/1
        , get_result/2
	      ]).

-define(WRITE_URI,          "/api/v1/write").
-define(WRITE_NQUAD_URI,    "/api/v1/write/file/nquad").
-define(DELETE_URI,         "/api/v1/delete").
-define(QUERY_GREMBLIN_URI, "/api/v1/query/gremlin").

start() ->
 ok.

stop() ->
  ok.

add(Squad) ->
  JsonBody = jsx:encode([Squad]),
  cayley_http_call(?WRITE_URI, JsonBody).

delete(Squad) ->
  JsonBody = jsx:encode([Squad]),
  cayley_http_call(?DELETE_URI, JsonBody).

get_result(Subject, Predicate) ->
  Query = io_lib:format(<<"g.V('~s').Out('~s').All()">>, [Subject, Predicate]),
  {ok, {_Code, JsonResult}} = query(Query),
  ResultMap = jsx:decode(JsonResult, [return_maps]),
  parse_query_result(ResultMap).

parse_query_result(#{<<"result">> := null}) -> [];
parse_query_result(#{<<"result">> := Results}) ->
  Fun = 
    fun(#{<<"id">> := Contact}) -> 
      Contact 
    end,
  lists:map(Fun, Results).

query(Query) ->
  cayley_http_call(?QUERY_GREMBLIN_URI, Query).

-spec cayley_http_call(string(), iodata()) -> map().
cayley_http_call(Uri, Body) ->
  {ok, Port} = application:get_env(kylie, port),
  {ok, Url}  = application:get_env(kylie, host),
  {ok, Timeout} = application:get_env(kylie, timeout),
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  Opts = #{timeout => Timeout},
  try 
    {ok, Pid} = shotgun:open(Url, Port),
    Response  = shotgun:request(Pid, post, Uri, Headers, Body, Opts),
    shotgun:close(Pid),
    get_cayley_error(Response)
   catch
    _:Exception -> {error, Exception}
  end.

get_cayley_error({Result, #{body := Description, status_code := Code}}) ->
  {Result, {Code, Description}}.
