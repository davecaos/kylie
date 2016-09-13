  -module(kylie).

-author("David Cesar Hernan Cao <david.c.h.cao@gmail.com>").
-github("https://github.com/davecaos").
-license("MIT").

-export([ start/0
	      , stop/0
	      , add/1
	      , delete/1
	      , query/1
        , get_result/2
        , build_gremblin/1
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
  query(Query).

filter_query_result(#{<<"result">> := null}) -> [];
filter_query_result(#{<<"result">> := Results}) ->
  Fun = 
    fun(#{<<"id">> := Contact}) -> 
      Contact 
    end,
  lists:map(Fun, Results).

build_gremblin(PropLisps) ->
 lists:map( fun build_query/1, PropLisps).

build_query({in, In}) ->
  io_lib:format(<<"In('~s').">>, [build_query(In)]);
build_query({out, Out}) ->
  io_lib:format(<<"Out('~s').">>, [build_query(Out)]);
build_query({graph_vertex, GraphVertex}) ->
  io_lib:format(<<"g.V('~s').">>, [build_query(GraphVertex)]);
build_query({graph_morphism, GraphMorphism}) ->
  io_lib:format(<<"g.M('~s').">>, [build_query(GraphMorphism)]);
build_query({graph_emit, Data}) ->
  io_lib:format(<<"g.Emit('~s').">>, [build_query(Data)]);

build_query({has, [Predicate, Object]}) ->
  io_lib:format(<<"Has('~s','~s' ).">>, [build_query(Predicate), build_query(Object)]);
build_query({get_limit, Limit}) ->
  io_lib:format(<<"GetLimit(~s).">>, [build_query(Limit)]);
build_query({skip, Skip}) ->
  io_lib:format(<<"Skip('~s').">>, [build_query(Skip)]);
build_query({follow, Follow}) ->
  io_lib:format(<<"Follow('~s').">>, [build_query(Follow)]);
build_query({followr, FollowR}) ->
  io_lib:format(<<"FollowR('~s').">>, [build_query(FollowR)]);
build_query({save, [Predicate, Tag]}) ->
  io_lib:format(<<"Save('~s','~s').">>, [build_query(Predicate), build_query(Tag)]);
build_query({intersect, Query}) ->
  io_lib:format(<<"Intersect(~s).">>, [build_query(Query)]);
build_query({union, Query}) ->
  io_lib:format(<<"Union('~s').">>, [build_query(Query)]);
build_query({except, Except}) ->
  io_lib:format(<<"Except('~s').">>, [build_query(Except)]);
build_query(all) ->
  <<"All()">>;
build_query(Node) ->
  Node.


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


get_cayley_error({Result, #{body := JsonBody, status_code := 200}}) ->
  {Result, filter_query_result(jsx:decode(JsonBody, [return_maps]))};
get_cayley_error({Result, #{body := Description, status_code := Code}}) ->
  {Result, {Code, Description}}.

