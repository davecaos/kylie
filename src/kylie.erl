  -module(kylie).

-author("David Cesar Hernan Cao <david.c.h.cao@gmail.com>").
-github("https://github.com/davecaos").
-license("MIT").

-export([ add/1
        , start/0
        , stop/0
        , delete/1
        , query/1
        , get_result/2
        , build_gremblin/1
        , build_gremblin_human_readable/1
        ]).

-define(WRITE_URI,          "/api/v1/write").
-define(WRITE_NQUAD_URI,    "/api/v1/write/file/nquad").
-define(DELETE_URI,         "/api/v1/delete").
-define(QUERY_GREMBLIN_URI, "/api/v1/query/gremlin").

-spec start() -> ok.
start() -> ok.

-spec stop() -> ok.
stop() ->  ok.

-spec add(squad:squad4()) -> tuple().
add(Squad) ->
  JsonBody = jsx:encode([Squad]),
  case cayley_http_call(?WRITE_URI, JsonBody) of
    {200, _Response} -> ok;
    {StatusErrorCode, Response} -> {StatusErrorCode, Response}
  end.

-spec delete(squad:squad4()) -> tuple().
delete(Squad) ->
  JsonBody = jsx:encode([Squad]),
  case cayley_http_call(?DELETE_URI, JsonBody) of
    {200, _Response} -> ok;
    {StatusErrorCode, Response} -> {StatusErrorCode, Response}
  end.

-spec get_result(binary(), binary()) -> {ok | error, list()}.
get_result(Subject, Predicate) ->
  Query = io_lib:format(<<"g.V('~s').Out('~s').All()">>, [Subject, Predicate]),
  query(Query).

filter_query_result(#{<<"result">> := null})    -> [];
filter_query_result(#{<<"result">> := Results}) ->
  Fun = 
    fun(#{<<"id">> := Contact}) -> 
      Contact 
    end,
  lists:map(Fun, Results).

-spec build_gremblin_human_readable(list()) -> binary().
build_gremblin_human_readable(PropLisps) ->
 erlang:iolist_to_binary(build_gremblin(PropLisps)).

-spec build_gremblin(list()) -> binary().
build_gremblin(PropLisps) ->
 lists:map(fun build_query/1, PropLisps).

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

-spec query(binary()) -> {ok | error, list()}.
query(Query) ->
  get_cayley_error(cayley_http_call(?QUERY_GREMBLIN_URI, Query)).

-spec cayley_http_call(string(), iodata()) -> map().
cayley_http_call(Uri, Body) ->
  DefaultPort = {ok, "64210"},
  DefaultHost = {ok, "127.0.0.1"},
  DefaultTimeOut = {ok, 3000},
  {ok, Port} = application:get_env(kylie, port, DefaultPort),
  {ok, Host}  = application:get_env(kylie, host, DefaultHost),
  {ok, Timeout} = application:get_env(kylie, timeout, DefaultTimeOut),
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  List    = [Host, <<":">>, Port, Uri],
  URL     = iolist_to_binary(List),
  Opts = [{timeout, Timeout}],
  {ok, StatusCode, _, ClientRef} = hackney:request(post, URL, Headers, Body, Opts),
  {ok, ResponseBody} = hackney:body(ClientRef),
  {StatusCode, ResponseBody}.

get_cayley_error({_StatusCode = 200, JsonResponse}) ->
  MapResponse = jsx:decode(JsonResponse, [return_maps]),
  {ok, filter_query_result(MapResponse)};
get_cayley_error({StatusErrorCode, ErrorDescription}) ->
  {error, {StatusErrorCode, ErrorDescription}}.

