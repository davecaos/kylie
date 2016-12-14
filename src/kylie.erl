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
        , build_gremblin_human_readable/1
        ]).

-type error() :: {integer(), binary()}.
-type json()  :: binary().


%% application
%% @doc Starts the application
-spec start() -> ok | {error, {already_started, ?MODULE}}.
start() ->
  {ok, _Started} = application:ensure_all_started(kylie).

%% @doc Stops the application
-spec stop() -> ok.
stop() ->
  application:stop(kylie).


-spec add(squad:squad4()) -> ok | error().
add(Squad) ->
  kylie_worker:add(Squad).

-spec query(iodata()) -> json().
query(Query) ->
  kylie_worker:query(Query).

-spec delete(squad:squad4()) -> ok | error().
delete(Squad) ->
  kylie_worker:delete(Squad).

-spec get_result(binary(), binary()) -> {ok | error, list()}.
get_result(Subject, Predicate) ->
  PropLispQuery = [{graph_vertex, Subject}, {out, Predicate}, all],
  GremblinQuery = build_gremblin_human_readable(PropLispQuery),
  query(GremblinQuery).

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
