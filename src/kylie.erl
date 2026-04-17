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

-type error() :: kylie_worker:error().
-type proplisp() :: [term()].

-export_type([error/0, proplisp/0]).


%% application
%% @doc Starts the application
-spec start() -> ok.
start() ->
  {ok, _Started} = application:ensure_all_started(kylie),
  ok.

%% @doc Stops the application
-spec stop() -> ok.
stop() ->
  application:stop(kylie).


-spec add(squad:squad4()) -> ok | error().
add(Squad) ->
  kylie_worker:add(Squad).

-spec query(iodata()) -> [map()] | error().
query(Query) ->
  kylie_worker:query(Query).

-spec delete(squad:squad4()) -> ok | error().
delete(Squad) ->
  kylie_worker:delete(Squad).

-spec get_result(binary(), binary()) -> [binary()] | error().
get_result(Subject, Predicate) ->
  PropLispQuery = [{graph_vertex, Subject}, {out, Predicate}, all],
  GremblinQuery = build_gremblin_human_readable(PropLispQuery),
  case query(GremblinQuery) of
    Results when is_list(Results) ->
      [Id || #{<<"id">> := Id} <- Results];
    {error, _} = Err ->
      Err
  end.

-spec build_gremblin_human_readable(proplisp()) -> binary().
build_gremblin_human_readable(PropLisps) ->
  erlang:iolist_to_binary(build_gremblin(PropLisps)).

-spec build_gremblin(proplisp()) -> iolist().
build_gremblin(PropLisps) ->
  lists:map(fun build_query/1, PropLisps).

%% @doc Escape single-quote and backslash in user-supplied strings so they
%% cannot break out of the surrounding '...' in the generated Gizmo query.
-spec escape(iodata() | integer()) -> binary().
escape(Int) when is_integer(Int) ->
  integer_to_binary(Int);
escape(IoData) ->
  Bin = iolist_to_binary(IoData),
  escape_bin(Bin, <<>>).

-spec escape_bin(binary(), binary()) -> binary().
escape_bin(<<>>, Acc) -> Acc;
escape_bin(<<$\\, Rest/binary>>, Acc) -> escape_bin(Rest, <<Acc/binary, "\\\\">>);
escape_bin(<<$', Rest/binary>>, Acc) -> escape_bin(Rest, <<Acc/binary, "\\'">>);
escape_bin(<<C, Rest/binary>>, Acc)  -> escape_bin(Rest, <<Acc/binary, C>>).

build_query({in, In}) ->
  <<"In('", (escape(In))/binary, "').">>;
build_query({out, Out}) ->
  <<"Out('", (escape(Out))/binary, "').">>;
build_query({graph_vertex, V}) ->
  <<"g.V('", (escape(V))/binary, "').">>;
build_query({graph_morphism, M}) ->
  <<"g.M('", (escape(M))/binary, "').">>;
build_query({graph_emit, D}) ->
  <<"g.Emit('", (escape(D))/binary, "').">>;
build_query({has, [Predicate, Object]}) ->
  <<"Has('", (escape(Predicate))/binary, "','", (escape(Object))/binary, "').">>;
build_query({get_limit, Limit}) when is_integer(Limit) ->
  <<"GetLimit(", (integer_to_binary(Limit))/binary, ").">>;
build_query({skip, Skip}) when is_integer(Skip) ->
  <<"Skip(", (integer_to_binary(Skip))/binary, ").">>;
build_query({follow, F}) ->
  <<"Follow('", (escape(F))/binary, "').">>;
build_query({followr, F}) ->
  <<"FollowR('", (escape(F))/binary, "').">>;
build_query({save, [Predicate, Tag]}) ->
  <<"Save('", (escape(Predicate))/binary, "','", (escape(Tag))/binary, "').">>;
build_query({intersect, Q}) ->
  <<"Intersect('", (escape(Q))/binary, "').">>;
build_query({union, Q}) ->
  <<"Union('", (escape(Q))/binary, "').">>;
build_query({except, E}) ->
  <<"Except('", (escape(E))/binary, "').">>;
build_query(all) ->
  <<"All()">>.
