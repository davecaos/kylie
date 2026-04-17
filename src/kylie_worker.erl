-module(kylie_worker).


-behaviour(gen_server).

-export([init/1, 
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

-export([start_link/0,
         stop/0,
         add/1,
         delete/1,
         query/1
        ]).

-define(WRITE_URI,             <<"/api/v1/write">>).
-define(WRITE_NQUAD_URI,       <<"/api/v1/write/file/nquad">>).
-define(DELETE_URI,            <<"/api/v1/delete">>).
%% Cayley renamed /api/v1/query/gremlin → /api/v1/query/gizmo.
%% Default to gizmo (current Cayley); override with {query_path, <<...>>}
%% in sys.config for older servers.
-define(DEFAULT_QUERY_URI,     <<"/api/v1/query/gizmo">>).

-type error() :: {error, {integer(), binary()} | {http, term()}}.
-type state() :: #{base_url   := binary(),
                   query_path := binary(),
                   options    := list(),
                   header     := list()}.

-spec start_link() -> {ok, pid()}.
start_link() ->
  Workers = application:get_env(kylie, workers_amount, 10),
  wpool:start_pool(
    kylie_worker_pool,
    [{workers, Workers}, {worker, {kylie_worker, []}}]
  ).

-spec stop() -> ok.
stop() ->
  wpool:stop_pool(kylie_worker_pool),
  ok.

-spec add(squad:squad()) -> ok | error().
add(Squad) ->
  wpool:call(kylie_worker_pool, {add, Squad}).

-spec delete(squad:squad()) -> ok | error().
delete(Squad) ->
  wpool:call(kylie_worker_pool, {delete, Squad}).

-spec query(iodata()) -> [map()] | error().
query(Query) ->
  wpool:call(kylie_worker_pool, {query, Query}).

-spec init(term()) -> {ok, state()}.
init(_Args) ->
  Port      = application:get_env(kylie, port,       <<"64210">>),
  Host      = application:get_env(kylie, host,       <<"127.0.0.1">>),
  Timeout   = application:get_env(kylie, timeout,    3000),
  QueryPath = application:get_env(kylie, query_path, ?DEFAULT_QUERY_URI),
  Headers   = [{<<"Content-Type">>, <<"application/json">>}],
  BaseUrl   = <<"http://", Host/binary, ":", Port/binary>>,
  Opts      = [{recv_timeout, Timeout}, {connect_timeout, Timeout}, with_body],
  {ok, #{base_url   => BaseUrl,
         query_path => QueryPath,
         options    => Opts,
         header     => Headers}}.

-spec handle_call(term(), _, state()) ->
  {reply, term(), state()} | {stop, normal, ok, state()}.
handle_call({add, Squad}, _From, State) ->
  {reply, add(Squad, State), State};
handle_call({delete, Squad}, _From, State) ->
  {reply, delete(Squad, State), State};
handle_call({query, Query}, _From, State) ->
  {reply, query(Query, State), State};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
  {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {stop, normal, state()}.
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_, State) ->
  {noreply, State}.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec add(squad:squad4(), state()) -> ok | error().
add(Squad, #{base_url := BaseUrl} = State) ->
  Url = <<BaseUrl/binary, ?WRITE_URI/binary>>,
  run_write(Url, Squad, State).

-spec delete(squad:squad4(), state()) -> ok | error().
delete(Squad, #{base_url := BaseUrl} = State) ->
  Url = <<BaseUrl/binary, ?DELETE_URI/binary>>,
  run_write(Url, Squad, State).

-spec query(iodata(), state()) -> [map()] | error().
query(Query, #{base_url := BaseUrl, query_path := QueryPath} = State) ->
  Url = <<BaseUrl/binary, QueryPath/binary>>,
  run_query(Url, Query, State).

-spec run_write(binary(), squad:squad4(), state()) -> ok | error().
run_write(Url, Squad, State) ->
  JsonBody = jsx:encode([Squad]),
  case cayley_http_call(Url, JsonBody, State) of
    {ok, _Response} -> ok;
    {error, _} = Err -> Err
  end.

-spec run_query(binary(), iodata(), state()) -> [map()] | error().
run_query(Url, Query, State) ->
  case cayley_http_call(Url, Query, State) of
    {ok, JsonResponse} ->
      RawMap = jsx:decode(JsonResponse, [return_maps]),
      filter_query_result(RawMap);
    {error, _} = Err -> Err
  end.

-spec cayley_http_call(binary(), iodata(), state()) ->
  {ok, binary()} | error().
cayley_http_call(URL, Body, #{options := Opts, header := Headers}) ->
  case hackney:request(post, URL, Headers, Body, Opts) of
    {ok, 200, _, ResponseBody} ->
      {ok, ResponseBody};
    {ok, StatusCode, _, ResponseBody} ->
      {error, {StatusCode, ResponseBody}};
    {error, Reason} ->
      {error, {http, Reason}}
  end.

-spec filter_query_result(map()) -> [map()].
filter_query_result(#{<<"result">> := null})    -> [];
filter_query_result(#{<<"result">> := Results}) -> Results;
filter_query_result(_Other)                     -> [].
