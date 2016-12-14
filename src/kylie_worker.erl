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

-define(WRITE_URI,          "/api/v1/write").
-define(WRITE_NQUAD_URI,    "/api/v1/write/file/nquad").
-define(DELETE_URI,         "/api/v1/delete").
-define(QUERY_GREMBLIN_URI, "/api/v1/query/gremlin").

-type error() :: {integer(), binary()}.
-type state() :: #{}.
-type json()  :: binary().

-spec start_link() -> ok.
start_link() -> 
  Workers = application:get_env(kylie, workers_amount, 10),
  wpool:start_pool(
    kylie_worker_pool,
    [{workers, Workers}, {worker, {kylie_worker, []}}]
  ).

-spec stop() -> ok.
stop() -> 
  wpool:stop().

add(Squad) ->
  wpool:call(kylie_worker_pool, {add, Squad}).

delete(Squad) ->
  wpool:call(kylie_worker_pool, {delete, Squad}).

query(Query) ->
  wpool:call(kylie_worker_pool, {query, Query}).

-spec init(term()) -> {ok, term()}.
init(_Args) ->
  DefaultPort = {ok, <<"64210">>},
  DefaultHost = {ok, <<"127.0.0.1">>},
  DefaultTimeOut = {ok, 3000},
  {ok, Port}   = application:get_env(kylie, port, DefaultPort),
  {ok, Host}   = application:get_env(kylie, host, DefaultHost),
  {ok, Timeout} = application:get_env(kylie, timeout, DefaultTimeOut),
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  BaseUrl    = <<Host/binary, <<":">>/binary, Port/binary>>,
  Opts = [{timeout, Timeout}],
  {ok, #{base_url => BaseUrl, options => Opts, header => Headers}}.

-spec handle_call(term(), _, state()) -> {atom(), tuple(), state()}.
handle_call({add, Squad}, _From, State) ->
  {reply, add(Squad, State), State};
handle_call({delete, Squad}, _From, State) ->
  {reply, delete(Squad, State), State};
handle_call({query, Query}, _From, State) ->
  {reply, query(Query, State), State};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

-spec handle_cast(atom(), state()) -> {atom(), atom(), state()}.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(atom(), state()) -> {atom(), state()}.
handle_info(_, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec terminate(atom(), state()) -> ok.
terminate(normal, _State) ->
  ok.

-spec add(squad:squad4()) -> ok | error().
add(Squad, #{base_url := BaseUrl} = State) ->
  Url = <<BaseUrl/binary,?WRITE_URI>>,
  run(Url, Squad, State).

-spec delete(squad:squad4()) -> ok | error().
delete(Squad, #{base_url := BaseUrl} = State) ->
  Url = <<BaseUrl/binary,?DELETE_URI>>,
  run(Url, Squad, State).

-spec query(binary()) -> json() | error().
query(Query, #{base_url := BaseUrl} = State) ->
  Url = <<BaseUrl/binary,?QUERY_GREMBLIN_URI>>,
  run_query(Url, Query, State).

-spec run(binary(), squad:squad4(), map()) -> ok | error().
run(Url, Squad, State) ->
  JsonBody = jsx:encode([Squad]),
  try cayley_http_call(Url, JsonBody, State) of
    {ok, _Response} -> ok;
    {error, {StatusErrorCode, ErrorDescription}} -> {error, {StatusErrorCode, ErrorDescription}}
  catch
    Error:Type -> {error, {Error, Type}}
  end.

-spec run_query(binary(), squad:squad4(), map()) -> map() | error().
run_query(Url, Query, State) ->
  try cayley_http_call(Url, Query, State) of
    {ok, JsonResponse} ->
      RawMap = jsx:decode(JsonResponse, [return_maps]),
      filter_query_result(RawMap);
    {error, {StatusErrorCode, ErrorDescription}} -> {error, {StatusErrorCode, ErrorDescription}}
  catch
    Error:Type -> {error, {Error, Type}}
  end.

-spec cayley_http_call(string(), iodata(), map()) -> map().
cayley_http_call(URL, Body, #{options := Opts, header := Headers}) ->
  {ok, StatusCode, _, ClientRef} = hackney:request(post, URL, Headers, Body, Opts),
  {ok, ResponseBody} = hackney:body(ClientRef),
  get_cayley_error({StatusCode, ResponseBody}).

get_cayley_error({_StatusCode = 200, JsonResponse}) ->
  {ok, JsonResponse};
get_cayley_error({StatusErrorCode, ErrorDescription}) ->
  {error, {StatusErrorCode, ErrorDescription}}.

filter_query_result(#{<<"result">> := null})    -> [];
filter_query_result(#{<<"result">> := Results}) ->
  Fun = 
    fun(#{<<"id">> := Contact}) -> 
      Contact 
    end,
  lists:map(Fun, Results).
