-module(kylie_worker_sup).

-behaviour(supervisor).

-export([start_link/0,
         stop/0]).

-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop() -> ok.
stop() ->
  ok.

%% supervisor.

init([]) ->
  KylieWorker =
    {kylie_worker, {kylie_worker, start_link, []},
     permanent, infinity, worker, [kylie_worker]
    },
  Children = [KylieWorker],
  {ok, {{one_for_one, 10, 10}, Children}}.