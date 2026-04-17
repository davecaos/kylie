-module(kylie_SUITE).

-author("David Cesar Hernan Cao <david.c.h.cao@gmail.com>").
-license("MIT").

-include_lib("common_test/include/ct.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ prop_lisps/1
        , delete/1
        , error_on_unreachable_cayley/1
        ]).

-type config() :: [{atom(), term()}].

%% Integration tests — require a running Cayley on 127.0.0.1:64210.
%% Run via: `rebar3 ct --suite=test/kylie_SUITE`.
%% See README.md ("Integration tests") for the Docker one-liner.
%%
%% If Cayley is not reachable the whole suite is skipped instead of
%% failing noisily — unit tests still cover the pure logic.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  [ prop_lisps
  , delete
  , error_on_unreachable_cayley
  ].

-spec init_per_suite(config()) -> config() | {skip, term()}.
init_per_suite(Config) ->
  {ok, _Apps} = application:ensure_all_started(kylie),
  case cayley_is_up() of
    true  -> Config;
    false ->
      application:stop(kylie),
      {skip, "Cayley is not reachable at 127.0.0.1:64210 — "
             "see README.md > Integration tests for the Docker one-liner."}
  end.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  application:stop(kylie),
  Config.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TC, Config) ->
  Config.

-spec end_per_testcase(atom(), config()) -> config().
end_per_testcase(_TC, Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec cayley_is_up() -> boolean().
cayley_is_up() ->
  _ = application:ensure_all_started(hackney),
  case hackney:request(get, <<"http://127.0.0.1:64210/">>,
                       [], <<>>,
                       [{connect_timeout, 500}, with_body]) of
    {ok, _Status, _H, _B} -> true;
    {error, _}            -> false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec prop_lisps(config()) -> ok.
prop_lisps(_Config) ->
  Squads =
    [ squad:new(<<"Kylie">>, <<"is">>, <<"singer">>)
    , squad:new(<<"Kylie">>, <<"is">>, <<"songwriter">>)
    , squad:new(<<"Kylie">>, <<"is">>, <<"model">>)
    , squad:new(<<"Kylie">>, <<"is">>, <<"author">>)
    , squad:new(<<"Kylie">>, <<"is">>, <<"actress">>)
    ],
  ok = lists:foreach(fun kylie:add/1, Squads),

  Results = kylie:get_result(<<"Kylie">>, <<"is">>),
  [ <<"actress">>
  , <<"author">>
  , <<"model">>
  , <<"singer">>
  , <<"songwriter">>
  ] = lists:sort(Results),

  ok = lists:foreach(fun kylie:delete/1, Squads),
  [] = kylie:get_result(<<"Kylie">>, <<"is">>),
  ok.

-spec delete(config()) -> ok.
delete(_Config) ->
  [Kylie, Enjoy, Rhythm, Fever, KylieMinogue] = Squads =
    [ squad:new(<<"Kylie">>, <<"recorded">>, <<"Kylie">>)
    , squad:new(<<"Kylie">>, <<"recorded">>, <<"Enjoy Yourself">>)
    , squad:new(<<"Kylie">>, <<"recorded">>, <<"Rhythm of Love">>)
    , squad:new(<<"Kylie">>, <<"recorded">>, <<"Fever">>)
    , squad:new(<<"Kylie">>, <<"recorded">>, <<"Kylie Minogue">>)
    ],
  ok = lists:foreach(fun kylie:add/1, Squads),

  [ <<"Enjoy Yourself">>
  , <<"Fever">>
  , <<"Kylie">>
  , <<"Kylie Minogue">>
  , <<"Rhythm of Love">>
  ] = lists:sort(kylie:get_result(<<"Kylie">>, <<"recorded">>)),

  ok = kylie:delete(KylieMinogue),
  [ <<"Enjoy Yourself">>, <<"Fever">>, <<"Kylie">>, <<"Rhythm of Love">>
  ] = lists:sort(kylie:get_result(<<"Kylie">>, <<"recorded">>)),

  ok = kylie:delete(Enjoy),
  [<<"Fever">>, <<"Kylie">>, <<"Rhythm of Love">>] =
    lists:sort(kylie:get_result(<<"Kylie">>, <<"recorded">>)),

  ok = kylie:delete(Rhythm),
  [<<"Fever">>, <<"Kylie">>] =
    lists:sort(kylie:get_result(<<"Kylie">>, <<"recorded">>)),

  ok = kylie:delete(Fever),
  [<<"Kylie">>] =
    lists:sort(kylie:get_result(<<"Kylie">>, <<"recorded">>)),

  ok = kylie:delete(Kylie),
  [] = kylie:get_result(<<"Kylie">>, <<"recorded">>),
  ok.

-spec error_on_unreachable_cayley(config()) -> ok.
error_on_unreachable_cayley(_Config) ->
  %% Point one worker at a dead port and confirm we get a typed
  %% {error, {http, _}} back instead of a crash.
  application:set_env(kylie, port, <<"1">>),
  application:stop(kylie),
  {ok, _} = application:ensure_all_started(kylie),
  {error, {http, _}} =
    kylie:add(squad:new(<<"a">>, <<"b">>, <<"c">>)),
  %% Restore default port so end_per_suite is clean.
  application:set_env(kylie, port, <<"64210">>),
  application:stop(kylie),
  {ok, _} = application:ensure_all_started(kylie),
  ok.
