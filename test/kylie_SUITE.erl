
-module(kylie_SUITE).

-author("David Cesar Hernan Cao <david.c.h.cao@gmail.com>").
-github("https://github.com/davecaos").
-license("MIT").

-include_lib("common_test/include/ct.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ prop_lisps/1, delete/1]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  [ prop_lisps 
  , delete 
  ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, _Apps} = application:ensure_all_started(kylie),
  Config.

init_per_testcase(_, Config) ->
  kylie:start(),
  Config.

end_per_testcase(_, Config) ->
  kylie:stop(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported Tests Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


prop_lisps(_Config) ->
  Squads =
  [ squad:new(<<"Kylie">>, <<"is">>,<< "singer">>),
    squad:new(<<"Kylie">>, <<"is">>,<< "songwriter">>),
    squad:new(<<"Kylie">>, <<"is">>,<< "model">>),
    squad:new(<<"Kylie">>, <<"is">>,<< "author">>),
    squad:new(<<"Kylie">>, <<"is">>,<< "actress">>)
  ],
  ok = lists:foreach(fun kylie:add/1, Squads),
  %% The idea is build this query -> <<"g.V('Kylie').Out('is').All()">>
  PropLispQuery = [{graph_vertex, <<"Kylie">>}, {out, <<"is">>}, all],
  GremblinQuery = kylie:build_gremblin(PropLispQuery),
  Result  = kylie:query(GremblinQuery),
  [ <<"actress">>
  , <<"author">>
  , <<"model">>
  , <<"singer">>
  , <<"songwriter">>
  ] = lists:sort(Result),

  ok = lists:foreach(fun kylie:delete/1, Squads),
  [] = kylie:query(GremblinQuery),
  ok.


delete(_Config) ->
  Squads =
  [ Kylie  = squad:new(<<"Kylie">>, <<"recorded">>, <<"Kylie">>),
    Enjoy  = squad:new(<<"Kylie">>, <<"recorded">>, <<"Enjoy Yourself">>),
    Rhythm = squad:new(<<"Kylie">>, <<"recorded">>, <<"Rhythm of Love">>),
    Fever  = squad:new(<<"Kylie">>, <<"recorded">>, <<"Fever">>),
    KylieMinogue = squad:new(<<"Kylie">>, <<"recorded">>, <<"Kylie Minogue">>)
  ],
  ok = lists:foreach(fun kylie:add/1, Squads),
  %% The idea is build this query -> <<"g.V('Kylie').Out('recorded').All()">>
  Result  = kylie:get_result(<<"Kylie">>, <<"recorded">>),
  [<<"Enjoy Yourself">>
  ,<<"Fever">>
  ,<<"Kylie">>
  ,<<"Kylie Minogue">>
  ,<<"Rhythm of Love">>] = lists:sort(Result),

  kylie:delete(KylieMinogue),
  Result2  = kylie:get_result(<<"Kylie">>, <<"recorded">>),
  [<<"Enjoy Yourself">>
  ,<<"Fever">>
  ,<<"Kylie">>
  ,<<"Rhythm of Love">>
  ] = lists:sort(Result2),

  kylie:delete(Enjoy),
  Result3  = kylie:get_result(<<"Kylie">>, <<"recorded">>),
  [<<"Fever">>
  ,<<"Kylie">>
  ,<<"Rhythm of Love">>
  ] = lists:sort(Result3),

  kylie:delete(Rhythm),
  Result4 = kylie:get_result(<<"Kylie">>, <<"recorded">>),
  [<<"Fever">>
  ,<<"Kylie">>] = lists:sort(Result4),

  kylie:delete(Fever),
  Result5 = kylie:get_result(<<"Kylie">>, <<"recorded">>),
  [<<"Kylie">>] = lists:sort(Result5),

  kylie:delete(Kylie),
  []  = kylie:get_result(<<"Kylie">>, <<"recorded">>),
  ok.











