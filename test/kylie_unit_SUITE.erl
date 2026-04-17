-module(kylie_unit_SUITE).

-author("David Cesar Hernan Cao <david.c.h.cao@gmail.com>").
-license("MIT").

-include_lib("common_test/include/ct.hrl").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ squad_new_3/1
        , squad_new_4/1
        , squad_getters_setters/1
        , squad_nquads_file/1

        , builder_graph_vertex/1
        , builder_out/1
        , builder_in/1
        , builder_has/1
        , builder_save/1
        , builder_limit_and_skip/1
        , builder_all_terminal/1
        , builder_full_pipeline/1

        , escape_single_quote/1
        , escape_backslash/1
        , escape_mixed/1
        , injection_single_quote_is_neutralised/1
        , injection_break_out_attempt_is_neutralised/1

        , get_limit_rejects_non_integer/1
        , skip_rejects_non_integer/1
        ]).

-type config() :: [{atom(), term()}].

%% These tests run entirely in-process, no network, no application start.

-spec all() -> [atom()].
all() ->
  [ squad_new_3
  , squad_new_4
  , squad_getters_setters
  , squad_nquads_file

  , builder_graph_vertex
  , builder_out
  , builder_in
  , builder_has
  , builder_save
  , builder_limit_and_skip
  , builder_all_terminal
  , builder_full_pipeline

  , escape_single_quote
  , escape_backslash
  , escape_mixed
  , injection_single_quote_is_neutralised
  , injection_break_out_attempt_is_neutralised

  , get_limit_rejects_non_integer
  , skip_rejects_non_integer
  ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% squad module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

squad_new_3(_Config) ->
  S = squad:new(<<"Kylie">>, <<"is">>, <<"singer">>),
  <<"Kylie">>  = squad:subject(S),
  <<"is">>     = squad:predicate(S),
  <<"singer">> = squad:object(S),
  ok.

squad_new_4(_Config) ->
  S = squad:new(<<"Kylie">>, <<"is">>, <<"singer">>, <<"label-a">>),
  <<"label-a">> = squad:label(S),
  ok.

squad_getters_setters(_Config) ->
  S0 = squad:new(<<"a">>, <<"b">>, <<"c">>),
  S1 = squad:subject(S0, <<"Kylie">>),
  S2 = squad:predicate(S1, <<"is">>),
  S3 = squad:object(S2, <<"singer">>),
  S4 = squad:label(S3, <<"lbl">>),
  <<"Kylie">>  = squad:subject(S4),
  <<"is">>     = squad:predicate(S4),
  <<"singer">> = squad:object(S4),
  <<"lbl">>    = squad:label(S4),
  ok.

squad_nquads_file(_Config) ->
  S1 = squad:new(<<"Kylie">>, <<"is">>, <<"singer">>, <<"lbl">>),
  S2 = squad:new(<<"Kylie">>, <<"is">>, <<"actress">>, <<"lbl">>),
  Bin = squad:nquads_file([S1, S2]),
  true = is_binary(Bin),
  %% One line per squad, terminated with ".\n".
  Lines = binary:split(Bin, <<"\n">>, [global, trim_all]),
  2 = length(Lines),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Query builder — positive shape
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

builder_graph_vertex(_Config) ->
  <<"g.V('Kylie').">> =
    kylie:build_gremblin_human_readable([{graph_vertex, <<"Kylie">>}]),
  ok.

builder_out(_Config) ->
  <<"Out('is').">> =
    kylie:build_gremblin_human_readable([{out, <<"is">>}]),
  ok.

builder_in(_Config) ->
  <<"In('is').">> =
    kylie:build_gremblin_human_readable([{in, <<"is">>}]),
  ok.

builder_has(_Config) ->
  <<"Has('is','singer').">> =
    kylie:build_gremblin_human_readable([{has, [<<"is">>, <<"singer">>]}]),
  ok.

builder_save(_Config) ->
  <<"Save('is','role').">> =
    kylie:build_gremblin_human_readable([{save, [<<"is">>, <<"role">>]}]),
  ok.

builder_limit_and_skip(_Config) ->
  %% Numerics must be emitted without single quotes — Gizmo expects integers.
  <<"GetLimit(10).Skip(5).">> =
    kylie:build_gremblin_human_readable([{get_limit, 10}, {skip, 5}]),
  ok.

builder_all_terminal(_Config) ->
  <<"All()">> = kylie:build_gremblin_human_readable([all]),
  ok.

builder_full_pipeline(_Config) ->
  Q = kylie:build_gremblin_human_readable(
        [{graph_vertex, <<"Kylie">>}, {out, <<"recorded">>},
         {out, <<"incluided">>}, all]),
  <<"g.V('Kylie').Out('recorded').Out('incluided').All()">> = Q,
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Query builder — escape helper
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

escape_single_quote(_Config) ->
  <<"g.V('it\\'s').">> =
    kylie:build_gremblin_human_readable([{graph_vertex, <<"it's">>}]),
  ok.

escape_backslash(_Config) ->
  <<"g.V('a\\\\b').">> =
    kylie:build_gremblin_human_readable([{graph_vertex, <<"a\\b">>}]),
  ok.

escape_mixed(_Config) ->
  <<"g.V('a\\\\b\\'c').">> =
    kylie:build_gremblin_human_readable([{graph_vertex, <<"a\\b'c">>}]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gizmo/Gremlin injection defence
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

injection_single_quote_is_neutralised(_Config) ->
  %% Raw single quote must NOT appear unescaped inside the argument.
  Hostile = <<"x').All();g.V('y">>,
  Q = kylie:build_gremblin_human_readable([{graph_vertex, Hostile}, all]),
  %% Sanity: no unescaped closing quote followed by dot-call inside the arg.
  nomatch = binary:match(Q, <<"').All();g.V('">>),
  %% The escaped sequence must be present.
  {_, _} = binary:match(Q, <<"\\'">>),
  ok.

injection_break_out_attempt_is_neutralised(_Config) ->
  %% A predicate trying to terminate the Out('...') call and inject code.
  Hostile = <<"is');delete;//">>,
  Q = kylie:build_gremblin_human_readable([{out, Hostile}]),
  %% Every single-quote inside the argument must be preceded by a
  %% backslash — that's what prevents it from closing the string literal
  %% at the JS parser level, no matter what byte sequence follows.
  {_, _} = binary:match(Q, <<"\\');delete">>),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type enforcement for numeric clauses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_limit_rejects_non_integer(_Config) ->
  %% GetLimit must be integer-only — a binary must crash, never be quoted.
  {'EXIT', _} = (catch kylie:build_gremblin_human_readable(
                         [{get_limit, <<"10">>}])),
  ok.

skip_rejects_non_integer(_Config) ->
  {'EXIT', _} = (catch kylie:build_gremblin_human_readable(
                         [{skip, <<"5">>}])),
  ok.
