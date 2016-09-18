# Kylie (can't get you out of my graph data base)
[![Hex.pm](https://img.shields.io/hexpm/v/kylie.svg?style=flat-square)](https://hex.pm/packages/kylie)
[![Hex.pm](https://img.shields.io/hexpm/dt/kylie.svg?style=flat-square)](https://hex.pm/packages/kylie)
[![License](http://img.shields.io/hexpm/l/kylie.svg?style=flat)](https://hex.pm/packages/kylie)

Kylie is a blond and small Erlang client for Cayley graph data base

 [Cayley](https://github.com/cayleygraph/cayley/) is an open-source graph data base written in go

---------
Example:
```erlang
Eshell V7.0  (abort with ^G)
1> squad:new("Kylie", "is", "singer").
#{object => "singer",predicate => "is",subject => "Kylie"}

2> kylie:add(squad:new(<<"Kylie">>, <<"is">>,<< "singer">>)).
{200, <<"{\"result\": \"Successfully wrote 1 quads.\"}">>}
2>  kylie:add(squad:new(<<"Kylie">>, <<"is">>,<< "songwriter">>)).
{200, <<"{\"result\": \"Successfully wrote 1 quads.\"}">>}
3> kylie:add(squad:new(<<"Kylie">>, <<"is">>,<< "model">>)).
{200, <<"{\"result\": \"Successfully wrote 1 quads.\"}">>}

```

You can do a generic query with Subject and Predicate.
```erlang
4> kylie:get_result(<<"Kylie">>, <<"is">>).
{ok, [<<"model">>,<<"singer">>,<<"songwriter">>]}
```


You can also do a proplisp(?) and generate a query in the [Gremblin](http://gremlindocs.spmallette.documentup.com/) way.

The idea is build this query: 
```javascript
g.V('Kylie').Out('recorded').Out('incluided').All()"
```

```erlang
5> kylie:add(squad:new(<<"Kylie">>, <<"recorded">>, <<"Fever">>)).
{200, <<"{\"result\": \"Successfully wrote 1 quads.\"}">>}
6> kylie:add(squad:new(<<"Fever">>, <<"incluided">>, <<"Can't Get You Out of My Head">>)).
{200, <<"{\"result\": \"Successfully wrote 1 quads.\"}">>}
7> kylie:add(squad:new(<<"Fever">>, <<"incluided">>, <<"In Your Eyes">>)),
{200, <<"{\"result\": \"Successfully wrote 1 quads.\"}">>}

8>PropLispQuery = [{graph_vertex, <<"Kylie">>}, {out, <<"recorded">>}, {out, <<"incluided">>}, all].
[{graph_vertex,<<"Kylie">>},{out,<<"recorded">>},{out,<<"incluided">>},all]
9>GremblinQuery = kylie:build_gremblin_human_readable(PropLispQuery).
<<"g.V('Kylie').Out('recorded').Out('incluided').All()">>
10>{ok, Result} = kylie:query(GremblinQuery).
{ok,[<<"Can't Get You Out of My Head">>,<<"In Your Eyes">>]}

```
![kylie](https://cloud.githubusercontent.com/assets/6124495/18232603/3f1f34fa-72a9-11e6-8b52-4a2731a4be7c.gif)

