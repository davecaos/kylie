# Kylie (can't get you out of my graph data base)
[![Hex.pm](https://img.shields.io/hexpm/v/kylie.svg?style=flat-square)](https://hex.pm/packages/kylie)
[![Hex.pm](https://img.shields.io/hexpm/dt/kylie.svg?style=flat-square)](https://hex.pm/packages/kylie)
[![License](http://img.shields.io/hexpm/l/kylie.svg?style=flat)](https://hex.pm/packages/kylie)

Kylie is a blond and small Erlang/Elixir client for Cayley graph data base

 [Cayley](https://github.com/cayleygraph/cayley/) is an open-source graph data base written in go

![kylie](https://cloud.githubusercontent.com/assets/6124495/18232603/3f1f34fa-72a9-11e6-8b52-4a2731a4be7c.gif)

---------
Example:
```erlang
>rebar3 shell

===> Verifying dependencies...
===> Compiling kylie
Erlang/OTP 19 [erts-8.1] [source-77fb4f8] [64-bit] [smp:4:4] [async-threads:0] [kernel-poll:false]

Eshell V8.1  (abort with ^G)
1> squad:new("Kylie", "is", "singer").
#{object => "singer",predicate => "is",subject => "Kylie"}

2> kylie:add(squad:new(<<"Kylie">>, <<"is">>,<< "singer">>)).
ok
3> kylie:add(squad:new(<<"Kylie">>, <<"is">>,<< "songwriter">>)).
ok
4> kylie:add(squad:new(<<"Kylie">>, <<"is">>,<< "model">>)).
ok

```

You can do a generic query with Subject and Predicate.
```erlang
5> kylie:get_result(<<"Kylie">>, <<"is">>).
[<<"model">>,<<"singer">>,<<"songwriter">>]
```


You can also do a proplisp(?) and generate a query in the [Gremblin](http://gremlindocs.spmallette.documentup.com/) way.

The idea is build this query: 
```javascript
  g.V('Kylie').Out('recorded').Out('incluided').All()"
```
With this proplisp:
```erlang
   [{graph_vertex, <<"Kylie">>}, {out, <<"recorded">>}, {out, <<"incluided">>}, all]
```

```erlang
6> kylie:add(squad:new(<<"Kylie">>, <<"recorded">>, <<"Fever">>)).
ok
7> kylie:add(squad:new(<<"Fever">>, <<"incluided">>, <<"Can't Get You Out of My Head">>)).
ok
8> kylie:add(squad:new(<<"Fever">>, <<"incluided">>, <<"In Your Eyes">>)),
ok

9>PropLispQuery = [{graph_vertex, <<"Kylie">>}, {out, <<"recorded">>}, {out, <<"incluided">>}, all].
[{graph_vertex,<<"Kylie">>},{out,<<"recorded">>},{out,<<"incluided">>},all]

10>GremblinQuery = kylie:build_gremblin_human_readable(PropLispQuery).
<<"g.V('Kylie').Out('recorded').Out('incluided').All()">>

11>{ok, Result} = kylie:query(GremblinQuery).
[<<"Can't Get You Out of My Head">>,<<"In Your Eyes">>]
```

---------
Elixir Example:
```elixir
>iex -S mix run -e "Kylie.start"
Erlang/OTP 19 [erts-8.1] [source-77fb4f8] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]


=INFO REPORT==== 14-Dec-2016::11:50:14 ===
Creating wpool ETS tableInteractive Elixir (1.3.4) - press Ctrl+C to exit (type h() ENTER for help)

iex(1)> Squad.new("Kylie", "is", "singer")
%{object: "singer", predicate: "is", subject: "Kylie"}

iex(2)> Kylie.add(Squad.new("Kylie", "is", "model"))
:ok
iex(3)> Kylie.add(Squad.new("Kylie", "is", "songwriter"))
:ok
iex(4)> Kylie.add(Squad.new("Kylie", "is", "actress"))
:ok

```

You can do a generic query with Subject and Predicate.
```elixir
iex(4)> Kylie.get_result("Kylie", "is")
["model","singer","songwriter"]
```


You can also do a proplisp(?) and generate a query in the [Gremblin](http://gremlindocs.spmallette.documentup.com/) way.

The idea is build this query: 
```javascript
  g.V("Kylie").Out("recorded").Out("incluided").All()"
```
With this proplisp:
```elixir
   [{:graph_vertex, "Kylie"}, {:out, "recorded"}, {:out, "incluided"}, :all]
```

```elixir

iex(5)> Kylie.add(Squad.new("Kylie", "recorded", "Fever"))
:ok
iex(6)> Kylie.add(Squad.new("Fever", "incluided", "Can't Get You Out of My Head"))
:ok
iex(7)> Kylie.add(Squad.new("Fever", "incluided", "In Your Eyes"))
:ok

iex(8)> propLispQuery = [{:graph_vertex, "Kylie"}, {:out, "recorded"}, {:out, "incluided"}, :all]
[{:graph_vertex, "Kylie"}, {:out, "recorded"}, {:out, "incluided"}, :all]

iex(9)> gremblinQuery = Kylie.build_gremblin_human_readable(propLispQuery)
"g.V("Kylie").Out("recorded").Out("incluided").All()"

iex(10)> {ok, result} = Kylie.query(gremblinQuery)
["Can't Get You Out of My Head","In Your Eyes"]

```