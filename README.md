# Kylie

Kylie is a Erlang application for Cayley graph data base

 [Cayley](https://github.com/cayleygraph/cayley/) is an open-source graph data base written in go

---------
Example:
```erlang
Eshell V7.0  (abort with ^G)
1>   squad:new("Kylie", "is", "singer").

#{object => "singer",predicate => "is",subject => "Kylie"}
2> kylie:add( squad:new(<<"Kylie">>, <<"is">>,<< "singer">>)).
{ok,{200, <<"{\"result\": \"Successfully wrote 1 quads.\"}">>}}
2>  kylie:add( squad:new(<<"Kylie">>, <<"is">>,<< "songwriter">>)).
{ok,{200, <<"{\"result\": \"Successfully wrote 1 quads.\"}">>}}
3> kylie:add( squad:new(<<"Kylie">>, <<"is">>,<< "model">>)).
{ok,{200, <<"{\"result\": \"Successfully wrote 1 quads.\"}">>}}

4> kylie:get_result(<<"Kylie">>, <<"is">>).
[<<"model">>,<<"singer">>,<<"songwriter">>]
```
