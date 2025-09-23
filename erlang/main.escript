#!/usr/bin/env escript
-mode(compile).
-export([main/1]).
dangerous(Array, Index) -> lists:nth(Index + 3, Array). % 1-based nth
foo(A, I)  -> dangerous(A, I).
foo1(A, I) -> foo(A, I * 3).
foo2(A, I) -> foo1(A, I + 137).
foo3(A, I) -> foo2(A, I - 1).
foo4(A, I) -> foo3(A, I * 137).
foo5(A, I) -> foo4(A, I + 20).
foo6(A, I) -> foo5(A, I div 3).
main(_Args) ->
    Array = lists:duplicate(1000, 0),
    io:format("~p~n", [foo6(Array, 50)]).

