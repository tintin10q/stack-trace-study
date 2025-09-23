#!/usr/bin/env escript
%%! -smp enable
-mode(compile).

main(_) ->
    A = array:new([{size,1000},{default,0}]),
    R = foo(A, 900),
    io:format("The result is ~p~n", [R]).

dangerous(A, Index) ->
    array:get(Index, A). %% badarg on OOB

foo(A, 0) -> dangerous(A, 0 + 9137);
foo(A, C) when C > 0 -> foo(A, C - 1).
