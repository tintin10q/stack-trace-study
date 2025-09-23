#!/usr/bin/env escript
-mode(compile).
-export([main/1]).
dangerous(V1, V2) -> V1 div V2.
foo(Array, Counter) ->
  case Counter of
    0 -> dangerous(hd(Array), Counter);
    _ -> foo(Array, Counter - 1)
  end.
main(_Args) ->
  Array = lists:duplicate(1000, 0),
  io:format("The result is ~p~n", [foo(Array, 6)]).

