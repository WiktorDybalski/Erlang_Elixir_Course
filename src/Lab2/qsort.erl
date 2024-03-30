-module(qsort).

-export([less_than/2, grt_eq_than/2, qs/1, random_elems/3, compare_speeds/1]).

less_than([], _) -> [];
less_than(List, Arg) -> [X || X <- List, X < Arg].

grt_eq_than([], _) -> [];
grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot|Tail]) -> qs(less_than(Tail, Pivot)) ++ [Pivot] ++ qs(grt_eq_than(Tail, Pivot)).


random_elems(N, Min, Max) -> [rand:uniform(Max - Min + 1) + Min - 1 || _<- lists:seq(1, N)].

compare_speeds([]) -> [];
compare_speeds(List) ->
  {Time1, _} = timer:tc(qsort, qs, [List]),
  {Time2, _} = timer:tc(lists, sort, [List]),
  io:format("Time1 = ~p ~n", [Time1]),
  io:format("Time2 = ~p ~n", [Time2]).
