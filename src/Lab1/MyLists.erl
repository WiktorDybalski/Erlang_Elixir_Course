-module('MyLists').

%% API
-export([contains/2, duplicateElements/1, sumFloats/1]).

%% [1, 3, 'a', "chars"], a -> true
contains([_ = Val| _], Val) -> true;
contains([], _) -> "There is nothing here";
contains([_|T], Val) -> contains(T, Val).

%%['A', 'B', 'C', 'E'] -> ['A','A','B','B','C','C','E','E']
duplicateElements([]) -> [];
duplicateElements([H|T]) -> [H, H | duplicateElements(T)].


sumFloats([]) -> 0;
sumFloats([H|T]) when is_float(H) -> H + sumFloats(T);
sumFloats([_|T]) -> sumFloats(T).
%% with acc
%%sumFloats([], acc) -> 0;
%%sumFloats([H|T], acc) when is_float(H) -> sumFloats(T, acc + H);
%%sumFloats([_|T], acc) -> sumFloats(T, acc).

