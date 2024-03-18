-module(power).

-export([power/2]).

power(_, 0) -> 1;
power(X, Y) -> X * power(X, Y - 1).
