-module(pingpong).
-export([start/0, stop/0, play/1]).


start() ->
  Ping = spawn(fun() -> ping(0) end),
  Pong = spawn(fun pong/0),
  register(ping, Ping),
  register(pong, Pong).

stop() ->
  ping ! stop,
  pong ! stop.

play(N) when is_integer(N) ->
  ping ! {play, N}.

ping(Total) ->
  receive
    {play, N} when N > 0 ->
      io:format("Let's Play!"),
      io:format("Ping ~p~n", [N]),
      timer:sleep(1000),
      pong ! {ping, N - 1},
      ping(Total + N);
    {pong, 0} -> stop();
    {pong, N} when N >= 0 ->
      io:format("Ping ~p~n", [N]),
      timer:sleep(1000),
      pong ! {ping, N - 1},
      ping(Total + N);
    stop -> ok;
    _ -> ping(Total)
  after
    10000 ->
      io:format("PING: Ending due to 20 seconds of inactivity. ~n"),
      io:format("Total ping: ~p~n", [Total]),
      ok
  end.

pong() ->
  receive
    {ping, N} when N >= 0 ->
      io:format("Pong ~p~n", [N]),
      timer:sleep(1000),
      ping ! {pong, N - 1},
      pong();
    stop -> ok;
    _ -> pong()
  after
    20000 ->
      io:format("PONG: Ending due to 20 seconds of inactivity. ~n"),
      ok
  end.