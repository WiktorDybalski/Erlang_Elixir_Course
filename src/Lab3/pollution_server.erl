-module(pollution_server).
-author("wikto").

-export([start/0, stop/0, show_error/1, pollution_loop/1, add_station/2, get_station/1, add_value/4,
  remove_value/3, get_one_value/3, get_station_mean/2, get_daily_mean/2, get_hourly_mean/3]).



start() ->
  register(server, spawn(fun() -> pollution_loop(none) end)),
  server ! init.

stop() ->
  server ! kill.

show_error(Message) ->
  io:format("ERROR: ~w. ~n", [Message]).


pollution_loop(Monitor) ->
  receive
    init -> case Monitor of
              none ->
                New_monitor = pollution:create_monitor(),
                pollution_loop(New_monitor);
              _ ->
                show_error("Monitor is already initialized"),
                pollution_loop(Monitor)
            end;
    kill -> ok;
    {"add_station", {Name, Coordiantes}} ->
      New_monitor = pollution:add_station(Name, Coordiantes, Monitor),
      case New_monitor of
        {error, Message} ->
          show_error(Message),
          pollution_loop(Monitor);
        _ ->
          pollution_loop(New_monitor)
      end;
    {"get_station", Station_id} ->
      Station = pollution:get_station(Station_id, Monitor),
      io:format("Value: ~p~n", [Station]),
      pollution_loop(Monitor);
    {"add_value", {Key, Date, Type, Value}} ->
      New_monitor = pollution:add_value(Key, Date, Type, Value, Monitor),
      case New_monitor of
        {error, Message} ->
          show_error(Message),
          pollution_loop(Monitor);
        _ ->
          pollution_loop(New_monitor)
      end;
    {"remove_value", {Key, Date, Type}} ->
      New_monitor = pollution:remove_value(Key, Date, Type, Monitor),
      case New_monitor of
        {error, Message} ->
          show_error(Message),
          pollution_loop(Monitor);
        _ ->
          pollution_loop(New_monitor)
      end;
    {"get_one_value", {Key, Date, Type}} ->
      Result = pollution:get_one_value(Key, Date, Type, Monitor),
      io:format("Value: ~p~n", [Result]),
      pollution_loop(Monitor);
    {"get_station_mean", {Key, Type}} ->
      Mean = pollution:get_station_mean(Key, Type, Monitor),
      io:format("Mean Value: ~p~n", [Mean]),
      pollution_loop(Monitor);
    {"get_daily_mean", {Type, Day}} ->
      Mean = pollution:get_daily_mean(Type, Day, Monitor),
      io:format("Daily Mean: ~p~n", [Mean]),
      pollution_loop(Monitor);
    {"get_hourly_mean", {Key, Type, Hour}} ->
      Mean = pollution:get_hourly_mean(Key, Type, Hour, Monitor),
      io:format("Hourly Mean: ~p~n", [Mean]),
      pollution_loop(Monitor)
  end.


add_station(Name, Coordinates) ->
  server ! {"add_station", {Name, Coordinates}}.

get_station(Station_id) ->
  server ! {"get_station", Station_id}.

add_value(Key, Date, Type, Value) ->
  server ! {"add_value", {Key, Date, Type, Value}}.

remove_value(Key, Date, Type) ->
  server ! {"remove_value", {Key, Date, Type}}.

get_one_value(Key, Date, Type) ->
  server ! {"get_one_value", {Key, Date, Type}}.

get_station_mean(Key, Type) ->
  server ! {"get_station_mean", {Key, Type}}.

get_daily_mean(Type, Day) ->
  server ! {"get_daily_mean", {Type, Day}}.

get_hourly_mean(Key, Type, Hour) ->
  server ! {"get_hourly_mean", {Key, Type, Hour}}.