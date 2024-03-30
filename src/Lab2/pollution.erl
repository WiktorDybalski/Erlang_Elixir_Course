-module(pollution).
-export([create_monitor/0, add_station/3, check_one_value/3, add_value/5, get_station/2, delete_measure/3, remove_value/4, get_value/3, get_one_value/4, get_station_mean/3, get_daily_mean/3, get_all_measurements/2, get_hourly_mean/4]).

-record(station, {name :: string(), coordinates :: {float(), float()}, measurements = #{}}).
-record(monitor, {name_map, cords_map}).

create_monitor() -> #monitor{name_map = maps:new(), cords_map = maps:new()}.

add_station(Name, Coordinates, Monitor) ->
  Is_id_unique = not maps:is_key(Name, Monitor#monitor.name_map) and not maps:is_key(Coordinates, Monitor#monitor.cords_map),
  case Is_id_unique of
    false ->
      {error, "Name or Cords already exists"};
    true ->
      New_name_map = maps:put(Name, #station{name = Name, coordinates = Coordinates, measurements = []}, Monitor#monitor.name_map),
      New_cords_map = maps:put(Coordinates, #station{name = Name, coordinates = Coordinates, measurements = []}, Monitor#monitor.cords_map),
      Monitor#monitor{name_map = New_name_map, cords_map = New_cords_map}
  end.

get_station(Station_id, Monitor) ->
  Station = case Station_id of
              [_ | _] -> case maps:is_key(Station_id, Monitor#monitor.name_map) of
                           true -> maps:get(Station_id, Monitor#monitor.name_map);
                           false -> {error, no_such_station_name}
                         end;
              {_, _} -> case maps:is_key(Station_id, Monitor#monitor.cords_map) of
                          true -> maps:get(Station_id, Monitor#monitor.cords_map);
                          false -> {error, no_such_coordinates}
                        end
            end,
  Station.

check_one_value(_, _, []) -> 0;
check_one_value(Type, Date, [{T, _, D} | Tail]) ->
  case Type == T andalso Date == D of
    true -> 1;
    false -> check_one_value(Type, Date, Tail)
  end.

add_value(Key, Date, Type, Value, Monitor) ->
  case get_station(Key, Monitor) of
    {error, _} -> {error, "Station doesn't exist"};
    Station ->
      case check_one_value(Type, Date, Station#station.measurements) of
        1 ->
          {error, "This values already exists in this Station"};
        0 ->
          New_measurements = {Type, Value, Date},
          Updated_measurements = [New_measurements | Station#station.measurements],
          Updated_station = Station#station{measurements = Updated_measurements},
          Updated_name_map = maps:put(Station#station.name, Updated_station, Monitor#monitor.name_map),
          Updated_cords_map = maps:put(Station#station.coordinates, Updated_station, Monitor#monitor.cords_map),
          Monitor#monitor{name_map = Updated_name_map, cords_map = Updated_cords_map}
      end
  end.

delete_measure(_, _, []) -> [];
delete_measure(Type, Date, [{T, V, D} | Tail]) ->
  case {Type, Date} == {T, D} of
    true ->
      delete_measure(Type, Date, Tail);
    false ->
      [{T, V, D} | delete_measure(Type, Date, Tail)]
  end.


remove_value(Key, Date, Type, Monitor) ->
  case get_station(Key, Monitor) of
    {error, _} -> {error, "Station doesn't exist"};
    Station ->
      Measurements = Station#station.measurements,
      Updated_measurements = delete_measure(Type, Date, Measurements),
      case Updated_measurements of
        Measurements -> {error, "There is no value to remove"};
        _ ->
          Updated_station = Station#station{measurements = Updated_measurements},
          Updated_name_map = maps:put(Station#station.name, Updated_station, Monitor#monitor.name_map),
          Updated_cords_map = maps:put(Station#station.coordinates, Updated_station, Monitor#monitor.cords_map),
          Monitor#monitor{name_map = Updated_name_map, cords_map = Updated_cords_map}
      end
  end.

get_value(_, _, []) -> {error, "There is no value"};
get_value(Type, Date, [{T, V, D} | Tail]) ->
  case {Type, Date} == {T, D} of
    true -> V;
    false -> get_value(Type, Date, Tail)
  end.

get_one_value(Key, Date, Type, Monitor) ->
  case get_station(Key, Monitor) of
    {error, _} -> {error, "Station doesn't exist"};
    Station ->
      Measurements = Station#station.measurements,
      case get_value(Type, Date, Measurements) of
        {error, _} -> {error, "Value doesn't exist"};
        Value -> Value
      end
  end.


get_station_mean(Key, Type, Monitor) ->
  case get_station(Key, Monitor) of
    {error, _} -> {error, "Station doesn't exist"};
    Station ->
      Measurements = Station#station.measurements,
      Values_list = [V || {T, V, _} <- Measurements, T == Type],
      Length = length(Values_list),
      case Length of
        0 -> {error, "No measurements of given type"};
        _ -> lists:sum(Values_list) / Length
      end
  end.

get_all_measurements(Acc, []) -> Acc;
get_all_measurements(Acc, [Station | Tail]) -> get_all_measurements(Acc ++ Station#station.measurements, Tail).

get_daily_mean(Type, Day, Monitor) ->
  Measurements = get_all_measurements([], maps:values(Monitor#monitor.name_map)),
  Filtered_measurements = [V || {T, V, {D, _}} <- Measurements, T == Type andalso D == Day],
  Length = length(Filtered_measurements),
  case Length of
    0 -> {error, "No measurements of given type"};
    _ -> lists:sum(Filtered_measurements) / Length
  end.


get_hourly_mean(Key, Type, Hour, Monitor) ->
  case get_station(Key, Monitor) of
    {error, _} -> {error, "Station doesn't exist"};
    Station ->
      Measurements = Station#station.measurements,
      Filtered_measurements = [V || {T, V, {_, {H, _, _}}} <- Measurements, T == Type andalso H == Hour],
      Length = length(Filtered_measurements),
      case Length of
        0 -> {error, "No measurements of given type"};
        _ -> lists:sum(Filtered_measurements) / Length
      end
  end.