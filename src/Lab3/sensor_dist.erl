-module(sensor_dist).
-export([sensorLocations/0, peopleLocations/0, dist/2, find_for_person/2, find_closest/2, find_for_person_parallel/3, find_closest_parallel/2, measure_time/0]).


get_rand_locations(Number) ->
  RandFun = fun() -> {rand:uniform(10000), rand:uniform(10000)} end,
  lists:map(fun(_) -> RandFun() end, lists:seq(1, Number)).

sensorLocations() -> get_rand_locations(2000).
peopleLocations() -> get_rand_locations(40000).

dist({X1, Y1}, {X2, Y2}) -> math:sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)).


find_for_person(PersonLocation, SensorLocations) ->
  Distances = [{dist(PersonLocation, SensorLocation), PersonLocation, SensorLocation} || SensorLocation <- SensorLocations],
  lists:min(Distances).

find_closest(PeopleLocations, SensorLocations) ->
  [{find_for_person(PersonLocation, SensorLocations)} || PersonLocation <- PeopleLocations].


find_for_person_parallel(PersonLocation, SensorLocations, Parent) ->
  Distances = [{dist(PersonLocation, SensorLocation), PersonLocation, SensorLocation} || SensorLocation <- SensorLocations],
  Parent ! lists:min(Distances).

find_closest_parallel(PeopleLocations, SensorLocations) ->
  Parent = self(),
%%  [spawn(?MODULE, find_for_person, [PersonLocation, SensorLocations, Parent]) || PersonLocation <- PeopleLocations],
  Children = [spawn(fun() -> find_for_person_parallel(PersonLocation, SensorLocations, Parent) end) || PersonLocation <- PeopleLocations],
  [receive Dist -> Dist end || _ <- Children].

measure_time() ->
  PL = peopleLocations(),
  SL = sensorLocations(),
  {Time1, Result1} = timer:tc(sensor_dist, find_closest, [PL, SL]),
  {Time2, Result2} = timer:tc(sensor_dist, find_closest_parallel, [PL, SL]),
  io:format("Czas wykonania: ~p sekund~n", [Time1 / 1000000]),
  io:format("Czas wykonania: ~p sekund~n", [Time2 / 1000000]),
  {Result1, Result2}.





