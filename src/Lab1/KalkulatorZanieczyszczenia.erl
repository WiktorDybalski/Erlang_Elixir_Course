-module('KalkulatorZanieczyszczenia').
-author("wikto").

%% API
-export([listaPomiarow/0, dodaj_pomiar/2, check_type/2, check_type_m/2, number_of_readings/2, number_of_readings3/3, calculate_max/2, calculate_max3/3, calculate_max3_m/3, calculate_mean/2, calculate_mean3/3, calculate_mean3_m/3]).

%%P1 = {"Stacja1", {{2024, 03, 18}, {12, 30, 00}}, [{"PM10", 20.0}, {"PM1", 15.0}, {"temperatura", 22.5}, {"cisnienie", 1012.3}, {"wilgotnosc", 55.0}]}.
%%P2 = {"Stacja2", {{2024, 03, 18}, {10, 15, 00}}, [{"PM10", 12.0}, {"PM1", 8.5}, {"temperatura", 18.5},  {"cisnienie", 822.5}, {"wilgotnosc", 60.0}]}.
%%P3 = {"Stacja3", {{2024, 03, 18}, {11, 45, 00}}, [{"PM10", 25.0}, {"PM1", 18.0}, {"temperatura", 15.0}, {"cisnienie", 820.5}, {"wilgotnosc", 75.0}]}.
%%
%%ListaPomiarow = [P1, P2, P3].
%%
%%P4 = {"Stacja4", {{2024, 03, 18}, {14, 45, 00}}, [{"PM10", 23.0}, {"PM1", 19.0}, {"temperatura", 24.0}, {"cisnienie", 900.5}, {"wilgotnosc", 60.0}]}.

listaPomiarow() ->
  [
    {"Stacja1", {{2024, 03, 18}, {12, 30, 00}}, [{"PM10", 20.0}, {"PM1", 15.0}, {"temperatura", 22.5}, {"cisnienie", 1012.3}, {"wilgotnosc", 55.0}]},
    {"Stacja2", {{2024, 03, 18}, {10, 15, 00}}, [{"PM10", 12.0}, {"PM1", 8.5}, {"temperatura", 18.5}, {"cisnienie", 822.5}, {"wilgotnosc", 60.0}]},
    {"Stacja3", {{2024, 03, 18}, {11, 45, 00}}, [{"PM10", 25.0}, {"PM1", 18.0}, {"temperatura", 15.0}, {"cisnienie", 820.5}, {"wilgotnosc", 75.0}]},
    {"Stacja4", {{2024, 03, 18}, {14, 45, 00}}, [{"PM10", 23.0}, {"PM1", 19.0}, {"temperatura", 24.0}, {"cisnienie", 900.5}, {"wilgotnosc", 60.0}]}
  ].

dodaj_pomiar([], P4) -> [P4];
dodaj_pomiar([H | T], P4) -> [H | dodaj_pomiar(T, P4)].

check_type([{_, _, M} | _], Type) -> check_type_m(M, Type).

check_type_m([], _) -> false;
check_type_m([{MType, _} | _], Type) when MType == Type -> true;
check_type_m([{_, _} | T], Type) -> check_type_m(T, Type).

%% Calculate Number of measurements
number_of_readings(Readings, Date) -> number_of_readings3(Readings, Date, 0).

number_of_readings3([], _, Counter) -> Counter;
number_of_readings3([{_, {{Y, M, D}, _}, _} | T], {Year, Month, Day}, Counter)
  when {Y, M, D} == {Year, Month, Day}
  -> number_of_readings3(T, {Year, Month, Day}, Counter + 1);
number_of_readings3([_ | T], {Year, Month, Day}, Counter)
  -> number_of_readings3(T, {Year, Month, Day}, Counter).


%% Calculate Max of measurement
calculate_max(Readings, Type) ->
  case check_type(Readings, Type) of
    true ->
      calculate_max3(Readings, Type, 0);
    false ->
      io:format("Nie ma takiego pomiaru!~n"),
      {error, nie_ma_takiego_pomiaru}
  end.

calculate_max3([], _, Temp) -> Temp;
calculate_max3([{_, _, M} | T], Type, Temp) ->
  NewTemp = calculate_max3_m(M, Type, Temp),
  calculate_max3(T, Type, NewTemp).

calculate_max3_m([], _, Temp) -> Temp;
calculate_max3_m([{MeasurementType, Value} | T], Type, Temp) when MeasurementType == Type ->
  NewTemp = max(Value, Temp),
  calculate_max3_m(T, Type, NewTemp);
calculate_max3_m([_ | T], Type, Temp) ->
  calculate_max3_m(T, Type, Temp).


%% Calculate Mean of someType of measurement
calculate_mean(Readings, Type) ->
  case check_type(Readings, Type) of
    true ->
      {Sum, Count} = calculate_mean3(Readings, Type, {0, 0}),
      case Count of
        0 -> 0;
        _ -> Sum / Count
      end;
    false ->
      io:format("Nie ma takiego pomiaru~n"),
      {error, nie_ma_takiego_pomiaru}
  end.

calculate_mean3([], _, {Sum, Counter}) -> {Sum, Counter};
calculate_mean3([{_, _, M} | T], Type, {Sum, Counter}) ->
  {NewSum, NewCount} = calculate_mean3_m(M, Type, {Sum, Counter}),
  calculate_mean3(T, Type, {NewSum, NewCount}).

calculate_mean3_m([], _, {Sum, Counter}) -> {Sum, Counter};
calculate_mean3_m([{MeasurementType, Value} | T], Type, {Sum, Counter}) when MeasurementType == Type ->
  calculate_mean3_m(T, Type, {Sum + Value, Counter + 1});
calculate_mean3_m([_ | T], Type, {Sum, Counter}) ->
  calculate_mean3_m(T, Type, {Sum, Counter}).

%%ListaPomiarow = KalkulatorZanieczyszczenia:listaPomiarow().
%%Pomiary = fun(D) -> lists:map(fun({_, _, P}) -> P end, D) end.
%%WszystkiePomiary = Pomiary(ListaPomiarow)
%%WszystkiePomiarySplaszczone3 = fun(P) -> lists:foldl(fun(InnerList, Acc) -> Acc ++ InnerList end,[], P) end.
%%Splasczone = WszystkiePomiarySplaszczone3(WszystkiePomiary).
%%KonkretnyPomiar = fun(P, Pomiar1) -> [{Pomiar, Wartosc} || {Pomiar, Wartosc} <- P, Pomiar == Pomiar1] end.
%%Cisnienie = KonkretnyPomiar(Splasczone, "cisnienie").
%%WilgotnoscSr = lists:sum(Wilgotnosc) / length(Wilgotnosc).


