-module(someFile).

-export([dodaj_pomiar/2]).

%%P1 = {"Stacja1", {{2024, 03, 18}, {12, 30, 00}}, [{"PM10", 20.0}, {"PM1", 15.0}, {"temperatura", 22.5}, {"cisnienie", 1012.3}, {"wilgotnosc", 55.0}]}.
%%P2 = {"Stacja2", {{2024, 03, 18}, {10, 15, 00}}, [{"PM10", 12.0}, {"PM1", 8.5}, {"temperatura", 18.5},  {"cisnienie", 822.5}, {"wilgotnosc", 60.0}]}.
%%P3 = {"Stacja3", {{2024, 03, 18}, {11, 45, 00}}, [{"PM10", 25.0}, {"PM1", 18.0}, {"temperatura", 15.0}, {"cisnienie", 820.5}, {"wilgotnosc", 75.0}]}.
%%
%%ListaPomiarow = [P1, P2, P3].
%%
%%P4 = {"Stacja4", {{2024, 03, 18}, {14, 45, 00}}, [{"PM10", 23.0}, {"PM1", 19.0}, {"temperatura", 24.0}, {"cisnienie", 900.5}, {"wilgotnosc", 60.0}]}.

dodaj_pomiar([], P4) -> [P4];
dodaj_pomiar([H|T], P4) -> [H|dodaj_pomiar(T, P4)].

%%{NazwaP1, _, _} = P1.