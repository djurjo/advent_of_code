:- module(day1, _, []).
:- export(_).
:- use_module('../../utils/parse_file.pl', [extract_data/3]).
:- use_module(library(stream_utils), [file_to_string/2]).
:- use_module(library(sort), [sort/2]).

%% Given the input get a list of elves
%% An elve is a tuple: (SumOfKalories, List of data)


main(Result1, Result2) :-
    file_to_string('/Users/danieljurjo/jurjo/advent/2022/problems/day1/input', RawData),
    extract_data(RawData, none, Data),
    get_elves(Data, Elves),
    get_max_kal(Elves, 0, Result1),
    sort(Elves, Elves_s),
    greedy_get(Elves_s, Result2).



greedy_get([_|Xs], R) :-
    Xs = [(A, _), (B, _), (C, _)], !,
    R is A + B + C.
greedy_get([_|Xs], R) :-
    greedy_get(Xs, R).


get_max_kal([], Max,Max).
get_max_kal([(Kals, _)|Elvs], Current, Max) :-
    Kals > Current , !,
    get_max_kal(Elvs, Kals, Max).
get_max_kal([_|Elvs], Current, Max) :-
    !, get_max_kal(Elvs, Current, Max).

get_elves([], []).
get_elves(Data, Elvs) :-
    get_one_elve(Data, NData, Elve),
    Elvs = [Elve|TElvs],
    get_elves(NData, TElvs).


get_one_elve([], [], (0, [])).
get_one_elve([D|Data], Data, Elv) :-
    D == [], !,
    Elv = (0, []).
get_one_elve([D|Data], NData, Elv) :-
    !,
    D = [NumVal],
    Elv = (Sum, LData),
    LData = [NumVal|TData],
    get_one_elve(Data, NData, (TSum, TData)),
    Sum is NumVal + TSum.