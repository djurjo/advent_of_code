:- module(day8, _, [assertions,dynamic, fsyntax, hiord]).

:- use_module(library(stream_utils), [get_line/2]). 
:- use_module(library(streams), [open/3]).
:- use_module('../../utils/parse_file.pl', [separate_row/5]).
:- use_module(library(lists), [length/2]). 

:- export(_).

solve_part1(File, Sol) :-
    open(File, read, Streams),
    part1(Streams, Sol).

signalPattern(Line) :-
    separate_row(Line, Pattern, [], Rest, 32).
    %get_pattern_unif(Pattern, Unif).


get_unif([X|Rest], Vars, P, Sub) :-
    P(X, Vars, Sub),
    .

    
part1(Streams, Sol) :-
    get_line(Streams, Line),
    \+ Line = end_of_file, !, 
    separate_row(Line, _SgPatt, [], [_|Vals], 124), %% atom_codes('|', [124]).
    count_uniq(Vals, 0, Count),     %%% [_|Vals] remove first blank ;)
    part1(Streams, TCount),
    Sol is TCount + Count.
part1(_, 0).

count_uniq([], Count, Count).
count_uniq(Line, CCount, Count) :-
    separate_row(Line, Num, [], Rest, 32),
    length(Num, M), !,
    update_count(M, CCount, TCount) ,
    count_uniq(Rest, TCount, Count).

update_count(M, CCount, TCount) :- 
    (M = 2 ; M = 3 ; M = 4 ; M = 7), !,
    TCount is CCount + 1.
update_count(_, Count, Count).


zero(X, [A, B, C, D, E, F, G], Sub) :-
    Sub =
    (
        X = C;
            X = B;
                X = C;
                    X = E;
                        X = F;
                            X = G
    ).
one(X,[A, B, C, D, E, F, G], Sub) :-
    Sub = (X = C; X = F).
two(X,[A, B, C, D, E, F, G], Sub) :-
    Sub =
    (
        X = A;
            X = C;
                X = D;
                    X = E;
                        X = G
                    ).
three(X,[A, B, C, D, E, F, G], Sub) :-
    Sub =
    (
        X = A;
            X = C;
                X = D;
                    X = F;
                        X = G
                        ).
four(X,[A, B, C, D, E, F, G], Sub) :-
    Sub =
    (
        X = B;
            X = C;
                X = D;
                    X = F
                ).
five(X,[A, B, C, D, E, F, G], Sub) :-
    Sub =
    (
        X = A;
            X = B;
                X = D;
                    X = F;
                        X = G
                    ).
six(X,[A, B, C, D, E, F, G], Sub) :-
    Sub =
    (
        X = A;
            X = B;
                X = D;
                    X = E;
                        X = F;
                            X = G
                        ).
seven(X,[A, B, C, D, E, F, G], Sub) :-
    Sub =
    (
        X = A;
            X = C;
                X = F
            ).
eight(X,[A, B, C, D, E, F, G], Sub) :- 
    Sub =
    (
        X = A;
            X = B;
                X = C;
                    X = D;
                        X = E;
                            X = F;
                                X = G
    ).
nine(X,[A, B, C, D, E, F, G], Sub) :-
    Sub =
    (
        X = A;
            X = B;
                X = C;
                    X = D;
                        X = F;
                            X = G
    ).    



    
