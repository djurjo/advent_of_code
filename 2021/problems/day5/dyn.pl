:- module(dyn, _, [assertions,dynamic]).

:- export(_).

:- dynamic(n/2).


set(X, Y) :-
    assert(n(X, Y)).

