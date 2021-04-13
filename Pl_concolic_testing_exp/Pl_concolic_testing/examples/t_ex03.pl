:- style_check(-singleton).
:- use_module('../extras/type_check/type_check').
:- style_check(+singleton).

:- type pint ---> zero ; s(pint).

:- pred nat(pint).
nat(zero).
nat(s(X)) :- nat(X).
