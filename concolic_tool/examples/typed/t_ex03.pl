:- style_check(-singleton).
:- use_module('./type_check/type_check').
:- style_check(+singleton).

:- type pint ---> '0' ; s(pint).

:- pred nat(pint).
nat('0').
nat(s(X)) :- nat(X).
