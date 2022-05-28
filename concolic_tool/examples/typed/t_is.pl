:- style_check(-singleton).
:- use_module('./type_check/type_check').
:- style_check(+singleton).

:- pred eval(integer).
%eval(X, Y) :- X = 0, Y is X + 1.
eval(X) :- X is 8 // 2.
eval(X) :- X is 5.
