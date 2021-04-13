:- style_check(-singleton).
:- use_module('../extras/type_check/type_check').
:- style_check(+singleton).

:- pred plis(list(pair(list(integer), integer))).
plis([[0] - 1]).
plis([[1] - 1]).
