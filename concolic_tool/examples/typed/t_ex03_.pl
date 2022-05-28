:- style_check(-singleton).
:- use_module('./type_check/type_check').
:- style_check(+singleton).

:- pred nat(integer).
nat(0).
nat(1).
nat(2).
nat(_).
