:- style_check(-singleton).
:- use_module('../extras/type_check/type_check').
:- style_check(+singleton).

:- type jy ---> a.
:- type zorn(_T, _U) ---> a.
:- type foo(T) ---> bare(T, float) ; erf(T) ; bar(pair(integer,T), integer).

:- pred perf(foo(foo(float)), foo(integer)).
perf(bar(1-bar(2-3.0, 1),3), bare(1, 2.0)).
perf(bar(4-bar(3-5.0, 1),4), bare(1, 3.0)).
