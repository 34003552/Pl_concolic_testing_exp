:- style_check(-singleton).
:- use_module('./type_check/type_check').
:- style_check(+singleton).

:- pred lis(list(integer)).
lis([0]).
lis([1,0]).
lis([2,1,0]).
lis([3,2,1,0]).
