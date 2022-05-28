:- style_check(-singleton).
:- use_module('./type_check/type_check').
:- style_check(+singleton).

:- type t0 ---> a ; b ; c ; s(t0) ; f(t0).

:- pred p(t0, t0).
p(s(a),b).
p(s(X),a) :- q(X).
p(f(X),s(Y)) :- r(X,Y).

:- pred q(t0).
q(a).
q(b).

:- pred r(t0, t0).
r(a,b).
r(c,b).
