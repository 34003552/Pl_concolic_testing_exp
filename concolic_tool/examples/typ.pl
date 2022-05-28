
/*
f(0).
f(1).
g(a, b, 42).
g(s(1, u(6)), 36, X) :- h(X, X, 17).
h(U, V, W) :- integer(U), V = 6, float(W).
e(_).
*/
/*
p(X) :- not(not(q(X))).
q(a).
*/
%p(X, Y) :- X*8.0 is Y + 6.
/*
:- U = V.
f(X) :- integer(X).
f(Y) :- float(Y).
*/

%p((x + 1) * ((^(x, 2) + 2) * (^(x, 3) + 3))).
%p((((x / x) / x) / x)/x).
%p(log(log(x))).
%p((x*x)*x).
%p(x*x).
%p(N*fac(N-1)).
p(exp(U) * DU).