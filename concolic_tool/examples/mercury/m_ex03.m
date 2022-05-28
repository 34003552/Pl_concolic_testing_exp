:- type pint ---> '0' ; s(pint).

:- pred nat(pint).
nat('0').
nat(s(X)) :- nat(X).
