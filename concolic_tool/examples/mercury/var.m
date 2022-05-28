:- import_module io.

f(!IO) :- io.format("%d\n", [i(6)], !IO).

main(!IO) :- f(!IO).