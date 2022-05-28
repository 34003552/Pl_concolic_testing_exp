:- module test.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

%:- type t(T) ---> a ; closure((func) = 4).
%:- inst i for i/0 ---> closure((func) = is_out is det).%[ground | i] ; closure((func) = is_out is det).
%:- mode is_in  == in(int_stream).

base = 10.

:- import_module string.
main(!IO) :-
    %io.write_string("Hello, World!\n", !IO), (N = [] ; true).
    _ = [base+2 | _],
    io.write_int(base*34,!IO), io.nl(!IO),
    io.write_string("13"++"34",!IO), io.nl(!IO).