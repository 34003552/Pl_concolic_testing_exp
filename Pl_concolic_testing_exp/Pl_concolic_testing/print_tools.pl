%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some pretty-printing utilities..
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(print_tools, [
        verbose/0, very_verbose/0,
        assert_verbose/0, assert_very_verbose/0,
        vprint/1, vprintln/1, vprintln_atom/1, vprintln_atom_ZORG/1,
        vvprint/1, vvprintln/1, vvprintln_atom/1,
        println/1, print_atom/1, println_atom/1
    ]).

:- trust_pred nl(any).

:- pred verbose.
:- dynamic verbose/0.
:- pred very_verbose.
:- dynamic very_verbose/0.

:- pred assert_verbose.
assert_verbose :- verbose -> true ; assertz(verbose)::assertz(any).

:- pred assert_very_verbose.
assert_very_verbose :- very_verbose -> true ; assertz(very_verbose)::assertz(any).

:- pred vprint(any).
vprint(X) :- (verbose -> print(user, X)::print(any, any) ; true).

:- pred vprintln(any).
vprintln(X) :- (verbose -> (print(user, X)::print(any, any), nl(user)) ; true).

:- pred vprintln_atom(any).
vprintln_atom(X) :- (
    verbose -> (
        copy_term(X, C),
        numbervars(C, 0, _)::numbervars(any, integer, any),
        print(user, C)::print(any, any), nl(user)
    ) ; true).

:- pred vprintln_atom_ZORG(list(any)). % JE: temporary clone
vprintln_atom_ZORG(X) :- (
    verbose -> (
        copy_term(X, C),
        numbervars(C, 0, _)::numbervars(list(any), integer, any),
        print(user,C)::print(any,list(any)), nl(user)
    ) ; true).

:- pred vvprint(any).
vvprint(X) :- (very_verbose -> print(user, X)::print(any, any) ; true).

:- pred vvprintln(any).
vvprintln(X) :- (very_verbose -> (print(user, X)::print(any, any), nl(user)) ; true).

:- pred vvprintln_atom(any).
vvprintln_atom(X) :- (
    very_verbose -> (
        copy_term(X, C),
        numbervars(C, 0, _)::numbervars(any, integer, any),
        print(user, C)::print(any, any), nl(user)
    ) ; true).

:- pred println(any).
println(X) :- print(user, X)::print(any, any), nl(user).

:- pred print_atom(any).
print_atom(X) :- copy_term(X, C), numbervars(C, 0, _)::numbervars(any, integer, any), print(user, C)::print(any, any).

:- pred println_atom(any).
println_atom(X) :- copy_term(X, C), numbervars(C, 0, _)::numbervars(any, integer, any), print(user, C)::print(any, any), nl(user).
