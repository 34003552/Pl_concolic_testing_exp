%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some pretty-printing utilities..
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(print_tools, [
        verbose/0, very_verbose/0,
        assert_verbose/0, assert_very_verbose/0,
        vprint/1, vprintln/1, vprintln_atom/1,
        vvprint/1, vvprintln/1, vvprintln_atom/1,
        println/1, print_atom/1, println_atom/1,

        print_testcases/1, print_trace/1, print_trace_step/1, print_testcases_2/1, print_test_cases/0
    ]).

:- include('extras/headers/h_print_tools.pl').

:- dynamic verbose/0.
:- dynamic very_verbose/0.

assert_verbose :- verbose -> true ; assertz(verbose).

assert_very_verbose :- very_verbose -> true ; assertz(very_verbose).

vprint(X) :- (verbose -> print(user, X) ; true).

vprintln(X) :- (verbose -> (print(user, X), nl(user)) ; true).

vprintln_atom(X) :- (
    verbose -> (
        copy_term(X, C),
        numbervars(C, 0, _),
        print(user, C), nl(user)
    ) ; true).

vvprint(X) :- (very_verbose -> print(user, X) ; true).

vvprintln(X) :- (very_verbose -> (print(user, X), nl(user)) ; true).

vvprintln_atom(X) :- (
    very_verbose -> (
        copy_term(X, C),
        numbervars(C, 0, _),
        print(user, C), nl(user)
    ) ; true).

println(X) :- print(user, X), nl(user).

print_atom(X) :- copy_term(X, C), numbervars(C, 0, _), print(user, C).

println_atom(X) :- copy_term(X, C), numbervars(C, 0, _), print(user, C), nl(user).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Printing test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_testcases([]).
print_testcases([testcase(A, [])|R]) :-
    print_atom(A),
    (with_trace -> print(' with trace '), println('{}'); nl),
    print_testcases(R).
print_testcases([testcase(A, Trace)|R]) :-
    print_atom(A),
    (with_trace -> print(' with trace '), print_trace(Trace); nl),
    print_testcases(R).

print_trace([]) :- nl.
print_trace([S|R]) :- print('{'), print_trace_step(S), print('} '), print_trace(R).

print_trace_step(l(P,N,K)) :- !,
    print('('), print(P), print('/'), print(N),
    print(','), print(K), print(')').

print_testcases_2([]).
print_testcases_2([A|R]) :- print_atom(A), nl, print_testcases_2(R).

%% This one prints both test cases and pending test cases:

print_test_cases :-
    nl, println('Time limit exceeded!'),
    testcases(Cases),  %% processed test cases
    reverse(Cases, CasesR),
    nl, println('Processed test cases: '),
    print_testcases(CasesR),
    nl, println('Pending test cases: '),
    findall(Goal, pending_test_case(Goal), PendingCases), %% pensing tests cases
    list_to_set(PendingCases, PendingCasesL), %% this is just to remove duplicates
    reverse(PendingCasesL, PendingCasesLR),
    nl, print_testcases_2(PendingCasesLR), !.