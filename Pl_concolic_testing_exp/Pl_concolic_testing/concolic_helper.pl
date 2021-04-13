
:- module(concolic_helper, [
		init_helper/4, clean_helper/0, 

		get_terms/1, integer/0, float/0, list/0,

		cl/2,
		add_dump_label/2, del_dump_label/2,

		pending_test_case/1,
		update_testcases/2, update_pending_test_cases/1,

		toggle_trace_display/1,
		display_testcases/2, write_test_cases/0

		%read_type_annotations/3, build_z3types/3
	]).

atomize(T, A) :- copy_term(T, A), numbervars(A, 0, _).

init_helper(CGoal, SGoal, GroundPos, GroundVars) :-
	assertz(constants(['o'])), %% 'o' is just some 'fresh' constant for the negatives cases to succeed.
    assertz(functions([])),
    
    %% adding clause labels:
    assertz(labeled([])),
    copy_term(SGoal, Atom),
    assertz(not_labeled(Atom)),
    add_clause_labels,
    
    %%initial info:
    ground_vars(SGoal, GroundPos, GroundVars),
    atomize(CGoal, CGoalA), v_printf("Initial goal:          ~w~n", [CGoalA]),
    atomize(SGoal, SGoalA), v_printf("Symbolic initial goal: ~w~n", [SGoalA]),
    atomize(GroundVars, GroundVarsA), v_printf("Ground variables:      ~w~n", [GroundVarsA]),
    
    assertz(testcases([])),
    assertz(pending_test_case(CGoal)).

clean_helper :-
	retractall(constants(_)),
    retractall(functions(_)),
    retractall(list),
    retractall(integer),
    retractall(float),

    retractall(labeled(_)),
    retractall(not_labeled(_)),
    retractall(cl(_, _)),

    retractall(pending_test_case(_)),
    retractall(testcases(_)),

    typifier:unregister_funcs.

get_terms(Terms) :-
	constants(C), get_consts(C, Consts),
    functions(F), get_fun(F, Functions),
    labeled(Preds), get_pred(Preds, Predicates),

    append(Consts, Functions, Terms_),
    append(Terms_, Predicates, Terms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extracting the (universal) type from the program.
% So far only atoms and functors are allowed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic constants/1.
:- dynamic functions/1.
:- dynamic integer/0.
:- dynamic float/0.
:- dynamic list/0.

esig_atom_list([]).
esig_atom_list([A | R]) :- esig_atom(A), esig_atom_list(R).

esig_atom(A) :- A =.. [_ | Args], esig_term_list(Args).

esig_term_list([]).
esig_term_list([T | R]) :- esig_term(T), esig_term_list(R).

esig_term(T) :- var(T), !.
esig_term(T) :- atom(T), !, update_constants(T).
esig_term(T) :- integer(T), !, assertz(integer).
esig_term(T) :- float(T), !, assertz(float).
%esig_term([]) :- assertz(list).
%esig_term([T | R]) :- esig_term(T), esig_term_list(R).
esig_term(T) :- is_list(T), !, assertz(list), esig_term_list(T). 
esig_term(T) :-
    compound(T), !, functor(T, F, N), update_functions(F, N),
    T =.. [F | Args], esig_term_list(Args).
esig_term(_T) :- nl, format("ERROR. Type not supported.", []), nl, halt.

% JE: useless?
list([]).
list([_]).

update_constants(C) :- constants(CL), member(C, CL), !.
update_constants(C) :- constants(CL), retractall(constants(_)), !, assertz(constants([C | CL])).

update_functions(F, N) :- functions(FL), member(fun(F, N), FL), !.
update_functions(F, N) :- functions(FL), retractall(functions(_)), !, assertz(functions([fun(F, N) | FL])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Getting list of constants, functions and predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_consts([], []).
get_consts([C|Consts], List) :- get_consts(Consts, List_), List = [(C, 0) | List_].

get_fun([], []).
get_fun([fun(Name, Arity) | Funs], List):- get_fun(Funs, List_), List = [(Name, Arity) | List_].

get_pred([], []).
get_pred([pred(Name, Arity) | Preds],List):- get_pred(Preds, List_), List = [(Name, Arity) | List_].

%%%
ground_vars(G, GPos, GVars) :-
    G =.. [_ | Vars],
    sort(GPos, SortedGPos),
    gvars(Vars, 1, SortedGPos, GVars).

gvars(_, _N, [], []) :- !.
gvars([V | RV], N, [P | RN], [V | GRV]) :- N = P, !, M is N + 1, gvars(RV, M, RN, GRV).
gvars([_ | RV], N, [P | RN], GRV) :- N \== P, M is N + 1, gvars(RV, M, [P | RN], GRV).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adding (clause) labels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic labeled/1.
:- dynamic not_labeled/1.
:- dynamic cl/2.  %% for programs

% JE: there is something weird below!!!
add_clause_labels :- \+ not_labeled(_), fail.
add_clause_labels :-
    retract(not_labeled(G)),
    functor(G, P, N), labeled(Labeled),
    retractall(labeled(_)), assertz(labeled([pred(P, N) | Labeled])), !,
    findall(cl(G, Body), prolog_reader:get_clause_as_list(G, Body), L),
    acl(L, 1, P, N),
    add_clause_labels.
    add_clause_labels.

update_not_labeled(B) :- (not_labeled(C), B =@= C -> true ; assertz(not_labeled(B))).

acl([], _, _, _).
acl([cl(H, Body) | R], K, P, N) :-
    H =.. [P | Args],
    copy_term(Args, CopyArgs),
    esig_term_list(CopyArgs), %% for extracting types
    esig_atom_list(Body), %% for extracting types
    append(Args, [l(P, N, K)], NewArgs),
    H2 =.. [P | NewArgs],
    add_dump_parameters(Body, BodyR),
    assertz(cl(H2, BodyR)),
    K2 is K + 1,
    acl(R, K2, P, N).

add_dump_parameters([], []).
add_dump_parameters([A | R], [AA | RR]) :-
    functor(A, P, N), A =.. [P | Args],
    % updating the pending predicates...
    functor(B, P, N), labeled(Labeled),
    (member(pred(P, N), Labeled) -> true ; update_not_labeled(B)),

    append(Args, [_], NewArgs),
    AA =.. [P | NewArgs],
    add_dump_parameters(R, RR).

add_dump_label(A, B) :-
    A =.. [P | Args],
    append(Args, [_], NewArgs),
    B =.. [P | NewArgs].

del_dump_label(A, B) :-
    A =.. [P | Args],
    append(NewArgs, [_], Args),
    B =.. [P | NewArgs].

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Updating (pending) test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic testcases/1.
:- dynamic pending_test_case/1.

update_testcases(CGoal, Trace) :-
    testcases(Cases), (
        member(testcase(CGoal, Trace), Cases), ! ;
        retractall(testcases(_)), assertz(testcases([testcase(CGoal, Trace) | Cases]))
    ), !.

update_pending_test_cases([]) :- !.
update_pending_test_cases([C | R]) :-
    pending_test_case(D), C =@= D,!, %% variant
    update_pending_test_cases(R).
update_pending_test_cases([C | R]) :-
    assertz(pending_test_case(C)),
    update_pending_test_cases(R).

grounding_vars(SGoal, GroundPos) :-
    testcases(OldCases),
    findall(testcase(GroundGoal, Trace), (
            member(testcase(CGoal, Trace), OldCases),
            copy_term(SGoal, GroundGoal),
            unifiable(CGoal, GroundGoal, U),
            sort(U, SortedU),
            maplist(apply_subs(SortedU), GroundPos)
        ), TestCases
    ),
    foldl(suppress_duplicate, TestCases, [], TestCasesNoDup),
    retractall(testcases(_)),
    assertz(testcases(TestCasesNoDup)).

apply_subs(SortedU, Pos) :- nth1(Pos, SortedU, Sigma), Sigma.

suppress_duplicate(testcase(A, Trace), Acc, NewAcc) :-
    member(testcase(A, _), Acc) ->
        NewAcc = Acc ;
        append(Acc, [testcase(A, Trace)], NewAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Printing test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_testcases(SGoal, GroundPos) :-
	grounding_vars(SGoal, GroundPos),
    testcases(Solution),
    write_testcases(Solution).

:- dynamic with_trace/0.

toggle_trace_display(Trace) :-
	retractall(with_trace), (Trace -> assertz(with_trace) ; true).

write_testcases([]).
write_testcases([testcase(A, []) | R]) :-
    atomize(A, AA), write(AA),
    (with_trace -> write(' with trace '), writeln('{}') ; nl),
    write_testcases(R).
write_testcases([testcase(A, Trace) | R]) :-
    atomize(A, AA), write(AA),
    (with_trace -> write(' with trace '), write_trace(Trace) ; nl),
    write_testcases(R).

write_trace([]) :- nl.
write_trace([S | R]) :- write('{'), write_trace_step(S), write('} '), write_trace(R).

write_trace_step(l(P, N, K)) :- !,
    write('('), write(P), write('/'), write(N),
    write(','), write(K), write(')').

write_testcases_2([]).
write_testcases_2([A | R]) :- atomize(A, AA), write(AA), nl, write_testcases_2(R).

%% This one prints both test cases and pending test cases:

write_test_cases :-
    nl, writeln('Time limit exceeded!'),
    testcases(Cases),  %% processed test cases
    reverse(Cases, CasesR),
    nl, writeln('Processed test cases: '),
    write_testcases(CasesR),
    nl, writeln('Pending test cases: '),
    findall(Goal, pending_test_case(Goal), PendingCases), %% pensing tests cases
    list_to_set(PendingCases, PendingCasesL), %% this is just to remove duplicates
    reverse(PendingCasesL, PendingCasesLR),
    nl, write_testcases_2(PendingCasesLR), !.