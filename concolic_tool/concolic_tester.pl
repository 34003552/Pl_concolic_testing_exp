%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Concolic testing
%
%  Only tested in SWI Prolog, http://www.swi-prolog.org/
%
%  Copyright (c) 2020 Sophie Fortz
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(concolic_tester, [mainT/3]).

:- use_module(concolic_helper).

assert_interactive :- assertz(interactive).

:- dynamic depthk/1. %% max term depth

%% goal is a list of atoms
mainT(CGoal, GroundPos, Depth) :-
    functor(CGoal, P, N), % Concrete Goal
    functor(SGoal, P, N), % Symbolic Goal

    clean_all,

    %% comment the next three asserts for the cgi-bin version:   ////To KEEP!
    %assert_very_verbose, assert_verbose, assert_interactive,

    assertz(depthk(Depth)),
    
    %% adding clause labels:
    assertz(labeled([])),
    copy_term(SGoal, Atom),
    assertz(not_labeled(Atom)),
    add_clause_labels,

    ground_vars(SGoal, GroundPos, GroundVars),
    concolic_helper:display_initial_info(CGoal, SGoal, GroundVars),

    assertz(traces([])),
    assertz(testcases([])),
    assertz(pending_test_case(CGoal)),
    
    concolic_testing(SGoal, GroundPos, GroundVars).

clean_all :-
    retractall(depthk(_)),
    
    retractall(labeled(_)),
    retractall(not_labeled(_)),
    retractall(cl(_, _)),

    retractall(traces(_)),
    retractall(pending_test_case(_)),
    retractall(testcases(_)).

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

add_clause_labels :- \+not_labeled(_), fail.
add_clause_labels :-
    retract(not_labeled(G)),
    functor(G, P, N), labeled(Labeled),
    retractall(labeled(_)), assertz(labeled([pred(P, N) | Labeled])), !,

    findall(cl(G, Body), lpmux:get_program_clause_body_as_list(G, Body), L),

    acl(L, 1, P, N),
    add_clause_labels.
add_clause_labels.

update_not_labeled(B) :- (not_labeled(C), B =@= C -> true ; assertz(not_labeled(B))).

acl([], _, _, _).
acl([cl(H, Body) | R], K, P, N) :-
    H =.. [P | Args],
    append(Args, [l(P, N, K)], NewArgs),
    H2 =.. [P | NewArgs],
    (nonvar(Body) -> add_dump_parameters(Body, BodyR) ; true),
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

:- dynamic traces/1.
:- dynamic testcases/1.
:- dynamic pending_test_case/1.

update_traces(Trace) :-
    traces(Traces),
    (member(Trace, Traces) -> fail ;
        retractall(traces(_)),
        assertz(traces([Trace | Traces]))
    ).

update_testcases(CGoal, Trace) :-
    testcases(Cases),
    (member(testcase(CGoal, Trace), Cases), ! ;
        retractall(testcases(_)),
        assertz(testcases([testcase(CGoal, Trace) | Cases]))
    ), !.

update_pending_test_cases([]) :- !.
update_pending_test_cases([C | R]) :-
    pending_test_case(D), C =@= D, !, %% variant
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
    ), TestCases),
    foldl(suppress_duplicate, TestCases, [], TestCasesNoDup),
    retractall(testcases(_)),
    assertz(testcases(TestCasesNoDup)).

apply_subs(SortedU, Pos) :- nth1(Pos, SortedU, Sigma), Sigma.

suppress_duplicate(testcase(A, Trace), Acc, NewAcc) :-
    member(testcase(A, _), Acc) ->
        NewAcc = Acc ;
        append(Acc, [testcase(A, Trace)], NewAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Concolic testing algorithm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

concolic_testing(SGoal, GroundPos, _) :- %% success !
    \+(pending_test_case(_)), !, % No more pending test cases.
    grounding_vars(SGoal, GroundPos).

concolic_testing(SGoal, GroundPos, GroundVars) :-
    copy_term(foo(SGoal, GroundVars), foo(SGoalCopy, GroundVarsCopy)),
    retract(pending_test_case(CGoal)), !,
    copy_term(CGoal, CGoalCopy), add_dump_label(CGoalCopy, CGoalCopyLabel),
    copy_term(SGoal, SGoalCopy), add_dump_label(SGoalCopy, SGoalCopyLabel),

    z3_helper:init_context(Z3Ctx),

    eval(Z3Ctx, CGoal-SGoalCopy, [CGoalCopyLabel]-[SGoalCopyLabel], [], [], GroundVarsCopy),

    z3_helper:clear_context(Z3Ctx),

    concolic_testing(SGoal, GroundPos, GroundVars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main transition rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% success
eval(_Z3Ctx, CGoal-_SGoal, []-[], Trace, _Gamma, _G):-
    update_testcases(CGoal, Trace).

% unfolding:
eval(Z3Ctx, CGoal-SGoal, [A | RA]-[B | RB], Trace, Gamma, G) :-
    cl(A, Body), !, %% non-deterministic !!!!!!!!!!!

    del_dump_label(A, ANoLabel),
    add_dump_label(ANoLabel, ACopy),
    get_clause_labels(ACopy, ListLabels),
    get_clause_labels(B, ListAllLabels),

    (\+update_traces(Trace) -> true ;
        alts(Z3Ctx, SGoal, Gamma, B, ListLabels, ListAllLabels, G, NewGoals),
        update_pending_test_cases(NewGoals)
    ),

    (nonvar(Body) -> append(Body, RA, NewCGoal) ; NewCGoal = RA),

    %% we require B to match only the same clauses as the concrete goal:
    B =.. [P | Args],
    get_atom_label(A, LabelA),
    change_label(LabelA, Args, ArgsLabelA), NewB =.. [P | ArgsLabelA],
    cl(NewB, BodyR), %% deterministic!
    concolic_helper:update_constraints(Gamma, Gamma_),
    (nonvar(BodyR) -> append(BodyR, RB, NewSGoal) ; NewSGoal = RB),
    append(Trace, [LabelA], NewTrace),
    subtract(ListAllLabels, ListLabels, ListDiffLabels),
    get_constraints(B, G, [], ListDiffLabels, NewGamma_),
    append(Gamma_, NewGamma_, NewGamma),
    term_variables(G, NewG),
    eval(Z3Ctx, CGoal-SGoal, NewCGoal-NewSGoal, NewTrace, NewGamma, NewG).

% failing:
eval(Z3Ctx, CGoal-SGoal, [A | _RA]-[B | _RB], Trace, Gamma, G) :-
    \+cl(A, _Body), !,
    get_clause_labels(B, ListAllLabels),
    (\+update_traces(Trace) -> true ;
        alts(Z3Ctx, SGoal, Gamma, B, [], ListAllLabels, G, NewGoals),
        update_pending_test_cases(NewGoals)
    ),
    update_testcases(CGoal, Trace).

% for debugging purpose
eval(_Z3Ctx, _, _, _, _, _) :- 
    writeln("ERROR: Impossible to find an evaluation rule to apply"),
    fail.


change_label(Label, [_], [Label]) :- !.
change_label(Label, [X | R], [X | RR]) :- change_label(Label, R, RR).

get_atom_label(A, Label) :- A =.. [_F | Args], last(Args, Label).

get_clause_labels(G, Labels) :-
    findall(K, (cl(G, _), get_atom_label(G, K)), Labels).

% useless??
get_new_goals([], []).
get_new_goals([foo(Atom, _, _) | R], [Atom | RR]) :- get_new_goals(R, RR).

% useless??
get_new_trace(Trace, Alts, Traces, NewTrace) :-
    prefix(PTrace, Trace), length(PTrace, N),
    nth0(N, Alts, (_, L)),
    oset_power(L, LPower),
    v_writef("All possibilities: ~w~n", [LPower]),
    v_writef("Visited traces:    ~w~n", [Traces]),
    member(Labels, LPower),  %% nondeterministic!!
    append(PTrace, [Labels], NewTrace),
    v_writef("~w~n", [\+(member(OtherTrace, Traces), prefix(NewTrace, OtherTrace))]),
    \+(member(OtherTrace, Traces), prefix(NewTrace, OtherTrace)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Searching for alternative goals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alts(Z3Ctx, SGoal, Gamma, Atom, Labels, AllLabels, G, NewGoals) :-
    oset_power(AllLabels, LPower),
    depthk(K),

    findall(NewGoal, (
        member(LPos, LPower), LPos \== Labels,
        subtract(AllLabels, LPos, LNeg),
        get_constraints(Atom, G, LPos, LNeg, NewConstr),
        append(Gamma, NewConstr, Constr),
        copy_term(foo(SGoal, G), foo(NewGoal, GCopy)),
        G = GCopy,

        z3_helper:solve(Z3Ctx, G, Constr),

        depth(SGoal, Depth),
        Depth =< K + 1
    ), NewGoals).

depth(T, D) :-
    compound(T) -> aggregate(max(B + 1), P ^ A ^ (arg(P, T, A), depth(A, B)), D) ; D = 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Writing the constraints in a correct form for the Z3 interface (1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mymember(X, [Y | _]) :- X == Y, !.
mymember(X, [_ | R]) :- mymember(X, R).

mysubtract([], _, []).
mysubtract([V | R], G, NG) :- mymember(V, G), mysubtract(R, G, NG).
mysubtract([V | R], G, [V | NG]) :- \+(mymember(V, G)), mysubtract(R, G, NG).

get_constraints(A, G, LPos, LNeg, Constrs) :-
    get_list_clauses(LPos, HPos),
    get_list_clauses(LNeg, HNeg),
    del_dump_label(A, ANoLabel),
    term_variables(ANoLabel, VarA),
    term_variables(G, VarsToBeGrounded),
    mysubtract(VarA, VarsToBeGrounded, VarsNotGrounded),
    concolic_helper:format_constraints(ANoLabel, VarsNotGrounded, HPos, HNeg, Constrs).

get_list_clauses([], []).
get_list_clauses([L | LList], [H | HList]) :-
    findall(H, (cl(V, _), get_atom_label(V, L),!,del_dump_label(V, H)), [H]),
    get_list_clauses(LList, HList).