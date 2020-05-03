%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Concolic testing
%
%  Only tested in SWI Prolog, http://www.swi-prolog.org/
%
%  Copyright (c) 2020 Sophie Fortz
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(concolic_algo, [print_test_cases/0, mainT/4, add_clause_labels/0, ground_vars/3, concolic_testing/3]).


:- use_module(prolog_reader).
:- use_module(swiplz3).
:- use_module(z3_helper).

:- type foo ---> foo(any, list(any)) ; foo(any, any) ; foo(any, any, any).
:- type unk ---> (any = any) ; (any \= any) ; forall(any, unk) ; exists(any, unk).

%:- type alpha ---> p(a,b) ; p(any) ; p(a,any) ; p(s(a),a) ; nat(integer) ; generate(any,any,any).

:- trust_pred foldl(any, list(flag_t), any, any).
:- trust_pred z3_mk_term_type(any, list(any), boolean, boolean).
:- trust_pred prolog_flag(any, list(any)).
:- trust_pred print_help.
:- trust_pred write(any).
:- trust_pred list_to_set(list(any), list(any)).
:- trust_pred reverse(list(any), list(any)).
:- trust_pred on_exception(any, any, any).
:- trust_pred flush_output(any).
:- trust_pred atom(any).
:- trust_pred halt.
:- trust_pred =@=(any, any).
:- trust_pred unifiable(any, any, any).
:- trust_pred maplist(any, list(integer)).
:- trust_pred last(list(any), any).
:- trust_pred prefix(list(any), any).
:- trust_pred length(list(any), any).
:- trust_pred nth0(any, any, any).
:- trust_pred \+(any, any).
:- trust_pred oset_power(list(any), list(list(any))).
:- trust_pred compound(any).
:- trust_pred aggregate(any, any, integer).
:- trust_pred cl(any, list(any)).
:- trust_pred update_constraint(any, any).
:- trust_pred z3_push(any).
:- trust_pred z3_mk_term_vars(any, list(any)).
:- trust_pred z3_check(any).
:- trust_pred z3_print_model(any, any).
:- trust_pred false.

:- pred filename(any).
:- dynamic filename/1.

:- pred depthk(integer).
:- dynamic depthk/1. %% max term depth

%%%



:- pred cli_option(any).
:- dynamic cli_option/1.
:- pred cli_initial_cg(any).
:- dynamic cli_initial_cg/1.
:- pred cli_initial_sg(any).
:- dynamic cli_initial_sg/1.
:- pred cli_initial_ground(list(integer)).
:- dynamic cli_initial_ground/1.
:- pred cli_initial_depth(integer).
:- dynamic cli_initial_depth/1.
:- pred cli_initial_timeout(integer).
:- dynamic cli_initial_timeout/1.
:- pred cli_initial_trace.
:- dynamic cli_initial_trace/0.
:- pred cli_initial_file(any).
:- dynamic cli_initial_file/1.

%main/0

 :- pred interactive.
:- dynamic interactive/0. %% used to decide which clause of write_form to use

:- pred main_cli.
main_cli :-
    cli_initial_cg(Q),
    cli_initial_ground(NR),
    cli_initial_depth(K),
    cli_initial_timeout(T),
    cli_initial_file(File),!, (
        cli_initial_trace ->
            main(Q, NR, K, T, true, File)::main(any, list(integer), integer, integer, boolean, any) ;
            main(Q, NR, K, T, false, File)::main(any, list(integer), integer, integer, boolean, any)
    ).

:- pred with_trace.
:- dynamic with_trace/0.

%main/6

%error_process/1

%% This one prints both test cases and pending test cases:
:- pred print_test_cases.
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

%get_options/3
%recognise_option/3
%recognised_option/2


:- pred convert_entry_to_term(any, any).
convert_entry_to_term(CLIGOAL, Term) :-
    on_exception(Exception, (
            atom_codes(CLIGOAL, Codes),
            read_from_chars(Codes, Term)
        ), (
            nl, print('### Illegal Command-Line Goal: "')::print(any),
            print(CLIGOAL)::print(any), print('"')::print(any), nl,
            format("### Use following format: \"Goal.\"~n",[]),
            print('### Exception: ')::print(any), print(Exception)::print(any), nl,
            halt
        )
    ).

print_help.

:- pred assert_interactive.
assert_interactive :- assertz(interactive).

%% goal is a list of atoms, File is the input program
:- pred mainT(any, list(integer), integer, any).
mainT(CGoal, GroundPos, K, File) :-
    functor(CGoal, P, N), % Concrete Goal
    functor(SGoal, P, N), % Symbolic Goal
    cleaning,
    %% comment the next three asserts for the cgi-bin version:   ////To KEEP!
    %assert_very_verbose,
    %assert_verbose,
    %assert_interactive,
    %
    assertz(depthk(K)),
    %
    assertz(filename(File)),
    vprintln(load_file(File)),
    flush_output(user),
    /*prolog_reader:*/load_file(File)::load_file(any),
    vprintln(finished_loading_file(File)),
    flush_output(user),
    %
    assertz(constants(['o'])), %% 'o' is just some 'fresh' constant for the negatives cases to succeed.
    assertz(functions([])),
    %
    %% adding clause labels:
    assertz(labeled([])),
    copy_term(SGoal, Atom),
    assertz(not_labeled(Atom)),
    add_clause_labels::add_clause_labels,
    %
    %%initial info:
    ground_vars(SGoal, GroundPos, GroundVars),
    vprint('Initial goal:          '), vprintln_atom(CGoal),
    vprint('Symbolic initial goal: '), vprintln_atom(SGoal),
    vprint('Ground variables:      '), vprintln_atom_ZORG(GroundVars),
    %
    assertz(traces([])),
    assertz(testcases([])),
    assertz(pending_test_case(CGoal)),
    %
    concolic_testing(SGoal, GroundPos, GroundVars).

%%%%%%%%%%%
:- pred cleaning.
cleaning :-
    retractall(cli_option(_)),
    retractall(cli_initial_cg(_)),
    retractall(cli_initial_sg(_)),
    retractall(cli_initial_ground(_)),
    retractall(cli_initial_depth(_)),
    retractall(cli_initial_depth(_)),
    retractall(cli_initial_trace),
    retractall(cli_initial_timeout(_)),
    %retractall(with_trace),
    retractall(depthk(_)),
    retractall(cli_initial_file(_)),
    %retractall(interactive),
    retractall(verbose),
    retractall(very_verbose),
    retractall(filename(_)),
    retractall(labeled(_)),
    retractall(not_labeled(_)),
    retractall(cl(_,_)),
    retractall(pending_test_case(_)),
    retractall(constants(_)),
    retractall(functions(_)),
    retractall(traces(_)),
    retractall(testcases(_)),
    retractall(list),
    retractall(integer).

%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extracting the (universal) type from the program.
% So far only atoms and functors are allowed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred constants(list(any)).
:- dynamic constants/1.
:- pred functions(list(any)).
:- dynamic functions/1.
:- pred integer.
:- dynamic integer/0.
:- pred list.
:- dynamic list/0.

:- pred esig_atom_list(list(any)).
esig_atom_list([]).
esig_atom_list([A|R]) :- esig_atom(A), esig_atom_list(R).

:- pred esig_atom(any).
esig_atom(A) :- (A =.. [_|Args])::(any =.. list(any)), esig_term_list(Args).

:- pred esig_term_list(list(any)).
esig_term_list([]).
esig_term_list([T|R]) :- esig_term(T), esig_term_list(R).

:- pred esig_term(any).
esig_term(T) :- var(T), !.
esig_term(T) :- atom(T), !, update_constants(T).
esig_term(T) :- integer(T)::integer(any), !, assertz(integer).
esig_term([]) :- assertz(list).
esig_term([T|R]) :- esig_term(T), esig_term_list(R).
esig_term(T) :-
    compound(T), !, functor(T, F, N), update_functions(F, N),
    (T =.. [F|Args])::(any =.. list(any)), esig_term_list(Args).
esig_term(_T) :- nl, format("ERROR. Type not supported.", []), nl, halt.

:- pred list(list(any)).
list([]).
list([_]).

:- pred update_constants(any).
update_constants(C) :- constants(CL), member(C, CL)::member(any, list(any)), !.
update_constants(C) :- constants(CL), retractall(constants(_)), !, assertz(constants([C|CL])).

:- pred update_functions(any, integer).
update_functions(F, N) :- functions(FL), member(fun(F, N),FL)::member(any, list(any)), !.
update_functions(F, N) :- functions(FL), retractall(functions(_)), !, assertz(functions([fun(F, N)|FL])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Getting list of constants, functions and predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred get_consts(list(any), list(any)).
get_consts([],[]).
get_consts([C|Consts], List) :- get_consts(Consts, List_), List = [(C,0)|List_].

:- pred get_fun(list(any), list(any)).
get_fun([],[]).
get_fun([fun(Name, Arity)|Funs], List):- get_fun(Funs, List_), List = [(Name, Arity)|List_].

:- pred get_pred(list(any), list(any)).
get_pred([],[]).
get_pred([pred(Name, Arity)|Preds],List):- get_pred(Preds, List_), List = [(Name, Arity)|List_].

%%%
:- pred ground_vars(any, list(integer), list(any)).
ground_vars(G, GPos, GVars) :-
    (G =.. [_|Vars])::(any =.. list(any)),
    sort(GPos, SortedGPos)::sort(list(integer), list(integer)),
    gvars(Vars, 1, SortedGPos, GVars).

:- pred gvars(list(any), integer, list(integer), list(any)).
gvars(_, _N, [], []) :- !.
gvars([V|RV], N, [P|RN], [V|GRV]) :- N = P, !, M is N + 1, gvars(RV, M, RN, GRV).
gvars([_|RV], N, [P|RN], GRV) :- N \== P, M is N + 1, gvars(RV, M, [P|RN], GRV).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adding (clause) labels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred labeled(list(any)).
:- dynamic labeled/1.
:- pred not_labeled(any).
:- dynamic not_labeled/1.
:- pred cl/2.
:- dynamic cl/2.  %% for programs

% JE: there is something weird below!!!
%:- pred add_clause_labels. 
add_clause_labels :- \+ not_labeled(_), fail.
add_clause_labels :-
    retract(not_labeled(G)),
    functor(G,P,N),labeled(Labeled),
    retractall(labeled(_)),assertz(labeled([pred(P,N)|Labeled])),!,
    findall(cl(G, Body),/*prolog_reader:*/get_clause_as_list(G,Body)::get_clause_as_list(clause_head,list(clause_body)),L),
    acl(L,1,P,N),
    add_clause_labels.
    add_clause_labels.

:- pred update_not_labeled(any).
update_not_labeled(B) :- (not_labeled(C), B =@= C -> true ; assertz(not_labeled(B))).

:- pred acl(list(any), integer, any, any).
acl([], _, _, _).
acl([cl(H, Body)|R], K, P, N) :-
    (H =.. [P|Args])::(any =.. list(any)),
    copy_term(Args, CopyArgs),
    esig_term_list(CopyArgs), %% for extracting types
    esig_atom_list(Body), %% for extracting types
    append(Args, [l(P, N, K)], NewArgs)::append(list(any), list(any), list(any)),
    (H2 =.. [P|NewArgs])::(any =.. list(any)),
    add_dump_parameters(Body, BodyR),
    assertz(cl(H2, BodyR)),
    K2 is K + 1,
    acl(R, K2, P, N).

:- pred add_dump_parameters(list(any), list(any)).
add_dump_parameters([], []).
add_dump_parameters([A|R], [AA|RR]) :-
    functor(A, P, N), (A =.. [P|Args])::(any =.. list(any)),
    % updating the pending predicates...
    functor(B, P, N), labeled(Labeled),
    (member(pred(P, N), Labeled)::member(any, list(any)) -> true ; update_not_labeled(B)),
    %
    append(Args, [_], NewArgs)::append(list(any), list(any), list(any)),
    (AA =.. [P|NewArgs])::(any =.. list(any)),
    add_dump_parameters(R, RR).

:- pred add_dump_label(any, any).
add_dump_label(A, B) :-
    (A =.. [P|Args])::(any =.. list(any)),
    append(Args, [_], NewArgs)::append(list(any), list(any), list(any)),
    (B =.. [P|NewArgs])::(any =.. list(any)).

:- pred del_dump_label(any, any).
del_dump_label(A, B) :-
    (A =.. [P|Args])::(any =.. list(any)),
    append(NewArgs, [_], Args)::append(list(any), list(any), list(any)),
    (B =.. [P|NewArgs])::(any =.. list(any)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Updating (pending) test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred traces(list(list(any))).
:- dynamic traces/1.
:- pred testcases(list(any)).
:- dynamic testcases/1.
:- pred pending_test_case(any).
:- dynamic pending_test_case/1.

:- pred update_testcases(any, list(any)).
update_testcases(CGoal, Trace) :-
    testcases(Cases), (
        member(testcase(CGoal, Trace), Cases)::member(any, list(any)), ! ;
        retractall(testcases(_)), assertz(testcases([testcase(CGoal,Trace)|Cases]))
    ), !.

:- pred update_pending_test_cases(list(any)).
update_pending_test_cases([]) :- !.
update_pending_test_cases([C|R]) :-
    pending_test_case(D), C =@= D,!, %% variant
    update_pending_test_cases(R).
update_pending_test_cases([C|R]) :-
    assertz(pending_test_case(C)),
    update_pending_test_cases(R).

:- pred grounding_vars(any, list(integer)).
grounding_vars(SGoal, GroundPos) :-
    testcases(OldCases),
    findall(testcase(GroundGoal, Trace), (
            member(testcase(CGoal, Trace), OldCases)::member(any, list(any)),
            copy_term(SGoal, GroundGoal),
            unifiable(CGoal, GroundGoal, U),
            sort(U, SortedU)::sort(any, any),
            maplist(apply_subs(SortedU), GroundPos)
        ), TestCases
    ),
    foldl(suppress_duplicate, TestCases, [], TestCasesNoDup),
    retractall(testcases(_)),
    assertz(testcases(TestCasesNoDup)).

:- pred apply_subs(any, any).
apply_subs(SortedU, Pos) :- nth1(Pos, SortedU, Sigma)::nth1(any, any, pred), Sigma.

:- pred suppress_duplicate(any, list(any), list(any)).
suppress_duplicate(testcase(A, Trace), Acc, NewAcc) :-
    member(testcase(A,_),Acc)::member(any, list(any)) ->
        NewAcc = Acc ;
        append(Acc, [testcase(A, Trace)], NewAcc)::append(list(any), list(any), list(any)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Printing test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred print_testcases(list(any)).
print_testcases([]).
print_testcases([testcase(A, [])|R]) :-
    print_atom(A),
    (with_trace::with_trace -> print(' with trace ')::print(any), println('{}'); nl),
    print_testcases(R).
print_testcases([testcase(A, Trace)|R]) :-
    print_atom(A),
    (with_trace::with_trace -> print(' with trace ')::print(any), print_trace(Trace); nl),
    print_testcases(R).

:- pred print_trace(list(any)).
print_trace([]) :- nl.
print_trace([S|R]) :- print('{')::print(any), print_trace_step(S), print('} ')::print(any), print_trace(R).

:- pred print_trace_step(any).
print_trace_step(l(P,N,K)) :- !,
    print('(')::print(any), print(P)::print(any), print('/')::print(any), print(N)::print(any),
    print(',')::print(any), print(K)::print(any), print(')')::print(any).

:- pred print_testcases_2(list(any)).
print_testcases_2([]).
print_testcases_2([A|R]) :- print_atom(A), nl, print_testcases_2(R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Concolic testing algorithm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred concolic_testing(any, list(integer), list(any)).
concolic_testing(SGoal, GroundPos, _) :- %% success !
    \+(pending_test_case(_)), !, % No more pending test cases.
    nl, println('Procedure complete!'), nl,
    grounding_vars(SGoal, GroundPos),
    testcases(Solution),
    print_testcases(Solution),!.

concolic_testing(SGoal, GroundPos, GroundVars) :-
    copy_term(foo(SGoal, GroundVars), foo(SGoalCopy, GroundVarsCopy)),
    retract(pending_test_case(CGoal)), !,
    copy_term(CGoal, CGoalCopy),
    copy_term(SGoal, SGoalCopy),
    add_dump_label(CGoalCopy, CGoalCopyLabel),
    add_dump_label(SGoalCopy, SGoalCopyLabel),

    z3_init_context(Ctx),
    constants(C),
    get_consts(C, Consts),
    functions(F),
    get_fun(F, Functions),
    labeled(Preds),
    get_pred(Preds, Predicates),
    append(Consts, Functions, Terms_)::append(list(any), list(any), list(any)),
    append(Terms_, Predicates, Terms)::append(list(any), list(any), list(any)),
    (integer -> Int = true; Int = false),
    (list -> List = true; List = false),
    z3_mk_term_type(Ctx, Terms, Int, List),
    eval(CGoal, [CGoalCopyLabel], [SGoalCopyLabel], [], SGoalCopy, [], GroundVarsCopy),
    z3_clear_context(Ctx),
    concolic_testing(SGoal, GroundPos, GroundVars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main transition rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred eval(any, list(any), list(any), list(any), any, list(any), any).

% success
eval(CGoal, [], [], Trace, _SGoal, _Gamma, _G):-
    update_testcases(CGoal,Trace).

% unfolding:
eval(CGoal, [A|RA], [B|RB], Trace, SGoal, Gamma, G) :-
    cl(A, Body), !, %% non-deterministic !!!!!!!!!!!

    del_dump_label(A, ANoLabel),
    add_dump_label(ANoLabel, ACopy),
    findall(N, (cl(ACopy, _), get_atom_label(ACopy, N)), ListLabels),
    findall(K, (cl(B, _), get_atom_label(B, K)), ListAllLabels),

    traces(Traces), (
        member(Trace, Traces)::member(list(any), list(list(any))) -> true ;
        alts(SGoal, Gamma, B, ListLabels, ListAllLabels, G, NewGoals),
        update_pending_test_cases(NewGoals),
        retractall(traces(_)),
        assertz(traces([Trace|Traces]))
    ),

    append(Body, RA, NewCGoal)::append(list(any), list(any), list(any)),

    %% we require B to match only the same clauses as the concrete goal:
    (B =.. [P|Args])::(any =.. list(any)),
    get_atom_label(A, LabelA),
    change_label(LabelA, Args, ArgsLabelA),(NewB =.. [P|ArgsLabelA])::(any =.. list(any)),
    cl(NewB, BodyR), %% deterministic !!!!!!!!!!!
    update_list_constraints(Gamma, Gamma_),
    append(BodyR, RB, NewSGoal)::append(list(any), list(any), list(any)),
    append(Trace, [LabelA], NewTrace)::append(list(any), list(any), list(any)),
    subtract(ListAllLabels, ListLabels, ListDiffLabels)::subtract(list(any), list(any), list(any)),
    get_constraints(B, G, [], ListDiffLabels, NewGamma_),
    append(Gamma_, NewGamma_, NewGamma)::append(list(any), list(unk), list(any)),
    term_variables(G, NewG)::term_variables(any, any),
    eval(CGoal, NewCGoal, NewSGoal, NewTrace, SGoal, NewGamma, NewG).

% failing:
eval(CGoal, [A|_RA], [B|_RB], Trace, SGoal, Gamma, G) :-
    \+(cl(A, _Body)), !,
    findall(K, (cl(B, _), get_atom_label(B, K)), ListAllLabels),
    traces(Traces), (
        member(Trace,Traces)::member(list(any), list(list(any))) -> true;
        alts(SGoal,Gamma,B,[],ListAllLabels,G,NewGoals),
        update_pending_test_cases(NewGoals),
        retractall(traces(_)),
        assertz(traces([Trace|Traces]))
    ),

    update_testcases(CGoal,Trace).

eval(_, _, _, _, _, _) :- % For debugging purpose
  writeln("ERROR: Impossible to find an evaluation rule to apply"),
  fail.

:- pred change_label(any, list(any), list(any)).
change_label(Label, [_], [Label]) :- !.
change_label(Label, [X|R], [X|RR]) :- change_label(Label, R, RR).

:- pred get_atom_label(any, any).
get_atom_label(A, Label) :- (A =.. [_F|Args])::(any =.. list(any)), last(Args, Label).

:- pred get_atom_label(any, any, any).
get_atom_label(A, Pred, Label) :-
    (A =.. [_|ArgsLab])::(any =.. list(any)),
    last(ArgsLab, Label),!,
    del_dump_label(A, Pred).

:- pred get_new_goals(list(foo), list(any)).
get_new_goals([], []).
get_new_goals([foo(Atom, _, _)|R], [Atom|RR]) :- get_new_goals(R, RR).

:- pred get_new_trace(any, any, any, list(any)).
get_new_trace(Trace, Alts, Traces, NewTrace) :-
    prefix(PTrace, Trace), length(PTrace, N),
    nth0(N, Alts, (_, L)),
    oset_power(L, LPower),
    vprint('All possibilities: '), vprintln(to_generic_term(LPower)),
    vprint('Visited traces:    '), vprintln(Traces),
    member(Labels, LPower)::member(list(any), list(list(any))),  %% nondeterministic!!
    append(PTrace, [Labels], NewTrace)::append(list(any), list(list(any)), list(any)),
    vprintln(\+(member(OtherTrace, Traces)::member(list(any), list(list(any))), prefix(NewTrace, OtherTrace))),
    \+(member(OtherTrace, Traces)::member(any, list(any)), prefix(NewTrace, OtherTrace)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Searching for alternative goals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred alts(any, list(any), any, list(any), list(any), any, list(any)).
alts(SGoal, Gamma, Atom, Labels, AllLabels, G, NewGoals) :-
    oset_power(AllLabels, LPower),
    depthk(K),

    findall(NewGoal, (
        member(LPos, LPower)::member(list(any), list(list(any))), LPos \== Labels,
        subtract(AllLabels, LPos, LNeg)::subtract(list(any), list(any), list(any)),
        get_constraints(Atom, G, LPos, LNeg, NewConstr),
        append(Gamma, NewConstr, Constr)::append(list(any), list(unk), list(any)),
        copy_term(foo(SGoal, G), foo(NewGoal, GCopy)),
        G = GCopy,
        context(N),
        solve(N, G, Constr, _Mod),
        depth(SGoal, Depth),
        Depth =< K + 1),
        NewGoals
    ).

:- pred depth(any, integer).
depth(T, D) :-
    compound(T) ->
    aggregate(max(B + 1),P ^ A ^ (arg(P, T, A), depth(A, B)), D) ;
    D = 0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Writing the constraints in a correct form for the Z3 interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred mymember(any, list(any)).
mymember(X, [Y|_]) :- X == Y, !.
mymember(X, [_|R]) :- mymember(X, R).

:- pred mysubtract(list(any), list(any), list(any)).
mysubtract([], _, []).
mysubtract([V|R], G, NG) :- mymember(V, G), mysubtract(R, G, NG).
mysubtract([V|R], G, [V|NG]) :- \+(mymember(V, G)), mysubtract(R, G, NG).

:- pred get_constraints(any, any, list(any), list(any), list(unk)).
get_constraints(A, G, LPos, LNeg, Constrs) :-
    get_list_clauses(LPos, HPos),
    get_list_clauses(LNeg, HNeg),
    del_dump_label(A, ANoLabel),
    term_variables(ANoLabel, VarA)::term_variables(any, list(any)),
    term_variables(G, VarsToBeGrounded)::term_variables(any, list(any)),
    mysubtract(VarA, VarsToBeGrounded, VarsNotGrounded),
    get_pos_consts(ANoLabel, VarsNotGrounded, HPos, PosConsts),
    get_neg_consts(ANoLabel, VarsNotGrounded, HNeg, NegConsts),
    append(PosConsts, NegConsts, Constrs)::append(list(unk), list(unk), list(unk)).

:- pred get_list_clauses(list(any), list(any)).
get_list_clauses([], []).
get_list_clauses([L|LList], [H|HList]) :-
    findall(H,(cl(V, _),get_atom_label(V, H, L)), [H]),
    get_list_clauses(LList, HList).

:- pred get_pos_consts(any, list(any), list(any), list(unk)).
get_pos_consts(_, _, [], []).
get_pos_consts(A, VarsNotGrounded, [H|T], Constrs) :-
    copy_term(H, CArgs),
    term_variables(CArgs, VCArgs)::term_variables(any, list(any)),
    exists_terms(VCArgs, A, VarsNotGrounded, CArgs, C1),
    get_pos_consts(A, VarsNotGrounded, T, C2),
    Constrs = [C1|C2].

:- pred get_neg_consts(any, list(any), list(any), list(unk)).
get_neg_consts(_, _, [], []).
get_neg_consts(A, VarsNotGrounded, [H|T], Constrs) :-
    copy_term(H, CArgs),
    term_variables(CArgs, VCArgs)::term_variables(any, list(any)),
    forall_terms(VCArgs, A, VarsNotGrounded, CArgs, C1),
    get_neg_consts(A, VarsNotGrounded, T, C2),
    Constrs = [C1|C2].

:- pred exists_terms(list(any), any, list(any), any, unk).
exists_terms([], A, VarsNotGrounded, Args, Pred):-
    Pred_ = (A = Args),
    exists_terms_atom(VarsNotGrounded, Pred_, Pred).
exists_terms([H|T], A, VarsNotGrounded, Args, Pred) :-
    exists_terms(T, A, VarsNotGrounded, Args, Pred1),
    Pred = exists(var(H), Pred1).

:- pred exists_terms_atom(list(any), unk, unk).
exists_terms_atom([], Pred, Pred).
exists_terms_atom([V|Vars], Pred1, Pred3) :-
    exists_terms_atom(Vars, Pred1, Pred2),
    Pred3 = exists(var(V), Pred2).

:- pred forall_terms(list(any), any, list(any), any, unk).
forall_terms([], A, VarsNotGrounded, Args, Pred):-
    Pred_ = (A \= Args),
    forall_terms_atom(VarsNotGrounded, Pred_, Pred).
forall_terms([H|T], A, VarsNotGrounded, Args, Pred) :-
    forall_terms(T, A, VarsNotGrounded, Args, Pred1),
    Pred = forall(var(H), Pred1).

:- pred forall_terms_atom(list(any), unk, unk).
forall_terms_atom([], Pred, Pred).
forall_terms_atom([V|Vars], Pred1, Pred3) :-
    forall_terms_atom(Vars, Pred1, Pred2),
    Pred3 = forall(var(V), Pred2).

:- pred update_list_constraints(list(any), list(any)).
update_list_constraints([], []).
update_list_constraints([C|Constr], [NC|NewConstr]) :-
  update_constraint(C, NC),
  update_list_constraints(Constr, NewConstr).

update_constraint((forall(var(V), C)), (forall(var(V), NewConstr))) :-
  var(V), !,
  update_constraint(C, NewConstr).
update_constraint((forall(var([H|T]), C)), NewConstr) :-
  update_constraint(C, NewConstr_),
  NewConstr = (forall(var(H), (forall(var(T), NewConstr_)))).
update_constraint(C, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Z3 solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred solve(any, any, list(any), any).
solve(N, _VarsToBeGrounded, Constr, Model) :-
    z3_push(N),
    /* Declaring constraints to solve*/
    /* To improve efficiency, we could only declare grounded variable, but it needs some C changes...
    %numbervars(VarsToBeGrounded),
    %get_varnames(VarsToBeGrounded,VarsStr),*/
    z3_termconstr2smtlib(N, [], Constr, VarsSTR, Csmtlib),
    (VarsSTR = [] -> true ; z3_mk_term_vars(N, VarsSTR)),
    %print("SMT = "),println(Csmtlib),
    (integer -> Int = true ; Int = false),
    (list -> List = true ; List = false),
    z3_assert_term_string(N, Csmtlib, Int, List),

    /* checking satisfiability */
    (z3_check(N) ->
        z3_print_model(N, Model),
        get_context_vars(N, VVS),
        get_model_varT_eval(N, VVS, Values),
        term_variables(Constr, AllVars)::term_variables(list(any), list(any)),
        z3_pop(N, VarsSTR),
        AllVars = Values;
        z3_pop(N, VarsSTR),
        false
    ).