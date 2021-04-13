%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Concolic testing
%
%  Only tested in SWI Prolog, http://www.swi-prolog.org/
%
%  Copyright (c) 2020 Sophie Fortz
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(concolic_algo, [main/6]).

:- use_module(prolog_reader).
:- use_module(typifier).
:- use_module(concolic_helper).
:- use_module(z3_helper).

error_process(time_limit_exceeded) :-
    writeln("Timeout exceeded"), concolic_helper:write_test_cases, halt.
error_process(X) :- writeln("Unknown Error": X), halt.

main(ConcreteGoal, Ground, Depth, Timeout, WithTrace, File) :-
    concolic_helper:toggle_trace_display(WithTrace),
    catch(call_with_time_limit(Timeout, mainT(ConcreteGoal, Ground, Depth, File)), X, error_process(X)).

assert_interactive :- assertz(interactive).

:- dynamic depthk/1. %% max term depth
:- dynamic filename/1.
:- dynamic traces/1.

%% goal is a list of atoms, File is the input program
mainT(ConcreteGoal, GroundPos, Depth, File) :-
    functor(ConcreteGoal, P, N), % Concrete Goal
    functor(SymbolicGoal, P, N), % Symbolic Goal

    clean_assert,

    %% comment the next three asserts for the cgi-bin version:   ////To KEEP!
    %assert_very_verbose, assert_verbose, assert_interactive,

    assertz(depthk(Depth)),
    assertz(filename(File)),

    flush_output(user),
    prolog_reader:load_file(File),
    flush_output(user),
    
    concolic_helper:init_helper(ConcreteGoal, SymbolicGoal, GroundPos, GroundVars),
    assertz(traces([])),
    
    concolic_testing(SymbolicGoal, GroundPos, GroundVars).

clean_assert :-
    retractall(depthk(_)),
    retractall(filename(_)),

    concolic_helper:clean_helper,
    retractall(traces(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Concolic testing algorithm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

concolic_testing(SGoal, GroundPos, _) :- %% success !
    \+(concolic_helper:pending_test_case(_)), !, % No more pending test cases.
    nl, writeln("Procedure complete!"), nl,
    concolic_helper:display_testcases(SGoal, GroundPos),!.

concolic_testing(SGoal, GroundPos, GroundVars) :-
    copy_term(foo(SGoal, GroundVars), foo(SGoalCopy, GroundVarsCopy)),
    retract(concolic_helper:pending_test_case(CGoal)), !,
    copy_term(CGoal, CGoalCopy),
    copy_term(SGoal, SGoalCopy),
    concolic_helper:add_dump_label(CGoalCopy, CGoalCopyLabel),
    concolic_helper:add_dump_label(SGoalCopy, SGoalCopyLabel),

    z3_init_context(Ctx),

    filename(File), (typifier:has_type_checker(File) -> (
            typifier:read_type_annotations(File, TCTypes, TCPreds),
            %writeln(TCTypes),
            %writeln(TCPreds),
            typifier:build_z3types(TCTypes, TCPreds, Z3Types),
            %print(Z3Types), nl, halt,
            swiplz3:z3_mk_datatypes(Ctx, [], Z3Types)
        ) ; (
            concolic_helper:get_terms(Terms),
            (concolic_helper:integer -> Int = true; Int = false),
            (concolic_helper:float -> throw("typeless mode does not allow float values yet"); true),
            (concolic_helper:list -> List = true; List = false),

            z3_mk_term_type(Ctx, Terms, Int, List)
        )
    ),

    eval(CGoal, [CGoalCopyLabel], [SGoalCopyLabel], [], SGoalCopy, [], GroundVarsCopy),
    
    z3_clear_context(Ctx),

    concolic_testing(SGoal, GroundPos, GroundVars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main transition rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% success
eval(CGoal, [], [], Trace, _SGoal, _Gamma, _G):-
    concolic_helper:update_testcases(CGoal,Trace).

% unfolding:
eval(CGoal, [A|RA], [B|RB], Trace, SGoal, Gamma, G) :-
    concolic_helper:cl(A, Body), !, %% non-deterministic !!!!!!!!!!!

    concolic_helper:del_dump_label(A, ANoLabel),
    concolic_helper:add_dump_label(ANoLabel, ACopy),
    findall(N, (concolic_helper:cl(ACopy, _), get_atom_label(ACopy, N)), ListLabels),
    findall(K, (concolic_helper:cl(B, _), get_atom_label(B, K)), ListAllLabels),

    traces(Traces), (
        member(Trace, Traces) -> true ;
        alts(SGoal, Gamma, B, ListLabels, ListAllLabels, G, NewGoals),
        concolic_helper:update_pending_test_cases(NewGoals),
        retractall(traces(_)),
        assertz(traces([Trace | Traces]))
    ),

    append(Body, RA, NewCGoal),

    %% we require B to match only the same clauses as the concrete goal:
    B =.. [P | Args],
    get_atom_label(A, LabelA),
    change_label(LabelA, Args, ArgsLabelA), NewB =.. [P | ArgsLabelA],
    concolic_helper:cl(NewB, BodyR), %% deterministic!
    update_list_constraints(Gamma, Gamma_),
    append(BodyR, RB, NewSGoal),
    append(Trace, [LabelA], NewTrace),
    subtract(ListAllLabels, ListLabels, ListDiffLabels),
    get_constraints(B, G, [], ListDiffLabels, NewGamma_),
    append(Gamma_, NewGamma_, NewGamma),
    term_variables(G, NewG),
    eval(CGoal, NewCGoal, NewSGoal, NewTrace, SGoal, NewGamma, NewG).

% failing:
eval(CGoal, [A | _RA], [B | _RB], Trace, SGoal, Gamma, G) :-
    \+(concolic_helper:cl(A, _Body)), !,
    findall(K, (concolic_helper:cl(B, _), get_atom_label(B, K)), ListAllLabels),
    traces(Traces), (
        member(Trace, Traces) -> true;
        alts(SGoal, Gamma, B, [], ListAllLabels, G, NewGoals),
        concolic_helper:update_pending_test_cases(NewGoals),
        retractall(traces(_)),
        assertz(traces([Trace | Traces]))
    ),

    concolic_helper:update_testcases(CGoal, Trace).

% for debugging purpose
eval(_, _, _, _, _, _) :- 
    writeln("ERROR: Impossible to find an evaluation rule to apply"),
    fail.

change_label(Label, [_], [Label]) :- !.
change_label(Label, [X | R], [X | RR]) :- change_label(Label, R, RR).

get_atom_label(A, Label) :- A =.. [_F | Args], last(Args, Label).

get_atom_label(A, Pred, Label) :-
    A =.. [_ | ArgsLab],
    last(ArgsLab, Label),!,
    concolic_helper:del_dump_label(A, Pred).

get_new_goals([], []).
get_new_goals([foo(Atom, _, _) | R], [Atom | RR]) :- get_new_goals(R, RR).

% useless ???
get_new_trace(Trace, Alts, Traces, NewTrace) :-
    prefix(PTrace, Trace), length(PTrace, N),
    nth0(N, Alts, (_, L)),
    oset_power(L, LPower),
    v_printf("All possibilities: ~w~n", [LPower]),
    v_printf("Visited traces:    ~w~n", [Traces]),
    member(Labels, LPower),  %% nondeterministic!!
    append(PTrace, [Labels], NewTrace),
    v_printf("~w~n", [\+(member(OtherTrace, Traces), prefix(NewTrace, OtherTrace))]),
    \+(member(OtherTrace, Traces), prefix(NewTrace, OtherTrace)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Searching for alternative goals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alts(SGoal, Gamma, Atom, Labels, AllLabels, G, NewGoals) :-
    oset_power(AllLabels, LPower),
    depthk(K),

    findall(NewGoal, (
        member(LPos, LPower), LPos \== Labels,
        subtract(AllLabels, LPos, LNeg),
        get_constraints(Atom, G, LPos, LNeg, NewConstr),
        append(Gamma, NewConstr, Constr),
        copy_term(foo(SGoal, G), foo(NewGoal, GCopy)),
        G = GCopy,
        context(N),
        solve(N, G, Constr, _Mod),
        depth(SGoal, Depth),
        Depth =< K + 1),
        NewGoals
    ).

depth(T, D) :-
    compound(T) -> aggregate(max(B + 1), P ^ A ^ (arg(P, T, A), depth(A, B)), D) ; D = 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Writing the constraints in a correct form for the Z3 interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mymember(X, [Y | _]) :- X == Y, !.
mymember(X, [_ | R]) :- mymember(X, R).

mysubtract([], _, []).
mysubtract([V | R], G, NG) :- mymember(V, G), mysubtract(R, G, NG).
mysubtract([V | R], G, [V | NG]) :- \+(mymember(V, G)), mysubtract(R, G, NG).

get_constraints(A, G, LPos, LNeg, Constrs) :-
    get_list_clauses(LPos, HPos),
    get_list_clauses(LNeg, HNeg),
    concolic_helper:del_dump_label(A, ANoLabel),
    term_variables(ANoLabel, VarA),
    term_variables(G, VarsToBeGrounded),
    mysubtract(VarA, VarsToBeGrounded, VarsNotGrounded),
    get_pos_consts(ANoLabel, VarsNotGrounded, HPos, PosConsts),
    get_neg_consts(ANoLabel, VarsNotGrounded, HNeg, NegConsts),
    append(PosConsts, NegConsts, Constrs).

get_list_clauses([], []).
get_list_clauses([L | LList], [H | HList]) :-
    findall(H, (concolic_helper:cl(V, _), get_atom_label(V, H, L)), [H]),
    get_list_clauses(LList, HList).

get_pos_consts(_, _, [], []).
get_pos_consts(A, VarsNotGrounded, [H | T], Constrs) :-
    copy_term(H, CArgs),
    term_variables(CArgs, VCArgs),
    exists_terms(VCArgs, A, VarsNotGrounded, CArgs, C1),
    get_pos_consts(A, VarsNotGrounded, T, C2),
    Constrs = [C1 | C2].

get_neg_consts(_, _, [], []).
get_neg_consts(A, VarsNotGrounded, [H | T], Constrs) :-
    copy_term(H, CArgs),
    term_variables(CArgs, VCArgs),
    forall_terms(VCArgs, A, VarsNotGrounded, CArgs, C1),
    get_neg_consts(A, VarsNotGrounded, T, C2),
    Constrs = [C1 | C2].

exists_terms([], A, VarsNotGrounded, Args, Pred):-
    Pred_ = (A = Args),
    exists_terms_atom(VarsNotGrounded, Pred_, Pred).
exists_terms([H | T], A, VarsNotGrounded, Args, Pred) :-
    exists_terms(T, A, VarsNotGrounded, Args, Pred1),
    Pred = exists(var(H), Pred1).

exists_terms_atom([], Pred, Pred).
exists_terms_atom([V | Vars], Pred1, Pred3) :-
    exists_terms_atom(Vars, Pred1, Pred2),
    Pred3 = exists(var(V), Pred2).

forall_terms([], A, VarsNotGrounded, Args, Pred):-
    Pred_ = (A \= Args),
    forall_terms_atom(VarsNotGrounded, Pred_, Pred).
forall_terms([H | T], A, VarsNotGrounded, Args, Pred) :-
    forall_terms(T, A, VarsNotGrounded, Args, Pred1),
    Pred = forall(var(H), Pred1).

forall_terms_atom([], Pred, Pred).
forall_terms_atom([V | Vars], Pred1, Pred3) :-
    forall_terms_atom(Vars, Pred1, Pred2),
    Pred3 = forall(var(V), Pred2).

update_list_constraints([], []).
update_list_constraints([C | Constr], [NC | NewConstr]) :-
  update_constraint(C, NC),
  update_list_constraints(Constr, NewConstr).

update_constraint((forall(var(V), C)), (forall(var(V), NewConstr))) :-
  var(V), !,
  update_constraint(C, NewConstr).
update_constraint((forall(var([H | T]), C)), NewConstr) :-
  update_constraint(C, NewConstr_),
  NewConstr = (forall(var(H), (forall(var(T), NewConstr_)))).
update_constraint(C, C).