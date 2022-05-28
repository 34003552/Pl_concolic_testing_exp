
:- module(concolic_helper, [
    main/6,

    format_constraints/5,
    update_constraints/2,
    
    display_initial_info/3,
    display_success/0
]).

:- use_module('../lpmux/lpmux').
:- use_module(z3_helper).

error_process(time_limit_exceeded) :- !, display_timeout.
error_process(X) :- print_message(error, X), halt.

:- dynamic with_trace/0.

main(ConcreteGoal, Ground, Depth, Timeout, WithTrace, File) :-
    retractall(with_trace), (WithTrace -> assertz(with_trace) ; true),

    lpmux:load_program(File), % File is the input program

    z3_helper:z3_mk_config,
    z3_helper:configure_z3,

    catch((call_with_time_limit(Timeout,
        concolic_tester:mainT(ConcreteGoal, Ground, Depth)
    ), display_success), X, error_process(X)),
    display_pred_coverage,

    z3_helper:z3_del_config,

    lpmux:unload_program.


write_pretty_term(Term) :-
    numbervars(Term, 0, _),
    write_term(Term, [numbervars(true)]),
    fail.
write_pretty_term(_Term).

display_initial_info(CGoal, SGoal, GroundVars) :-
    %% initial info:
    v_format("Initial goal:          ~@~n", [write_pretty_term(CGoal)]),
    v_format("Symbolic initial goal: ~@~n", [write_pretty_term(SGoal)]),
    v_format("Ground variables:      ~@~n", [write_pretty_term(GroundVars)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Writing the constraints in a correct form for the Z3 interface (2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_constraints(A, VarsNotGrounded, HPos, HNeg, Constrs) :-
    maplist({A, VarsNotGrounded}/[H, 'exists'(Vars, A = CArgs)]>>(
        copy_term(H, CArgs),
        term_variables(CArgs, VCArgs),
        append(VCArgs, VarsNotGrounded, Vars)
    ), HPos, PosConsts),
    maplist({A, VarsNotGrounded}/[H_, 'forall'(Vars_, A \= CArgs_)]>>(
        copy_term(H_, CArgs_),
        term_variables(CArgs_, VCArgs_),
        append(VCArgs_, VarsNotGrounded, Vars_)
    ), HNeg, NegConsts),
    append(PosConsts, NegConsts, Constrs).

update_constraint(C, C_) :- C =.. [Q, Vs, F], member(Q, ['forall', 'exists']), is_list(Vs), !,
    term_variables(Vs, Vs_), update_constraint(F, F_), C_ =.. [Q, Vs_, F_].
update_constraint(C, C).

update_constraints(Cs, Cs_) :- maplist(update_constraint, Cs, Cs_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate coverage measuring
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

estimate_pred_coverage_aux(Mod:Pred, TestCases, MaxDepth, CovN/CovD) :-
    (predicate_property(Pred, built_in) -> CovN = 1, CovD = 1 ;
        findall(CovN_/CovD_, (
            clause(Mod:Pred, _, Ref0),
            lpmux:get_clause_body_as_list(Mod:Pred, Body),
            findall(TestCaseBody, (
                member(TestCase, TestCases), \+predicate_property(TestCase, built_in),
                clause(Mod:TestCase, _, Ref1), Ref0 = Ref1,
                %writeln(p-Pred-TestCase-Ref0),
                lpmux:get_clause_body_as_list(Mod:TestCase, TestCaseBody)
            ), TestCaseBodies),
            %writeln(b-Body-TestCaseBodies),
            (Body == [] ->
                (TestCaseBodies = [] -> CovN_ = 0 ; CovN_ = 1), CovD_ = 1
            ;
                (MaxDepth > 0 ->
                    MaxDepth_ is MaxDepth - 1,
                    length(Body, BodyLen), gen_tools:generate_list([I,I]>>true, BodyLen, BodyIndexes),
                    foldl([I, CovN0_/CovD0_, CovN2_/CovD2_]>>(
                        nth0(I, Body, G),
                        findall(G_, (member(TestCaseBody, TestCaseBodies), nth0(I, TestCaseBody, G_)), TestCases_),
                        estimate_pred_coverage_aux(Mod:G, TestCases_, MaxDepth_, CovN1_/CovD1_),
                        CovN2_ is CovN0_ + CovN1_, CovD2_ is CovD0_ + CovD1_
                    ), BodyIndexes, 0/0, CovN_/CovD_)
                ;
                    CovN_ = 0, CovD_ = 0
                )
            )
        ), Aleph),

        ((member(TestCase, TestCases), (predicate_property(TestCase, built_in) ->
            \+Mod:TestCase ; \+clause(Mod:TestCase, _, _))) -> CovN_ = 1, CovD_ = 1 ;
            CovN_ = 0,
            % attempt to build a naive failing test case
            copy_term(Pred, Pred_),
            term_variables(Pred_, PredVars),
            maplist(=('ø'), PredVars),
            (clause(Mod:Pred_, _, _) -> CovD_ = 0 ; CovD_ = 1)
        ),

        foldl([CovN0_/CovD0_, CovN1_/CovD1_, CovN2_/CovD2_]>>(
            CovN2_ is CovN0_ + CovN1_, CovD2_ is CovD0_ + CovD1_
        ), [CovN_/CovD_ | Aleph], 0/0, CovN/CovD)
    ).

estimate_pred_coverage(PCov) :-
    concolic_tester:testcases(Solution),
    %writeln(Solution),
    maplist({P}/[testcase(A, _Trace), A]>>(functor(A, N, Ar), functor(P, N, Ar)), Solution, Kappa),
    %writeln(P-Kappa),
    
    concolic_tester:depthk(K),
    catch(call_with_time_limit(10, (
        estimate_pred_coverage_aux(instrumented_program:P, Kappa, K, PCov)
    )), E, PCov = 'NA'(E)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Printing test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_success :-
    nl, writeln("Procedure complete!"), nl,
    concolic_tester:testcases(Solution),
    write_testcases(Solution).

%% This one prints both test cases and pending test cases:

display_timeout :-
    nl, writeln("Time limit exceeded!"),
    concolic_tester:testcases(Cases),  %% processed test cases
    reverse(Cases, CasesR),

    (Cases == [] -> true ;
        nl, writeln("Processed test cases: "),
        write_testcases(CasesR)
    ),

    findall(Goal, concolic_tester:pending_test_case(Goal), PendingCases), %% pending tests cases
    (PendingCases == [] -> true ;
        nl, writeln("Pending test cases: "),
        list_to_set(PendingCases, PendingCasesL), %% this is just to remove duplicates
        reverse(PendingCasesL, PendingCasesLR),
        maplist([A]>>(write_pretty_term(A), nl), PendingCasesLR)
    ), !.

display_pred_coverage :-
    estimate_pred_coverage(PCov),
    nl, write("Estimated predicate coverage: "),
    (PCov = 'NA'(E) -> format("NA (~w)", [E]) ;
        PCov = PCovN/PCovD,
        (PCovD is 0 -> PCov_ is nan ; PCov_ is 100 * PCov),
        format("~2f% (~d/~d)", [PCov_, PCovN, PCovD])
    ), nl.

write_testcases([]).
write_testcases([testcase(A, Trace) | R]) :-
    write_pretty_term(A),
    (\+with_trace -> true ;
        write(" with trace "),
        (Trace == [] -> write("{}") ;
            [TraceH | TraceT] = Trace,
            write("{"),
            write_trace_step(TraceH),
            maplist([S]>>(write(" >> "), write_trace_step(S)), TraceT),
            write("}")
        )
    ), nl, write_testcases(R).

write_trace_step(l(P, N, K)) :- format("(~w/~w,~w)", [P, N, K]).
    