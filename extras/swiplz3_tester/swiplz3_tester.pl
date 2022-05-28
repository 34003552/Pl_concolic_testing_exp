
:- use_module('../../toolbox/gen_tools').
:- use_module('../../swiplz3/swiplz3').
:- use_module(smtlib2_parser).


:- dynamic context_id/1.

init_context :-
    z3_mk_config(),
    z3_set_param_value("model", "true"),
    z3_mk_context(ContextID),
    z3_mk_solver(ContextID),
    z3_del_config(),
    assert(context_id(ContextID)).

del_context :-
    context_id(ContextID),
    z3_del_solver(ContextID),
    z3_del_context(ContextID),
    retract(context_id(ContextID)).


catch_errors(Call) :- catch(Call, swiplz3_error(FuncName, Error, Message), (
        writeln("The swiplz3 module has crashed!"),
        format("An exception occurred while calling '~s'~n", [FuncName]),
        format("Error: ~s~nDetails: ~s~n", [Error, Message]),
        fail
    )
).

guard_argc(Instruction, Argc == N) :- N == 0, !,
    (Argc == N -> true ; format(string(S), "'~s' expects no arguments!", [Instruction]), throw(S)).
guard_argc(Instruction, Argc == N) :-
    (Argc == N -> true ; format(string(S), "'~s' expects ~d arguments!", [Instruction, N]), throw(S)).
guard_argc(Instruction, Argc >= N) :- 
    (Argc >= N -> true ; format(string(S), "'~s' expects at least ~d arguments!", [Instruction, N]), throw(S)).
guard_argc(Instruction, Guard) :-
    format(string(S), "'~s' has an ill-formed guard (~w)!", [Instruction, Guard]), throw(S).

% When calling a swiplz3 primitive, a type parameter should be parameterized like this
% Type<Arg0,Arg1,...> (without parentheses or spaces) instead of (Type Arg0 Arg1 ...)
% because the assertion parser will not be able to delimit tokens properly otherwise.

smt_to_swiplz3_sort(SMTType, SwiplType) :- 
    (is_list(SMTType) ->
        [VTH | VTT] = SMTType,
        convlist(smt_to_swiplz3_sort, VTT, VTT_),
        atomics_to_string(VTT_, ",", VTT__),
        format(string(SwiplType), "~a<~w>", [VTH, VTT__])
    ;
        atom_string(SMTType, SwiplType)
    ).

smt_to_swiplz3_datatypes(SMTTypes, SwiplTypes) :-
    convlist([Type, Type_]>>(
        [TName | ICTs] = Type,
        [TName_ | OCTs] = Type_,
        atom_string(TName, TName_),
        convlist([Ct, Ct_]>>(\+is_list(Ct) ->
            atom_string(Ct, Ct_)
        ;
            [CName | IACs] = Ct,
            [CName_ | OACs] = Ct_,
            atom_string(CName, CName_),
            convlist([Ac, Ac_]>>(
                [AName, AType] = Ac,
                [AName_, AType_] = Ac_,
                atom_string(AName, AName_),
                smt_to_swiplz3_sort(AType, AType_)
            ), IACs, OACs)
        ), ICTs, OCTs)
    ), SMTTypes, SwiplTypes).

smt_to_swiplz3_assertion(ArgNList, Assertion) :-
    (   ArgNList = ['as', Symbol, Type], !,
        smt_to_swiplz3_assertion(Symbol, Symbol_),
        smt_to_swiplz3_sort(Type, Type_),
        format(string(Assertion), "(as ~a ~s)", [Symbol_, Type_])
    ;   ArgNList = [Quantifier, Vars, Expr], member(Quantifier, [forall, exists]), !,
        convlist([[VName, VType],S]>>(
            smt_to_swiplz3_sort(VType, VType_),
            format(string(S), "(~a ~s)", [VName, VType_])
        ), Vars, M),
        atomics_to_string(M, "", W),
        smt_to_swiplz3_assertion(Expr, Assertion_),
        format(string(Assertion), "(~a (~s) ~s)", [Quantifier, W, Assertion_])
    ;   is_list(ArgNList), !,
        convlist(smt_to_swiplz3_assertion, ArgNList, M),
        atomics_to_string(M, " ", W),
        format(string(Assertion), "(~w)", [W])
    ;
        atom_string(ArgNList, Assertion)
    ).

/** Z3 interpreter **/

execute(Instruction, Args) :-
    context_id(ContextID),
    length(Args, Argc),
    (   Instruction == "push", guard_argc(Instruction, Argc == 0), !,
        z3_push(ContextID)

    ;   Instruction == "pop", guard_argc(Instruction, Argc == 0), !,
        z3_pop(ContextID)

    ;   Instruction == "declare-datatypes", guard_argc(Instruction, Argc == 2), !,
        [OptTypenames_, Datatypes_] = Args,
        convlist(atom_string, OptTypenames_, OptTypenames),
        smt_to_swiplz3_datatypes(Datatypes_, Datatypes),
        z3_mk_datatypes(ContextID, OptTypenames, Datatypes)

    ;   Instruction == "declare-sort", guard_argc(Instruction, Argc == 1), !,
        [SortName_] = Args,
        atom_string(SortName_, SortName),
        z3_mk_sort(ContextID, SortName, "")

    ;   Instruction == "define-sort", guard_argc(Instruction, Argc == 3), !,
        [SortName_, OptTypeNames_, BaseName_] = Args,
        smt_to_swiplz3_sort([SortName_ | OptTypeNames_], SortName),
        smt_to_swiplz3_sort(BaseName_, BaseName),
        z3_mk_sort(ContextID, SortName, BaseName)

    ;   Instruction == "$declare-consts", guard_argc(Instruction, Argc == 2), !,
        [VarNames_, VarType_] = Args,
        convlist(atom_string, VarNames_, VarNames),
        smt_to_swiplz3_sort(VarType_, VarType),
        z3_mk_vars(ContextID, VarNames, VarType)

    ;   Instruction == "declare-const", guard_argc(Instruction, Argc == 2), !,
        [VarName, VarType] = Args,
        execute("$declare-consts", [[VarName], VarType])

    ;   Instruction == "declare-fun", guard_argc(Instruction, Argc == 3), !,
        [FuncName_, FuncArgs_, FuncRet_] = Args,
        atom_string(FuncName_, FuncName),
        convlist(smt_to_swiplz3_sort, FuncArgs_, FuncArgs),
        smt_to_swiplz3_sort(FuncRet_, FuncRet),
        z3_declare_fun(ContextID, FuncName, FuncArgs, FuncRet)

    ;   Instruction == "define-fun", guard_argc(Instruction, Argc == 4), !,
        [FuncName_, FuncArgs_, FuncRet_, FuncBody_] = Args,
        atom_string(FuncName_, FuncName),
        convlist([[N, T], [N_, T_]]>>(atom_string(N, N_), smt_to_swiplz3_sort(T, T_)), FuncArgs_, FuncArgs),
        smt_to_swiplz3_sort(FuncRet_, FuncRet),
        smt_to_swiplz3_assertion(FuncBody_, FuncBody),
        z3_define_fun(ContextID, FuncName, FuncArgs, FuncRet, FuncBody)

    ;   Instruction == "define-fun-rec", guard_argc(Instruction, Argc == 4), !,
        [FuncName_, FuncArgs_, FuncRet_, FuncBody_] = Args,
        atom_string(FuncName_, FuncName),
        convlist([[N, T], [N_, T_]]>>(atom_string(N, N_), smt_to_swiplz3_sort(T, T_)), FuncArgs_, FuncArgs),
        smt_to_swiplz3_sort(FuncRet_, FuncRet),
        smt_to_swiplz3_assertion(FuncBody_, FuncBody),
        z3_define_fun_rec(ContextID, FuncName, FuncArgs, FuncRet, FuncBody)

    ;   Instruction == "assert", guard_argc(Instruction, Argc == 1), !,
        [ArgNList] = Args,
        smt_to_swiplz3_assertion(ArgNList, Assertion),
        z3_assert_string(ContextID, Assertion)

    ;   Instruction == "check-sat", guard_argc(Instruction, Argc == 0), !,
        z3_check_sat(ContextID, Result),
        (   Result == 'unsat', !, writeln("UNSAT")
        ;   Result == 'sat', !, writeln("SAT")
        ;   Result == 'unknown', !, writeln("UNKNOWN")
        )

    ;   Instruction == "get-model", guard_argc(Instruction, Argc == 0), !,
        z3_get_model_to_string(ContextID, String),
        format("(model~n~s)~n", [String])

    ;   Instruction == "$eval-model-const", guard_argc(Instruction, Argc == 1), !,
        [VarName_] = Args,
        atom_string(VarName_, VarName),
        z3_eval_model_var(ContextID, VarName, VarValue),
        format(string(String), '(define-const ~s Term ~w)', [VarName, VarValue]),
        writeln(String)

    ;   Instruction == "reset", guard_argc(Instruction, Argc == 0), !,
        del_context(),
        init_context()

    ;
        format(string(String), "Unrecognized instruction: ~s", [Instruction]),
        throw(String)
    ).


read_line(S) :- get_char(C), (char_type(C, end_of_line) -> S = [] ; read_line(S_), S = [C | S_]).

/** Main program **/

main :-
    writeln("-----------------------------"),
    writeln("Welcome to the swiplz3 tester"),
    writeln("-----------------------------"),

    current_prolog_flag(argv, Args),
    (Args == [] ->
        %throw("You must specify a script to load!")
        writeln("You did not specify a script to load from the command-line!"),
        ScriptDir = "scripts",
        format("Thus, I let you now select a script from 'extras/swiplz3_tester/~s': ", [ScriptDir]), nl,
        read_line(ScriptFile), nl, (ScriptFile == [] -> halt ; true),
        format(string(ScriptPath), "./~s/~s", [ScriptDir, ScriptFile]),
        (exists_file(ScriptPath) -> true ; format("The file '~s' does not exist.~n", [ScriptPath]), main, halt)
    ;
        [ScriptPath | _] = Args
    ),
    format("Loading file '~s'~n", [ScriptPath]), nl,

    init_context(),
    smtlib2_parser:parse_file(ScriptPath, execute),
    del_context(),

    halt.