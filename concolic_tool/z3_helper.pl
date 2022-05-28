%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dealing with Z3 contexts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(z3_helper, [
    configure_z3/0,
    init_context/1, clear_context/1,

    solve/3
]).

:- use_module('../swiplz3/swiplz3').
:- use_module('constr2smt').

configure_z3 :-
    z3_set_param_value("model", "true"),
    z3_set_param_value("timeout", "1000"),

    retractall(current_z3types(_)),
    retractall(constr2smt:msg_type_inference).

init_context(Ctx) :-
    z3_mk_context(Ctx),
    z3_mk_solver(Ctx),

    retractall(var(Ctx, _)),

    dispose_context(Ctx).

:- dynamic required_unktypes/1.
dispose_context(Ctx) :-
    get_current_z3types(Z3Types),
    foreach(required_unktypes(UnkType), z3_mk_sort(Ctx, UnkType, "")),
    z3_mk_datatypes(Ctx, [], Z3Types).

clear_context(Ctx) :-
    z3_del_solver(Ctx),
    z3_del_context(Ctx),

    retractall(declared_funcs(_,_,_)).


build_legacy_z3types(TermDesc, Z3Types) :-
    TermDesc = term_desc(Terms, Preds, Int, List),
    exclude([(TName, TArity)]>>(
        current_op(_, _, TName),
        constr2smt:builtin_func(sig(TName,_,RetT), _), RetT \= '_pred_'
    ), Terms, Terms_),

    (\+Int -> TermCtorInt = [] ;
        TermCtorInt = ["term_from_int", ["term_as_int", "Int"]],
        assertz(constr2smt:regctor('$term_from_int', [integer], 'Term'))
    ),
    (\+List -> TermListType = [], TermCtorList = [] ;
        TermListType = ["TermList", ["nil"], ["insert", ["head", "Term"], ["tail", "TermList"]]],
        TermCtorList = ["cons", ["list", "TermList"]],
        assertz(constr2smt:regctor('$nil', [], 'TermList')),
        assertz(constr2smt:regctor('$insert', ['Term', 'TermList'], 'TermList')),
        assertz(constr2smt:regctor('$cons', ['TermList'], 'Term')),
        assertz(constr2smt:regctor('$list', ['Term'], 'TermList'))
    ),

    convlist([(TName, TArity), [CName | Accs]]>>(
        format(string(CName), "'~a'/~d", [TName, TArity]),
        gen_tools:generate_list([I, [AName, "Term"]]>>format(string(AName), "~s_arg_~d", [CName, I]), TArity, Accs),
        gen_tools:generate_list([I, 'Term']>>true, TArity, ATypes),
        assertz(constr2smt:regctor(TName, ATypes, 'Term'))
    ), Terms_, TermCtorFunc),
    exclude([[]]>>true, [TermCtorInt | [TermCtorList | TermCtorFunc]], TermCtors),
    TermType = ["Term" | TermCtors],

    convlist([(PName, PArity), [CName | Accs]]>>(
        functor(Head, PName, PArity), \+predicate_property(Head, built_in),
        format(string(CName), "'~a'/~d", [PName, PArity]),
        gen_tools:generate_list([I, [AName, "Term"]]>>format(string(AName), "~s_arg_~d", [CName, I]), PArity, Accs),
        gen_tools:generate_list([I, 'Term']>>true, PArity, ATypes),
        assertz(constr2smt:regctor(PName, ATypes, '_pred_'))
    ), Preds, PredCtors),
    PredType = ["_pred_" | PredCtors],

    exclude([[]]>>true, [TermType, TermListType, PredType], Z3Types).


instantiate_types_aux(T0, L) :-
    lpmux:current_types(CT_), CT_ = Type_-_Enum_,
    subsumes_term(Type_, T0), !,
    copy_term(CT_, CT), CT = T0-_Enum__, T0 =.. [_N | A],
    convlist(instantiate_types_aux, A, L_), append(L_, L__),
    append(L__, [CT], L).

instantiate_types(InstTypes) :-
    findall(TE, (
        lpmux:current_predtypes(PT), PT =.. [_Name | Types],
        convlist(instantiate_types_aux, Types, TE_), append(TE_, TE)
    ), InstTypes_),
    append(InstTypes_, InstTypes__),
    list_to_set(InstTypes__, InstTypes).

is_builtin_type(Type_) :- member(Type_, [boolean, integer, float, string, list(_), pair(_,_)]).

build_current_z3types(Z3Types) :-
    instantiate_types(InstTypes),
    findall([Type | Enum], (
        (member(Type_-Enum_, InstTypes),
            constr2smt:dt_translate(Type_, Type),
            (nonvar(Enum_) -> true ;
                (is_builtin_type(Type_) -> fail ;
                    (required_unktypes(Type) -> true ; assertz(required_unktypes(Type))), fail
                )
            )
        ;
            findall(PT, lpmux:current_predtypes(PT), Enum_), Enum_ \= [], Type = "_pred_"
        ),
        convlist([Ctor_, Ctor]>>(
            Ctor_ =.. [CName_ | CArgs_],
            atom_string(CName_, CName__),
            length(CArgs_, CArity),
            format(string(CName), "'~s'/~d", [CName__, CArity]),
            gen_tools:generate_list([I,O]>>format(string(O), "~s_arg_~d", [CName, I]), CArity, ANames),
            convlist(constr2smt:dt_translate, CArgs_, ATypes),
            maplist([AName, AType, [AName, AType]]>>true, ANames, ATypes, Accs),
            (Accs = [] -> Ctor = CName ; Ctor = [CName | Accs]),
            assertz(constr2smt:regctor(CName_, CArgs_, Type_))
        ), Enum_, Enum)
    ), Z3Types).

:- dynamic current_z3types/1.

get_current_z3types(Z3Types) :-
    (current_z3types(Z3Types) -> true ;
        retractall(constr2smt:regctor(_,_,_)),
        ((lpmux:current_language('prolog'), \+lpmux:use_experimental_pltyper) ->
            assertz(constr2smt:use_legacy_typing),
            lpmux:get_legacy_term_descriptor(TermDesc),
            build_legacy_z3types(TermDesc, Z3Types)
        ;
            retractall(constr2smt:use_legacy_typing),
            build_current_z3types(Z3Types)
        ),
        assert(current_z3types(Z3Types))
    ).

mk_vars(Ctx, Vars) :-
    foldl([VarNs:VarT, Vs, RemVs]>>(
        [VarN0:VarT | Vs_] = Vs,
        partition({VarT}/[VarN:VarT_]>>(VarT_ == VarT), Vs_, TVs_, RemVs),
        maplist([VarN:VarT, VarN]>>true, TVs_, VarNs_),
        VarNs = [VarN0 | VarNs_]
    ), Vars_, Vars, []), !,
    foreach(member(Vs:T, Vars_), (constr2smt:dt_translate(T, T_), z3_mk_vars(Ctx, Vs, T_))).

unwrap_ctors(T, T_) :-
    T =.. [F | Args],
    length(Args, A),
    maplist(unwrap_ctors, Args, Args_),
    ((F/A = 'term_from_int'/1 ; F/A = 'cons'/1), !, [T_] = Args_
    ; F/A = 'nil'/0, !, T_ = []
    ; F/A = 'insert'/2, !, T_ =.. ['[|]' | Args_]
    ;(F/A = 'mk-pair'/2, Op = '-'), !, T_ =.. [Op | Args_]
    ;
        re_matchsub("^'(?<name>.*)'/(?<arity>[0-9]+)$"/a, F, W, []), !,
        (\+atom_number(W.get(arity), A) -> 
            throw("Arguments given to functor do not match its arity")
        ;
            (   re_matchsub("^(?<number>[0-9]+)i$"/a, W.get(name), WW, []), !,
                atom_number(WW.get(number), N),
                (A = 0 -> T_ = N ; throw("Arity of a number has to be 0"))
            ;
                T_ =.. [W.get(name) | Args_]
            )
        )
    ; T_ =.. [F | Args_]).

eval_model_vars(_, [], []) :- !.
eval_model_vars(N, [Var|R], [Val|RR]) :-
    z3_eval_model_var(N, Var, Val_),
    unwrap_ctors(Val_, Val),
    eval_model_vars(N, R, RR).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Z3 solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic var/2.

assert_vars(Ctx, Vars) :-
    foreach(member(VN:_VT, Vars), assertz(var(Ctx, VN))).

get_context_vars(Ctx, VVS) :-
    findall(VV, var(Ctx, VV), VVS).

retract_vars(Ctx, Vars) :-
    foreach(member(VN:_VT, Vars), retract(var(Ctx, VN))).

:- dynamic declared_funcs/3.

solve(Ctx, VarsToBeGrounded, Constr) :-
    z3_push(Ctx),
    ignore((
        termconstrW2smtlib([], Constr, VarsToBeGrounded, Vars, RqFuncs, Csmtlib),

        (Vars = [] -> true ;
            assert_vars(Ctx, Vars),
            mk_vars(Ctx, Vars)
        ),

        foreach((member(func(F_, Args_, Ret_, Body, Rec), RqFuncs), \+declared_funcs(F_, Args_, Ret_)), (
            z3_mk_func(Ctx, F_, Args_, Ret_, Body, Rec)
            %assertz(declared_funcs(F_, Args_, Ret_))
        )),

        %write("SMT = "), writeln(Csmtlib),
        z3_assert_string(Ctx, Csmtlib),

        /* checking satisfiability */
        (z3_check_sat(Ctx, sat) ->
            get_context_vars(Ctx, VVS),
            z3_get_model_to_string(Ctx, _Model),
            eval_model_vars(Ctx, VVS, Values),
            VarsToBeGrounded = Values,
            Ret = true
        ;
            Ret = false
        ),
        retract_vars(Ctx, Vars)
    )),
    z3_pop(Ctx),
    (ground(Ret) -> Ret ; throw("Assertion solving has failed!")).

termconstrW2smtlib(OldC, C, VarsToBeGrounded, NewVarsST, RqFuncs, SMT) :-
    term_variables(OldC, OldCVars),
    term_variables(C, CVars),

    subtract(CVars, OldCVars, NewVars),
    copy_term((C, VarsToBeGrounded, NewVars), (CC, CVarsToBeGrounded, CNewVars)),
    numbervars(CNewVars),

    include({CVarsToBeGrounded}/[I]>>member(I, CVarsToBeGrounded), CNewVars, CNewVars_),

    maplist([V, VName:_VType]>>with_output_to(string(VName), write(V)), CNewVars_, NewVarsST),
    constr2smt:termconstr2smtlib(CC, NewVarsST, RqFuncs, SMT).