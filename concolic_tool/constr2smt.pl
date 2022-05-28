
:- module(constr2smt, [
    use_legacy_typing/0,
    regctor/3,
    termconstr2smtlib/4
]).

:- use_module('../toolbox/gen_tools').

:- dynamic use_legacy_typing/0.
:- dynamic msg_type_inference/0.

dt_translate(T, T_) :- var(T), !,
    (use_legacy_typing -> 
        (\+msg_type_inference ->
            print_message(warning, "Incomplete type inference - Term used by default!"),
            assertz(msg_type_inference)
        ; true),
        T_ = "Term"
    ;
        (\+msg_type_inference ->
            print_message(warning, "Incomplete type inference!"),
            assertz(msg_type_inference)
        ; true),
        T_ = "_?_"
    ).

dt_translate(boolean, "Bool") :- !.
dt_translate(integer, "Int") :- !.
dt_translate(float, "Real") :- !.
dt_translate(string, "String") :- !.
dt_translate(list(T), Type) :- !,
    dt_translate(T, T_),
    format(string(Type), "List<~s>", [T_]).
dt_translate(pair(T0, T1), Type) :- !,
    dt_translate(T0, T0_), dt_translate(T1, T1_),
    format(string(Type), "Pair<~s,~s>", [T0_, T1_]).
dt_translate(F, Type) :- F =.. [FName | FArgs], FArgs \= [], !,
    maplist(dt_translate, FArgs, FArgs_),
    atomics_to_string(FArgs_, ",", Ts),
    format(string(Type), "~a<~s>", [FName, Ts]).
dt_translate(T, Type) :- atom_string(T, Type).


:- dynamic regctor/3.

:- dynamic required_funcs/5.

/*
    termconstr2smtlib/4 takes a list of constraints (over terms and predicates) with the list of
    its variables to ground as (VName:_VType) and returns the list of typed variables as (VName:VType)
    with the list of required functions and a string with the SMTLIB2 representation "(assert ...)"
*/
termconstr2smtlib(CC, NewVars, RqFuncs, SMT) :-
    %writeln(a-CC),
    maplist(parse_constr, CC, CC_),

    %writeln(b-CC_),
    maplist(constr2smt, CC_, SMTs),
    atomics_to_string(SMTs, " ", SMT_),
    ((length(SMTs, NSMTs), NSMTs < 2) -> SMT = SMT_ ;
        format(string(SMT), "(and ~s)", [SMT_])
    ),
    !,
    %writeln(c-SMT),

    maplist([VarS:VarType]>>(atom_string(VarA, VarS), get_smt_var(VarA, VarType)), NewVars),
    clear_smt_vars,

    findall(func(F_, Args_, Ret_, Body, Rec), (
        required_funcs(F, Args, Ret, Body, Rec), length(Args, Len), format(string(F_), "'~a'/~d", [F, Len]), 
        maplist(dt_translate, [Ret | Args], [Ret_ | Args_])
    ), RqFuncs_),
    list_to_set(RqFuncs_, RqFuncs),
    retractall(required_funcs(_,_,_,_,_)).


:- dynamic temp_smt_var/1.

link_smt_var(VName, VType) :-
    with_output_to(atom(VName_), write(VName)),
    nb_linkval(VName_, VType),
    assertz(temp_smt_var(VName_)).

get_smt_var(VName, VType) :-
    with_output_to(atom(VName_), write(VName)),
    nb_current(VName_, VType).

clear_smt_vars :-
    forall(temp_smt_var(VName), nb_delete(VName)), 
    retractall(temp_smt_var(_)).


parse_constr(T, 'var'(VName)>>VType) :- T = '$VAR'(_), !,
    VName = T,
    link_smt_var(VName, VType).
parse_constr(T, 'quant'(QName, QVars, QExpr)>>boolean) :- T =.. [QName, Vars, Expr], member(QName, ['forall', 'exists']), is_list(Vars), !,
    maplist(parse_constr, Vars, QVars),
    parse_constr(Expr, QExpr), QExpr = _>>boolean.
parse_constr(T, 'val'(VValue)>>VType) :- integer(T), !,
    VValue = T, (use_legacy_typing -> VType = 'Term' ; VType = integer).
parse_constr(T, 'val'(VValue)>>VType) :- float(T), !,
    VValue = T, (use_legacy_typing -> VType = 'Term' ; VType = float).
parse_constr(T, 'val'(VValue)>>VType) :- (T = true ; T = false), \+use_legacy_typing, !,
    VValue = T, VType = boolean.
parse_constr(T, 'val'(VValue)>>VType) :- string(T), !,
    VValue = T, (use_legacy_typing -> VType = 'Term' ; VType = string).
parse_constr(T, 'func'(FName, FArgs)>>FType) :- T =.. [FName | FArgs_], (FName == [] ; atom(FName)), !,
    maplist(parse_constr, FArgs_, FArgs__),
    maplist([_>>FArgT, FArgT]>>true, FArgs__, FArgTs),

    (is_builtin_func(T) ->
        (builtin_func(sig(FName, FArgTs, FType), _)
        ;
            (use_legacy_typing -> % while using legacy typing, built-in functions without body can be fully inferred!
                maplist(=('Term'), FArgTs), FType = 'Term'
            ; fail % to find somewhere in the lpmux database
            )
        ; true),
        FArgs = FArgs__
    ;
        ((regctor(FName, FArgTs_, FType), same_length(FArgs__, FArgTs_)) *->
            maplist([FArg_, T1, FArg]>>(
                FArg_ = _>>T0,
                (T0 = T1, FArg = FArg_, ! ;
                    use_legacy_typing, T0 == 'Term', T1 == 'TermList', !, FArg = 'func'('$list', [FArg_])>>T1 ;
                    use_legacy_typing, T0 == 'TermList', T1 == 'Term', !, FArg = 'func'('$cons', [FArg_])>>T1 ;
                    fail /*FArg = 'func'('cast', [FArg_])>>T1*/)
            ), FArgs__, FArgTs_, FArgs)
        ;
            length(FArgs_, Arity),
            format("Invalid functor: ~w/~d~n", [FName, Arity]),
            forall(regctor(FName, UX0, UX1), format("> available option: ~w -> ~w~n", [UX0, UX1])),
            FArgs = FArgs__
        )
    ).
parse_constr(T,_) :- format(string(Error), "Unable to parse ~p!", [T]), throw(Error).


constr2smt('var'(VName)>>VType, SMT) :- !,
    with_output_to(string(SMT), write(VName)),
    get_smt_var(VName, VType).
constr2smt('quant'(QName, QVars, QExpr)>>_QType, SMT) :- !,
    convlist([I,O]>>(
        constr2smt(I, VSMT),
        I = 'var'(_VName)>>VType,
        dt_translate(VType, VType_),
        format(atom(O), "(~w ~a)", [VSMT, VType_])
    ), QVars, QVSMTs), atomics_to_string(QVSMTs, "", QVSMT),
    constr2smt(QExpr, SMT_),
    (QVSMT == "" -> SMT = SMT_; % maybe useless
        format(string(SMT), "(~a (~s) ~s)", [QName, QVSMT, SMT_])
    ).
constr2smt('val'(VValue)>>_VType, SMT) :- !,
    (\+use_legacy_typing -> term_string(VValue, SMT) ;
        (integer(VValue), !,
            (regctor('$term_from_int', [integer], 'Term') ->
                format(string(SMT), "(term_from_int ~d)", [VValue])
            ;
                format(string(SMT), "|'~di'/0|", [VValue])
                %format(string(SMT), "|'~w'/0|", [VValue])
            )
        ;float(VValue), !,
            throw("legacy typing does not allow float values")
        ;
            term_string(VValue, SMT)
        )
    ).
constr2smt('func'(FName, FArgs)>>FType, SMT) :- !,
    maplist([FArg, FAType, FASMT]>>(FArg = _>>FAType, constr2smt(FArg, FASMT)), FArgs, FATypes, FASMTs),

    (builtin_func(sig(FName, FATypes, FType_), Body/i) *->
        expand_inline(Body, FASMTs, SMT__),
        (ground(FType_) -> SMT = SMT__ ; dt_translate(FType, W), format(string(SMT), "(as ~s ~s)", [SMT__, W])),
        FType = FType_
    ;
        (builtin_func(sig(FName, FATypes, FType), Body_) ->
            (Body_ = Body/r -> Rec = true ; Body = Body_, Rec = false),
            assertz(required_funcs(FName, FATypes, FType, Body, Rec))
        ;
            (regctor(FName, FATypes, FType) -> true ;
                Body = "", Rec = false,
                assertz(required_funcs(FName, FATypes, FType, Body, Rec))
            )
        ),
        length(FATypes, FArity),
        format(atom(FName_), "|'~a'/~d|", [FName, FArity]),
        atomics_to_string(FASMTs, " ", SMT_),
        (SMT_ == "" -> SMT = FName_ ; format(string(SMT), "(~s ~s)", [FName_, SMT_]))
    ).
constr2smt(_, _SMT) :- throw("invalid element!").

is_numeric_t(T) :- member(T, [integer, float]).

builtin_func(sig('+', [T, T], T), "(+ x!0 x!1)"/i) :- is_numeric_t(T).
builtin_func(sig('-', [T, T], T), "(- x!0 x!1)"/i) :- is_numeric_t(T).
builtin_func(sig('*', [T, T], T), "(* x!0 x!1)"/i) :- is_numeric_t(T).
builtin_func(sig('/', [T, T], T), "(/ x!0 x!1)"/i) :- is_numeric_t(T).
builtin_func(sig('//', [T, T], T), "(ite (>= x!0 0) (div x!0 x!1) (div (- x!0) (- x!1)))") :- is_numeric_t(T).
builtin_func(sig(div, [T, T], T), "(ite (>= x!1 0) (div x!0 x!1) (- (div (- x!0) x!1)))") :- is_numeric_t(T).
builtin_func(sig(mod, [T, T], T), "(ite (>= x!1 0) (mod x!0 x!1) (- (mod (- x!0) x!1)))") :- is_numeric_t(T).
builtin_func(sig(rem, [T, T], T), "(ite (>= x!0 0) (rem (abs x!0) (abs x!1)) (- (rem (abs x!0) (abs x!1))))") :- is_numeric_t(T).

builtin_func(sig('=', [T, T], boolean), "(= x!0 x!1)"/i).
builtin_func(sig('\\=', [T, T], boolean), "(not (= x!0 x!1))"/i).
builtin_func(sig('is', [T, T], boolean), "(= x!0 x!1)"/i) :- is_numeric_t(T).
builtin_func(sig('=:=', [T, T], boolean), "(= x!0 x!1)"/i) :- is_numeric_t(T).
builtin_func(sig('>', [T, T], boolean), "(> x!0 x!1)"/i) :- is_numeric_t(T).
builtin_func(sig('<', [T, T], boolean), "(< x!0 x!1)"/i) :- is_numeric_t(T).
builtin_func(sig('>=', [T, T], boolean), "(>= x!0 x!1)"/i) :- is_numeric_t(T).
builtin_func(sig('=<', [T, T], boolean), "(<= x!0 x!1)"/i) :- is_numeric_t(T).

builtin_func(sig('-', [T0, T1], pair(T0, T1)), "(mk-pair x!0 x!1)"/i) :- \+use_legacy_typing.
builtin_func(sig([], [], 'Term'), "(cons nil)"/i) :- use_legacy_typing.
%builtin_func(sig([], [], 'TermList'), "nil"/i) :- use_legacy_typing.
builtin_func(sig([], [], list(_)), "nil"/i) :- \+use_legacy_typing.
builtin_func(sig('[|]', ['Term', 'Term'], 'Term'), "(cons (insert x!0 (list x!1)))"/i) :- use_legacy_typing.
%builtin_func(sig('[|]', ['Term', 'TermList'], 'TermList'), "(insert x!0 x!1)"/i) :- use_legacy_typing.
builtin_func(sig('[|]', [T0, T1], T1), "(insert x!0 x!1)"/i) :- \+use_legacy_typing, T1 = list(T0).

builtin_func(sig(',', [boolean, boolean], boolean), "(and x!0 x!1)"/i).
builtin_func(sig(';', [boolean, boolean], boolean), "(or x!0 x!1)"/i).

is_builtin_func(T) :- predicate_property(T, built_in) ; functor(T, N, _A), (N == [] ; current_op(_, _, N)).

expand_inline(Body, Args, SMT) :-
    length(Args, Arity),
    gen_tools:generate_list([I,I]>>true, Arity, Indexes),
    foldl([Index, I, O]>>(
        nth0(Index, Args, Arg),
        with_output_to(string(Arg_), write(Arg)),
        format(string(Pat), "\\bx!~d\\b", [Index]),
        re_replace("\\$"/g, "$n", Arg_, Arg__), % disable captures!
        re_replace(Pat/g, Arg__, I, O)
    ), Indexes, Body, SMT).