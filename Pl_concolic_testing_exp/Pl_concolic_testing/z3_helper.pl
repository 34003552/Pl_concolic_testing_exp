%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dealing with Z3 contexts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(z3_helper, [
	context/1,
	z3_init_context/1, z3_clear_context/1,
	z3_mk_term_type/4,
	solve/4,

	%z3_mk_context/1,
	z3_pop/2, 

	z3_intconstr2smtlib/5, z3_termconstr2smtlib/5, 

    get_varnames/2, get_context_vars/2, assert_vars/2
	]).

:- use_module(swiplz3).

:- dynamic context/1. %% current context

z3_init_context(N) :-
    z3_mk_config,
    z3_set_param_value("model", "true"),
    z3_mk_context(N),
    z3_mk_solver(N),
    assertz(context(N)),
    z3_del_config.

z3_clear_context(N) :-
    z3_del_solver(N),
    z3_del_context(N),
    retractall(context(N)).

z3h_mk_vars(_N, [], []).
z3h_mk_vars(N, [VNH | VNT], [VTH | VTT]) :-
	z3_mk_vars(N, [VNH], VTH),
	z3h_mk_vars(N, VNT, VTT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Z3 solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(N, _VarsToBeGrounded, Constr, Model) :-
    z3_push(N),
    /* Declaring constraints to solve*/
    /* To improve efficiency, we could only declare grounded variable, but it needs some C changes...
    %numbervars(VarsToBeGrounded),
    %get_varnames(VarsToBeGrounded,VarsStr),*/

    (typifier:has_type_checker(_File) -> (
    		%writeln("point A"),
    		typifier:z3_termconstrW2smtlib(N, [], Constr, VarsSTR, VarsTy, Csmtlib),
    		%writeln("point B"),
    		z3h_mk_vars(N, VarsSTR, VarsTy),
    		z3_assert_term_string(N, Csmtlib, false, false)
    	) ; (
    		z3_termconstr2smtlib(N, [], Constr, VarsSTR, Csmtlib),
    		(VarsSTR = [] -> true ; z3_mk_term_vars(N, VarsSTR)),
    		%write("SMT = "),writeln(Csmtlib),
    		(concolic_helper:integer -> Int = true ; Int = false),
    		(concolic_helper:list -> List = true ; List = false),
    		z3_assert_term_string(N, Csmtlib, Int, List)
    	)
    ),

    /* checking satisfiability */
    (z3_check(N) ->
        z3_print_model(N, Model),
        get_context_vars(N, VVS),
        get_model_varT_eval(N, VVS, Values),
        term_variables(Constr, AllVars),
        z3_pop(N, VarsSTR),
        AllVars = Values;
        z3_pop(N, VarsSTR),
        false
    ).

/* ---- ? ---- */

z3_mk_context(N) :-
    z3_mk_new_context(N),
    retractall(var(N, _)).

retract_var(_, []).
retract_var(N, [H|T]):-
    retract(var(N, H)),
    retract_var(N, T).

z3_pop(N,Vars):-
    retract_var(N, Vars),
    z3_pop(N).

:- dynamic var/2.

/*
   get_varnames/2 takes a list of variables produced by numbervars
   and returns a list of strings
*/
get_varnames([], []).
get_varnames([V|VR], [VN|VRN]) :-
    write_to_chars(V, VN_),
    string_codes(VN, VN_),
    get_varnames(VR, VRN).

/*
    intconstr2smtlib/5 takes a context, the constraints so far, a new constraint
    (over integers) and returns a list of strings with the names of the new variables
    and a string with the SMTLIB2 representation "(assert ... )"
*/
z3_intconstr2smtlib(Context, OldC, C, NewVarsStr, SMT) :-
    copy_term((OldC, C),(OldCC, CC)),
    term_variables(OldCC, OldCCVars),
    term_variables(CC, CCVars),
    numbervars((OldCCVars, CCVars)),
    subtract(CCVars, OldCCVars, NewVars),
    get_varnames(NewVars, NewVarsStr), (
        NewVarsStr=[] -> true ;
        assert_vars(Context,NewVarsStr)
    ),
    constr2smt(CC, SMT_),
    string_codes(SMT, SMT_), !.



assert_vars(_, []).
assert_vars(N, [X|R]) :-
    assertz(var(N, X)),
    assert_vars(N, R).

assert_terms(_, []).
assert_terms(N, [(X, Y)|R]) :-
    assertz(term(N, X, Y)),
    assert_terms(N, R).

get_context_vars(N, VVS) :-
    findall(VV, var(N, VV), VVS).



var_decl([], []).
var_decl([V|R], SS) :-
    format(codes(S), "(declare-const ~w Int) ", [V]),
    var_decl(R, RS),
    append(S, RS, SS).

/*
    constr2smt/2 translates a list of simple integer constraints (>,<,=,\=,>=,=< and +,-,*,div,mod,rem)
    to a list of codes representing an SMTLIB2 string
*/
constr2smt([C], SMT) :-
    !, con2smt(C, SMT).
constr2smt(List, SMT) :-
    con2smt_list(List, SMT_),
    format(codes(SMT), "(and ~s)", [SMT_]).

con2smt_list([C], SMT) :-
    !,con2smt(C, SMT).
con2smt_list([C|R],SMT) :-
    con2smt(C, SMT1),
    con2smt_list(R, SMTR),
    format(codes(SMT), "~s ~s", [SMT1, SMTR]).

/* expression rooted by a binary operator */
con2smt(T, SMT) :-
    functor(T, F, 2), !, transf(F, Fmt),
    arg(1, T, Arg1), con2smt(Arg1, SMT1),
    arg(2, T, Arg2), con2smt(Arg2, SMT2),
    format(codes(SMT), Fmt, [SMT1, SMT2]).

/* negative number */
con2smt(T, SMT) :-
    functor(T, -, 1), !, transf(-, Fmt),
    arg(1, T, Arg1),con2smt(Arg1, SMT1),
    format(codes(SMT), Fmt, [SMT1]).

/* integer */
con2smt(T, SMT) :-
    integer(T), !,
    atom_codes(T, SMT).

/* variable */
con2smt(T, SMT) :-
    functor(T, '$VAR', 1), !,
    write_to_chars(T, SMT).

/* unsupported term */
con2smt(T, _SMT) :-
    throw(unsupported_constraint(T)).

/* binary operators */
transf(>, "(> ~s ~s)").
transf(<, "(< ~s ~s)").
transf(>=, "(>= ~s ~s)").
transf(=<, "(<= ~s ~s)").
transf(=, "(= ~s ~s)").
transf(\=, "(not (= ~s ~s))").
transf(*, "(* ~s ~s)").
transf(+, "(+ ~s ~s)").
transf(-, "(- ~s ~s)").
transf(div, "(div ~s ~s)").
transf(mod, "(mod ~s ~s)").
transf(rem, "(rem ~s ~s)").

/* unary operators */
transf(-, "(- ~s)").

/*
    z3_termconstr2smtlib/5 takes a context, the constraints so far, a
    new constraint (over terms and predicates) and returns a list of strings
    with the names of the new variables and a string with the SMTLIB2
    representation "(assert ... )"
*/
z3_termconstr2smtlib(Context, OldC, C, NewVarsStr, SMT) :-

    term_variables(OldC, OldCVars),
    term_variables(C, CVars),

    subtract(CVars, OldCVars, NewVars),
    copy_term((C, NewVars),(CC, CNewVars)),
    numbervars(CNewVars),

    get_varnames(CNewVars, NewVarsStr), (
        NewVarsStr=[] -> true ;
        assert_vars(Context,NewVarsStr)
    ),
    constrP2smt(CC, _, SMT_),
    string_codes(SMT, SMT_), !.

/*
    constrP2smt/2 translates a list of simple constraints (=,\=) over predicates
    to a list of codes representing an SMTLIB2 string
*/
constrP2smt([C], LT, SMT) :-
    !, conP2smt(C, LT, SMT).
constrP2smt(List, LT, SMT) :-
    conP2smt_list(List, LT, SMT_),
    format(codes(SMT), "(and ~s)", [SMT_]).

conP2smt_list([C], LT, SMT) :-
    !, conP2smt(C, LT, SMT).
conP2smt_list([C|R], LT, SMT) :-
    conP2smt(C, LT1, SMT1),
    conP2smt_list(R, LT2, SMT2),
    append(LT1, LT2, LT),
    format(codes(SMT), "~s ~s", [SMT1, SMT2]).

/*peano_s(T, V, Var, Val) :-
    functor(T, s, 1),
    arg(1, T, Arg1),
    (peano_s(Arg1, V, V + 1, Val) ; Var = Arg1, Val = V + 1).*/

/* expression rooted by a binary operator */
conP2smt(T, LT, SMT) :-
    functor(T, F, 2), transfT(F, Fmt), !,
    arg(1, T, Arg1), conP2smt(Arg1, LT1, SMT1),
    arg(2, T, Arg2), conP2smt(Arg2, LT2, SMT2),
    append(LT1, LT2, LT),
    format(codes(SMT), Fmt, [SMT1, SMT2]).

/* var declaration */
conP2smt(T, LT, SMT) :-
    functor(T, var, 1), !, transfT(var, Fmt),
    arg(1, T, Arg1), conP2smt(Arg1, LT, SMT1),
    format(codes(SMT), Fmt, [SMT1]).

/* variable */
conP2smt(T, LT, SMT) :-
    functor(T, '$VAR', 1), !,
    LT = [],
    write_to_chars(T, SMT).

/* peano s */
/*conP2smt(T, LT, SMT) :-
    peano_s(T, 0, Var, Val), !,
    LT = [],
    format(codes(SMT), "(term_from_int (+ (term_as_int ~w) ~d))", [Var, Val]).*/

/* integer */
conP2smt(T, LT, SMT) :-
    integer(T), !,
    LT = [],
    atom_codes(T, SMT_),
    format(codes(SMT), "(term_from_int ~s)", [SMT_]).

/* float */
/* boolean */
/* pair */

/* nil */
conP2smt(T, LT, SMT) :-
    T = [], !, LT = [],
    format(codes(SMT), "(cons nil)", []).

/* list */
conP2smt(T, LT, SMT) :-
    functor(T, '[|]', 2), !,
    get_args_list(T, 1, LTH, Head),
    get_args_list(T, 2, LTT, Tail),
    append(LTH, LTT, LT),
    format(codes(SMT), "(cons (insert ~s (list ~s)))", [Head, Tail]).

/* term/0 */
conP2smt(T, LT, SMT) :-
    functor(T, N, 0),!,
    LT = [(N,0)],
    write_to_chars(N, SMT).

/* term/Arity */
conP2smt(T, LT, SMT) :-
    functor(T, N, Arity), !,
    write_to_chars(N, SMT1),
    list_of_args(T, Arity, LT_, SMT2),
    LT = [(N, Arity)|LT_],
    format(codes(SMT), "(~s ~s)", [SMT1, SMT2]).

/* unsupported term */
conP2smt(T, LT, _SMT) :-
    LT = [],
    throw(unsupported_constraint(T)).


/* Take the Nth arguments of the functor T (useful for lists) */
get_args_list(T, N, LT, SMT) :-
    arg(N, T, A),
    conP2smt(A, LT, SMT).

/* Create the list of the arguments of the functor T */
list_of_args(T, 1, LT, Args):-
    arg(1, T, A),
    conP2smt(A, LT, Args).

list_of_args(T, I, LT, Args) :-
    I_ is (I - 1),
    list_of_args(T, I_, LT1, Args_),
    arg(I, T, A),
    conP2smt(A, LT2, SMT),
    format(codes(Args), "~s ~s", [Args_, SMT]),
    append(LT1, LT2, LT).

/* binary operators */
/*
transfT(>, "(> ~s ~s)").
transfT(<, "(< ~s ~s)").
transfT(>=, "(>= ~s ~s)").
transfT(=<, "(<= ~s ~s)").
transfT(*, "(* ~s ~s)").
transfT(+, "(+ ~s ~s)").
transfT(-, "(- ~s ~s)").
transfT(div, "(div ~s ~s)").
transfT(mod, "(mod ~s ~s)").
transfT(rem, "(rem ~s ~s)").
*/
transfT(=, "(= ~s ~s)").
transfT(\=, "(not (= ~s ~s))").
transfT(forall, "(forall ~s ~s)").
transfT(exists, "(exists ~s ~s)").

/* unary operators */
transfT(var, "((~s Term))").