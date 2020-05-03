%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  SWIProlog-Z3 interface
%
%  Only tested in SWI Prolog, http://www.swi-prolog.org/
%
%  Copyright (c) 2020 Sophie Fortz
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(swiplz3, [get_varnames/2, z3_mk_config/0, z3_set_param_value/2, z3_mk_context/1, z3_mk_solver/1,
    z3_del_config/0, z3_del_solver/1, z3_del_context/1, z3_push/1, z3_pop/2, z3_assert_int_string/2,
    z3_assert_term_string/4, z3_intconstr2smtlib/5, z3_termconstr2smtlib/5, z3_check/1, z3_mk_int_vars/2,
	z3_mk_term_type/4, z3_mk_term_vars/2, z3_print_model/2, get_context_vars/2, get_model_var_eval/3,
    get_model_varT_eval/3]).

:- use_foreign_library(swiplz3).

:- type term0 ---> (any, any).
:- type term1 ---> to_term1(integer) ; to_term1(any) ; to_term1(list(any)).
:- type term2 ---> (any, integer).

:- trust_pred functor(any, unk_t0, integer).
%:- trust_pred atom_codes(any, any).
%:- trust_pred numbervars(list(any)).
:- trust_pred z3_mk_new_context(any).
:- trust_pred retract(any).
:- trust_pred retractall(any).
:- trust_pred z3_pop(any).
:- trust_pred z3_assert_int_string_(any, any).
:- trust_pred z3_assert_term_string_(any, any, boolean, boolean).
:- trust_pred assertz(any).
:- trust_pred z3_get_model_intvar_eval(any, any, any).
:- trust_pred z3_get_model_termvar_eval(any, any, any).
%:- trust_pred term_variables(list(any), any).
:- trust_pred string_codes(any, any).
%:- trust_pred append(any, any, any).
:- trust_pred write_to_chars(any, any).
%:- trust_pred subtract(any, any, any).

:- pred z3_mk_context(any).
z3_mk_context(N) :-
    z3_mk_new_context(N),
    retractall(var(N, _)).

:- pred retract_var(any, list(any)).
retract_var(_, []).
retract_var(N, [H|T]):-
    retract(var(N, H)),
    retract_var(N, T).

:- pred z3_pop(any, list(any)).
z3_pop(N,Vars):-
    retract_var(N, Vars),
    z3_pop(N).

:- pred var(any, any).
:- dynamic var/2.

/*
   get_varnames/2 takes a list of variables produced by numbervars
   and returns a list of strings
*/
:- pred get_varnames(list(any), list(any)).
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
:- pred z3_intconstr2smtlib(any, any, any, list(any), any).
z3_intconstr2smtlib(Context, OldC, C, NewVarsStr, SMT) :-
    copy_term((OldC, C),(OldCC, CC)),
    term_variables(OldCC, OldCCVars)::term_variables(list(any), any),
    term_variables(CC, CCVars)::term_variables(list(any), any),
    numbervars((OldCCVars, CCVars))::numbervars(any),
    subtract(CCVars, OldCCVars, NewVars)::subtract(any, any, list(any)),
    get_varnames(NewVars, NewVarsStr), (
        NewVarsStr=[] -> true ;
        assert_vars(Context,NewVarsStr)
    ),
    constr2smt(CC, SMT_),
    string_codes(SMT, SMT_), !.

/*
    z3_assert_int_string/2 takes an SMT formula with integer variables and asserts it to a context
*/
:- pred z3_assert_int_string(any, any).
z3_assert_int_string(N, SMT) :-
    string_codes(SMT, SMTC),
    string_codes("(assert ", S1),
    string_codes(")", S2),
    append(S1, SMTC, S3)::append(any, any, any),
    append(S3, S2, SMTLIB2_)::append(any, any, any),
    string_codes(SMTLIB2, SMTLIB2_),
    z3_assert_int_string_(N, SMTLIB2).


/*
    z3_assert_term_string/2 takes an SMT formula with term variables and asserts it to a context
*/
:- pred z3_assert_term_string(any, any, boolean, boolean).
z3_assert_term_string(N, SMT, Int, List) :-
    string_codes(SMT, SMTC),
    string_codes("(assert ", S1),
    string_codes(")", S2),
    append(S1, SMTC, S3)::append(any, any, any),
    append(S3, S2, SMTLIB2_)::append(any, any, any),
    string_codes(SMTLIB2, SMTLIB2_),
    z3_assert_term_string_(N, SMTLIB2, Int, List).

:- pred assert_vars(any, list(any)).
assert_vars(_, []).
assert_vars(N, [X|R]) :-
    assertz(var(N, X)),
    assert_vars(N, R).

:- pred assert_terms(any, list(any)).
assert_terms(_, []).
assert_terms(N, [(X, Y)|R]) :-
    assertz(term(N, X, Y)),
    assert_terms(N, R).

:- pred get_context_vars(any, list(any)).
get_context_vars(N, VVS) :-
    findall(VV, var(N, VV), VVS).

:- pred get_model_var_eval(any, list(any), list(any)).
get_model_var_eval(_, [], []) :- !.
get_model_var_eval(N, [Var|R], [Val|RR]) :-
    z3_get_model_intvar_eval(N, Var, Val),
    get_model_var_eval(N, R, RR).

:- pred get_model_varT_eval(any, list(any), list(any)).
get_model_varT_eval(_, [], []) :- !.
get_model_varT_eval(N, [Var|R], [Val|RR]) :-
    z3_get_model_termvar_eval(N, Var, Val),
    get_model_varT_eval(N, R, RR).

:- pred var_decl(list(any), list(any)).
var_decl([], []).
var_decl([V|R], SS) :-
    string_codes("(declare-const ", S1),
    write_to_chars(V, Var),
    append(S1, Var, S2)::append(any, any, any),
    string_codes(" Int) ", S3),
    append(S2, S3, S)::append(any, any, any),
    var_decl(R, RS),
    append(S, RS, SS)::append(any, list(any), list(any)).

/*
    constr2smt/2 translates a list of simple integer constraints (>,<,=,\=,>=,=< and +,-,*,div,mod,rem)
    to a list of codes representing an SMTLIB2 string
*/
:- pred constr2smt(list(any), any).
constr2smt([C], SMT) :-
    !,con2smt(to_term1(C), SMT).
constr2smt(List, SMT) :-
    con2smt_list(List, SMT_),
    string_codes("(and ", S1),
    append(S1, SMT_, S2)::append(any, any, any),
    string_codes(")", S3),
    append(S2, S3, SMT)::append(any, any, any).

:- pred con2smt_list(list(any), any).
con2smt_list([C], SMT) :-
    !,con2smt(to_term1(C), SMT).
con2smt_list([C|R],SMT) :-
    con2smt(to_term1(C), SMT1),
    con2smt_list(R, SMTR),
    string_codes(" ", Blank),
    append(SMT1, Blank, S)::append(any, any, any),
    append(S, SMTR, SMT)::append(any, any, any).

:- pred con2smt(term1, any).

/* expression rooted by a binary operator */
con2smt(to_term1(T), SMT) :-
    functor(T, F, 2), !, transf(F,S1,S2),
    arg(1, T, Arg1),con2smt(to_term1(Arg1), SMT1),
    arg(2, T, Arg2),con2smt(to_term1(Arg2), SMT2),
    string_codes(" ", Blank),
    append(S1, SMT1, S)::append(any, any, any), append(S, Blank, S_)::append(any, any, any),
    append(S_, SMT2, S__)::append(any, any, any), append(S__, S2, SMT)::append(any, any, any).

/* negative number */
con2smt(to_term1(T), SMT) :-
    functor(T, -, 1), !, transf(-, S1, S2),
    arg(1, T, Arg1),con2smt(to_term1(Arg1), SMT1),
    append(S1, SMT1, S)::append(any, any, any),append(S, S2, SMT)::append(any, any, any).

/* integer */
con2smt(to_term1(T), SMT) :-
    integer(T), !,
    atom_codes(T, SMT)::atom_codes(integer, any).

/* variable */
con2smt(to_term1(T), SMT) :-
    functor(T, '$VAR', 1),!,
    write_to_chars(T, SMT).

/* unsupported term */
con2smt(to_term1(T), _SMT) :-
    throw(unsupported_constraint(T)).

:- pred transf(any, any, any).

/* binary operators */
transf(>, S1, S2) :- string_codes("(> ", S1), string_codes(")", S2).
transf(<, S1, S2) :- string_codes("(< ", S1), string_codes(")", S2).
transf(>=, S1, S2) :- string_codes("(>= ", S1), string_codes(")", S2).
transf(=<, S1, S2) :- string_codes("(<= ", S1), string_codes(")", S2).
transf(=, S1, S2) :- string_codes("(= ", S1), string_codes(")", S2).
transf(\=, S1, S2) :- string_codes("(not (= ", S1), string_codes("))", S2).
transf(*, S1, S2) :- string_codes("(* ", S1), string_codes(")", S2).
transf(+, S1, S2) :- string_codes("(+ ", S1), string_codes(")", S2).
transf(-, S1, S2) :- string_codes("(- ", S1), string_codes(")", S2).
transf(div, S1, S2) :- string_codes("(div ", S1), string_codes(")", S2).
transf(mod, S1, S2) :- string_codes("(mod ", S1), string_codes(")", S2).
transf(rem, S1, S2) :- string_codes("(rem ", S1), string_codes(")", S2).

/* unary operators */
transf(-, S1, S2) :- string_codes("(- ", S1), string_codes(")", S2).

/*
    z3_termconstr2smtlib/5 takes a context, the constraints so far, a
    new constraint (over terms and predicates) and returns a list of strings
    with the names of the new variables and a string with the SMTLIB2
    representation "(assert ... )"
*/
:- pred z3_termconstr2smtlib(any, list(any), list(any), list(any), any).
z3_termconstr2smtlib(Context, OldC, C, NewVarsStr, SMT) :-

    term_variables(OldC, OldCVars)::term_variables(list(any), any),
    term_variables(C, CVars)::term_variables(list(any), any),

    subtract(CVars, OldCVars, NewVars)::subtract(any, any, any),
    copy_term((C, NewVars),(CC, CNewVars)),
    numbervars(CNewVars)::numbervars(list(any)),

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
:- pred constrP2smt(list(any), list(term2), any).
constrP2smt([C], LT, SMT) :-
    !, conP2smt(to_term1(C), LT, SMT).
constrP2smt(List, LT, SMT) :-
    conP2smt_list(List, LT, SMT_),
    string_codes("(and ", S1),
    append(S1, SMT_, S2)::append(any, any, any),
    string_codes(")", S3),
    append(S2, S3, SMT)::append(any, any, any).

:- pred conP2smt_list(list(any), list(term2), any).
conP2smt_list([C], LT, SMT) :-
    !,conP2smt(to_term1(C), LT, SMT).
conP2smt_list([C|R], LT, SMT) :-
    conP2smt(to_term1(C), LT1, SMT1),
    conP2smt_list(R, LT2, SMT2),
    string_codes(" ", Blank),
    append(SMT1, Blank, S)::append(any, any, any),
    append(LT1, LT2, LT)::append(list(term2), list(term2), list(term2)),
    append(S, SMT2, SMT)::append(any, any, any).

:- pred conP2smt(term1, list(term2), any).

/* expression rooted by a binary operator */
conP2smt(to_term1(T), LT, SMT) :-
    functor(T, F, 2),
    transfT(F, S1, S2), !,
    arg(1, T, Arg1), conP2smt(to_term1(Arg1), LT1, SMT1),
    arg(2, T, Arg2), conP2smt(to_term1(Arg2), LT2, SMT2),
    string_codes(" ", Blank),
    append(S1, SMT1, S)::append(any, any, any), append(S, Blank, S_)::append(any, any, any),
    append(S_, SMT2, S__)::append(any, any, any), append(S__, S2, SMT)::append(any, any, any),
    append(LT1, LT2, LT)::append(list(term2), list(term2), list(term2)).

/* var declaration */
conP2smt(to_term1(T), LT, SMT) :-
    functor(T, var, 1), !, transfT(var, S1, S2),
    arg(1, T, Arg1),conP2smt(to_term1(Arg1), LT, SMT1),
    append(S1, SMT1, S)::append(any, any, any), append(S, S2, SMT)::append(any, any, any).

/* variable */
conP2smt(to_term1(T), LT, SMT) :-
    functor(T, '$VAR', 1),!,
    LT = [],
    write_to_chars(T, SMT).

/* integer */
conP2smt(to_term1(T),LT,SMT) :-
    integer(T), !,
    LT = [],
    atom_codes(T, SMT_)::atom_codes(integer, any),
    string_codes("(term_from_int ", S1),
    string_codes(")", S2),
    append(S1, SMT_, S)::append(any, any, any), append(S,S2,SMT)::append(any, any, any).

/* list */
conP2smt(to_term1(T), LT, SMT) :-
    functor(T, '[|]', 2), !,
    get_args_list(T, 1, LTH, Head),
    get_args_list(T, 2, LTT, Tail),
    append(LTH, LTT, LT)::append(list(term2), list(term2), list(term2)),
    string_codes(Head, SMT1), % string E
    string_codes(Tail, SMT2), % string F
    string_codes("(cons (insert ", S1),
    string_codes(" (list ", S2),
    string_codes(")))", S3),
    append(S1, SMT1, S)::append(any, any, any), append(S, S2, S_)::append(any, any, any),
    append(S_, SMT2, S__)::append(any, any, any), append(S__, S3, SMT)::append(any, any, any).

/* term/0 */
conP2smt(to_term1(T), LT, SMT) :-
    functor(T, N, 0),!,
    LT = [(N,0)],
    write_to_chars(N, SMT).

/* term/Arity */
conP2smt(to_term1(T), LT, SMT) :-
    functor(T, N, Arity), !,
    write_to_chars(N, SMT1),
    list_of_args(T, Arity, LT_, SMT2),
    string_codes(" ", Blank),
    string_codes("(", S1),string_codes(")", S2),
    append(S1, SMT1, S)::append(any, any, any), append(S, Blank, SBlank)::append(any, any, any),
    append(SBlank, SMT2, S_)::append(any, any, any), append(S_, S2, SMT)::append(any, any, any),
    LT = [(N, Arity)|LT_].

/* unsupported term */
conP2smt(to_term1(T), LT, _SMT) :-
    LT = [],
    throw(unsupported_constraint(T)).


/* Take the Nth arguments of the functor T (useful for lists) */
:- pred get_args_list(any, integer, list(term2), any).
get_args_list(T, N, LT, SMT) :-
    arg(N, T, A),
    conP2smt(to_term1(A), LT, SMT).

:- pred list_of_args(any, integer, list(term2), any).

/* Create the list of the arguments of the functor T */
list_of_args(T, 1, LT, Args):-
    arg(1, T, A),
    conP2smt(to_term1(A), LT, Args).

list_of_args(T, I, LT, Args) :-
    I_ is (I - 1),
    list_of_args(T, I_, LT1, Args_),
    arg(I, T, A),
    conP2smt(to_term1(A), LT2, SMT),
    string_codes(" ", Blank),
    append(Args_, Blank, SBlank)::append(any, any, any), append(SBlank, SMT, Args)::append(any, any, any),
    append(LT1, LT2, LT)::append(list(term2), list(term2), list(term2)).

:- pred transfT(any, any, any).

/* binary operators */
/*
transfT(>,S1,S2) :- string_codes("(> ",S1),string_codes(")",S2).
transfT(<,S1,S2) :- string_codes("(< ",S1),string_codes(")",S2).
transfT(>=,S1,S2) :- string_codes("(>= ",S1),string_codes(")",S2).
transfT(=<,S1,S2) :- string_codes("(<= ",S1),string_codes(")",S2).
transfT(*,S1,S2) :- string_codes("(* ",S1),string_codes(")",S2).
transfT(+,S1,S2) :- string_codes("(+ ",S1),string_codes(")",S2).
transfT(-,S1,S2) :- string_codes("(- ",S1),string_codes(")",S2).
transfT(div,S1,S2) :- string_codes("(div ",S1),string_codes(")",S2).
transfT(mod,S1,S2) :- string_codes("(mod ",S1),string_codes(")",S2).
transfT(rem,S1,S2) :- string_codes("(rem ",S1),string_codes(")",S2).*/
transfT(=, S1, S2) :- string_codes("(= ", S1), string_codes(")",S2).
transfT(\=, S1, S2) :- string_codes("(not (= ", S1), string_codes("))",S2).
transfT(forall, S1, S2) :- string_codes("(forall ", S1), string_codes(")",S2).
transfT(exists, S1, S2) :- string_codes("(exists ", S1), string_codes(")",S2).

/* unary operators */
transfT(var, S1, S2) :- string_codes("((", S1),string_codes(" Term))", S2).
