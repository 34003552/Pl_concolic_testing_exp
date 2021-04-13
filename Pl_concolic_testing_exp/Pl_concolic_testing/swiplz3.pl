%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  SWIProlog-Z3 interface
%
%  Only tested in SWI Prolog, http://www.swi-prolog.org/
%
%  Copyright (c) 2020 Sophie Fortz
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(swiplz3, [
        z3_mk_config/0, z3_del_config/0, z3_set_param_value/2, z3_mk_new_context/1, z3_del_context/1, z3_mk_solver/1,
        z3_del_solver/1, z3_push/1, z3_pop/1, z3_mk_int_vars/2, z3_mk_term_type/4, z3_mk_term_vars/2,
        z3_assert_int_string/2, z3_assert_term_string/4, z3_check/1, z3_print_model/2,

        get_model_var_eval/3, get_model_varT_eval/3,

        z3_mk_datatypes/3, z3_mk_vars/3
    ]).

%:- use_module(library(prolog_stack)).
:- use_foreign_library(swiplz3).

/* ---- ? ---- */

%dt_translate_aux([], _).
dt_translate_aux([H], S) :- !, dt_translate(H, S).
dt_translate_aux([H | T], S) :- dt_translate(H, S0), dt_translate_aux(T, S1), format(string(S), "~w ~w", [S0, S1]).

dt_translate("boolean", "Bool") :- !.
dt_translate("integer", "Int") :- !.
dt_translate("float", "Real") :- !.
dt_translate("list"-[T], Type) :- !, dt_translate(T, U),
    format(string(Type), "List<~w>", [U]).
dt_translate("pair"-[T0, T1], Type) :- !, dt_translate(T0, U0), dt_translate(T1, U1),
    format(string(Type), "Pair<~w,~w>", [U0, U1]).
dt_translate(N-L, Type) :- !, dt_translate_aux(L, Us),
    format(string(Type), "~w<~w>", [N, Us]).
dt_translate(Type, Type).

dt_ct_ac_filter([Name, Type], [Name, Type_]) :- dt_translate(Type, Type_).

dt_ct_acs_filter([], []).
dt_ct_acs_filter([Ac | IT], [Ac_ | OT]) :- dt_ct_ac_filter(Ac, Ac_), dt_ct_acs_filter(IT, OT).

dt_ct_filter(Name, Name) :- \+is_list(Name).
dt_ct_filter([Name | IACs], [Name | OACs]) :- dt_ct_acs_filter(IACs, OACs).

dt_cts_filter([], []).
dt_cts_filter([Ct | IT], [Ct_ | OT]) :- dt_ct_filter(Ct, Ct_), dt_cts_filter(IT, OT).

dt_filter([Name | ICTs], [Name_ | OCTs]) :- dt_translate(Name, Name_), dt_cts_filter(ICTs, OCTs).

dts_filter([], []).
dts_filter([Dt | IT], [Dt_ | OT]) :- dt_filter(Dt, Dt_), dts_filter(IT, OT).

z3_mk_datatypes(Ctx, Tpls, Dts) :-
    %writeln(Dts),
    dts_filter(Dts, Dts_),
    %writeln(Dts_),
    z3_mk_datatypes_(Ctx, Tpls, Dts_).

z3_mk_vars(Ctx, VarNames, VarType) :-
    dt_translate(VarType, VarType_),%format("yui: ~w ~w~n", [VarType, VarType_]),
    z3_mk_vars_(Ctx, VarNames, VarType_).



get_model_var_eval(_, [], []) :- !.
get_model_var_eval(N, [Var|R], [Val|RR]) :-
    z3_get_model_intvar_eval(N, Var, Val),
    get_model_var_eval(N, R, RR).

get_model_varT_eval(_, [], []) :- !.
get_model_varT_eval(N, [Var|R], [Val|RR]) :-
    z3_get_model_termvar_eval(N, Var, Val),
    get_model_varT_eval(N, R, RR).


/*
    z3_assert_int_string/2 takes an SMT formula with integer variables and asserts it to a context
*/
z3_assert_int_string(N, SMT) :-
    format(string(SMTLIB2), "(assert ~s)", [SMT]),
    z3_assert_int_string_(N, SMTLIB2).


/*
    z3_assert_term_string/2 takes an SMT formula with term variables and asserts it to a context
*/
z3_assert_term_string(N, SMT, Int, List) :-
    format(string(SMTLIB2), "(assert ~s)", [SMT]),
    z3_assert_term_string_(N, SMTLIB2, Int, List).