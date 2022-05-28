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
    z3_mk_config/0, z3_del_config/0,
    z3_set_param_value/2,
    z3_mk_context/1, z3_del_context/1,
    z3_mk_solver/1, z3_del_solver/1,
    z3_push/1, z3_pop/1,
    z3_mk_datatypes/3, z3_mk_sort/3,
    z3_mk_vars/3,
    z3_mk_func/6, z3_declare_fun/4, z3_define_fun/5, z3_define_fun_rec/5,
    z3_assert_string/2,
    z3_check_sat/2,
    z3_get_model_to_string/2, z3_eval_model_var/3
]).

:- use_module('../toolbox/gen_tools').
:- use_dual_foreign_library.

normalize_funcargs_in_body(FuncArgs, FuncBody, FuncBody_) :-
    foldl([Arg, I, O]>>(
        [AName, _AType] = Arg,
        nth0(ArgN, FuncArgs, Arg),
        format(string(Pat), "\\b~s\\b", [AName]),
        format(string(NAName), "x!~d", [ArgN]),
        re_replace(Pat/g, NAName, I, O)
    ), FuncArgs, FuncBody, FuncBody_).


z3_declare_fun(ContextID, FuncName, FuncArgs, FuncRet) :-
    z3_mk_func(ContextID, FuncName, FuncArgs, FuncRet, "", false).

z3_define_fun(ContextID, FuncName, FuncArgs, FuncRet, FuncBody) :-
    normalize_funcargs_in_body(FuncArgs, FuncBody, FuncBody_),
    convlist([[_AName, AType], AType]>>true, FuncArgs, FuncArgs_),
    z3_mk_func(ContextID, FuncName, FuncArgs_, FuncRet, FuncBody_, false).

z3_define_fun_rec(ContextID, FuncName, FuncArgs, FuncRet, FuncBody) :-
    normalize_funcargs_in_body(FuncArgs, FuncBody, FuncBody_),
    convlist([[_AName, AType], AType]>>true, FuncArgs, FuncArgs_),
    z3_mk_func(ContextID, FuncName, FuncArgs_, FuncRet, FuncBody_, true).

/*
    z3_assert_string/2 takes an SMT formula with term variables and asserts it to a context
*/
z3_assert_string(N, SMT) :-
    format(string(SMTLIB2), "(assert ~s)", [SMT]),
    z3_assert_string_(N, SMTLIB2).