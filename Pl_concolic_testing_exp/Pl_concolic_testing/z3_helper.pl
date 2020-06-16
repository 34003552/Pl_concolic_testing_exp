%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dealing with Z3 contexts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(z3_helper, [context/1, z3_init_context/1, z3_clear_context/1]).

:- include('extras/headers/h_z3_helper.pl').

:- ensure_loaded(swiplz3).

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