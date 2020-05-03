%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dealing with Z3 contexts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(z3_helper, [context/1, z3_init_context/1, z3_clear_context/1]).

:- ensure_loaded(swiplz3).

:- pred context(any).
:- dynamic context/1. %% current context

:- trust_pred z3_mk_config.
:- trust_pred z3_del_config.
:- trust_pred z3_set_param_value(any, any).
:- trust_pred z3_mk_solver(any).
:- trust_pred z3_del_solver(any).
:- trust_pred z3_del_context(any).

:- pred z3_init_context(any).
z3_init_context(N) :-
    z3_mk_config,
    z3_set_param_value("model", "true"),
    z3_mk_context(N),
    z3_mk_solver(N),
    assertz(context(N)),
    z3_del_config.

:- pred z3_clear_context(any).
z3_clear_context(N) :-
    z3_del_solver(N),
    z3_del_context(N),
    retractall(context(N)).