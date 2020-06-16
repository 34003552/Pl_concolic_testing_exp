:- if(predicate_property(use_type_checker, defined)).

:- pred context(any).

:- trust_pred z3_mk_config.
:- trust_pred z3_del_config.
:- trust_pred z3_set_param_value(any, any).
:- trust_pred z3_mk_solver(any).
:- trust_pred z3_del_solver(any).
:- trust_pred z3_del_context(any).

:- pred z3_init_context(any).
:- pred z3_clear_context(any).

:- else.

:- endif.