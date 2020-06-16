:- if(predicate_property(use_type_checker, defined)).

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
:- pred retract_var(any, list(any)).
:- pred z3_pop(any, list(any)).
:- pred var(any, any).
:- pred get_varnames(list(any), list(any)).
:- pred z3_intconstr2smtlib(any, any, any, list(any), any).
:- pred z3_assert_int_string(any, any).
:- pred z3_assert_term_string(any, any, boolean, boolean).
:- pred assert_vars(any, list(any)).
:- pred assert_terms(any, list(any)).
:- pred get_context_vars(any, list(any)).
:- pred get_model_var_eval(any, list(any), list(any)).
:- pred get_model_varT_eval(any, list(any), list(any)).
:- pred var_decl(list(any), list(any)).
:- pred constr2smt(list(any), any).
:- pred con2smt_list(list(any), any).
:- pred con2smt(term1, any).
:- pred transf(any, any, any).
:- pred z3_termconstr2smtlib(any, any, constr_t, list(any), any).
:- pred constrP2smt(list(any), list(term2), any).
:- pred conP2smt_list(list(any), list(term2), any).
:- pred conP2smt(term1, list(term2), any).
:- pred get_args_list(any, integer, list(term2), any).
:- pred list_of_args(any, integer, list(term2), any).
:- pred transfT(any, any, any).

:- else.

:- endif.