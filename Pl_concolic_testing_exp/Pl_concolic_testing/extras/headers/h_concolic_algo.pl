:- if(predicate_property(use_type_checker, defined)).

:- type foo ---> foo(any, list(any)) ; foo(any, any) ; foo(any, any, any).
:- type unk ---> (any = any) ; (any \= any) ; forall(any, unk) ; exists(any, unk).

%:- type alpha ---> p(a,b) ; p(any) ; p(a,any) ; p(s(a),a) ; nat(integer) ; generate(any,any,any).

:- trust_pred foldl(any, list(flag_t), any, any).
:- trust_pred z3_mk_term_type(any, list(any), boolean, boolean).
:- trust_pred prolog_flag(any, list(any)).
:- trust_pred print_help.
:- trust_pred write(any).
:- trust_pred list_to_set(list(any), list(any)).
:- trust_pred reverse(list(any), list(any)).
:- trust_pred on_exception(any, any, any).
:- trust_pred flush_output(any).
:- trust_pred atom(any).
:- trust_pred halt.
:- trust_pred =@=(any, any).
:- trust_pred unifiable(any, any, any).
:- trust_pred maplist(any, list(integer)).
:- trust_pred last(list(any), any).
:- trust_pred prefix(list(any), any).
:- trust_pred length(list(any), any).
:- trust_pred nth0(any, any, any).
:- trust_pred \+(any, any).
:- trust_pred oset_power(list(any), list(list(any))).
:- trust_pred compound(any).
:- trust_pred aggregate(any, any, integer).
:- trust_pred cl(any, list(any)).
:- trust_pred update_constraint(unk, unk).
:- trust_pred z3_push(any).
:- trust_pred z3_mk_term_vars(any, list(any)).
:- trust_pred z3_check(any).
:- trust_pred z3_print_model(any, any).
:- trust_pred false.

:- pred filename(any).
:- pred depthk(integer).

:- pred cli_option(any).
:- pred cli_initial_cg(any).
:- pred cli_initial_sg(any).
:- pred cli_initial_ground(list(integer)).
:- pred cli_initial_depth(integer).
:- pred cli_initial_timeout(integer).
:- pred cli_initial_trace.
:- pred cli_initial_file(any).

:- pred interactive.

:- pred main_cli.

:- pred with_trace.

:- pred print_test_cases.

:- pred convert_entry_to_term(any, any).

:- pred assert_interactive.

:- pred mainT(any, list(integer), integer, any).

:- pred cleaning.

:- pred constants(list(any)).
:- pred functions(list(any)).
:- pred integer.
:- pred list.

:- pred esig_atom_list(list(any)).

:- pred esig_atom(any).

:- pred esig_term_list(list(any)).

:- pred esig_term(any).

:- pred list(list(any)).

:- pred update_constants(any).

:- pred update_functions(any, integer).

:- pred get_consts(list(any), list(any)).

:- pred get_fun(list(any), list(any)).

:- pred get_pred(list(any), list(any)).

:- pred ground_vars(any, list(integer), list(any)).

:- pred gvars(list(any), integer, list(integer), list(any)).

:- pred labeled(list(any)).

:- pred not_labeled(any).

:- pred cl/2.

%:- pred add_clause_labels. 

:- pred update_not_labeled(any).

:- pred acl(list(any), integer, any, any).

:- pred add_dump_parameters(list(any), list(any)).

:- pred add_dump_label(any, any).

:- pred del_dump_label(any, any).

:- pred traces(list(list(any))).
:- pred testcases(list(any)).
:- pred pending_test_case(any).

:- pred update_testcases(any, list(any)).
:- pred update_pending_test_cases(list(any)).
:- pred grounding_vars(any, list(integer)).
:- pred apply_subs(any, any).
:- pred suppress_duplicate(any, list(any), list(any)).

:- pred print_testcases(list(any)).
:- pred print_trace(list(any)).
:- pred print_trace_step(any).
:- pred print_testcases_2(list(any)).

:- pred concolic_testing(any, list(integer), list(any)).

:- pred eval(any, list(any), list(any), list(any), any, list(unk), any).

:- pred change_label(any, list(any), list(any)).
:- pred get_atom_label(any, any).
:- pred get_atom_label(any, any, any).

:- pred get_new_goals(list(foo), list(any)).
:- pred get_new_trace(any, any, any, list(any)).

:- pred alts(any, list(unk), any, list(any), list(any), any, list(any)).
:- pred depth(any, integer).

:- pred mymember(any, list(any)).
:- pred mysubtract(list(any), list(any), list(any)).
:- pred get_constraints(any, any, list(any), list(any), list(unk)).
:- pred get_list_clauses(list(any), list(any)).
:- pred get_pos_consts(any, list(any), list(any), list(unk)).
:- pred get_neg_consts(any, list(any), list(any), list(unk)).
:- pred exists_terms(list(any), any, list(any), any, unk).
:- pred exists_terms_atom(list(any), unk, unk).
:- pred forall_terms(list(any), any, list(any), any, unk).
:- pred forall_terms_atom(list(any), unk, unk).
:- pred update_list_constraints(list(unk), list(unk)).

:- pred solve(any, any, list(unk), any).



:- pred term_variables(any, list(any)).
:- pred append(list(any), list(any), list(any)).
%:- pred append__list_any(list(list(any)), list(list(any)), list(list(any))).
:- pred append__T0(list(any), list(list(any)), list(any)).
:- pred append__unk(list(unk), list(unk), list(unk)).
:- pred subtract(list(any), list(any), list(any)).
:- pred member(any, list(any)).
:- pred member__list_any(list(any), list(list(any))).
:- pred =..(any, list(any)).
:- pred print(any).
:- pred nth1(any, any, any).
:- pred sort(any, any).
:- pred sort__integer(list(integer), list(integer)).
:- pred add_clause_labels.
:- pred main(any, list(integer), integer, integer, boolean, any).

%append__list_any(L1, L2, L12) :- append(L1, L2, L12) :: (list(list(any)), list(list(any)), list(list(any))).
append__T0(L1, L2, L12) :- append(L1, L2, L12) :: (list(any), list(list(any)), list(any)).
append__unk(L1, L2, L12) :- append(L1, L2, L12) :: (list(unk), list(unk), list(unk)).
member__list_any(T, L) :- member(T, L) :: (list(any), list(list(any))).
sort__integer(L0, L1) :- sort(L0, L1) :: (list(integer), list(integer)).

:- else.

append__T0(L1, L2, L12) :- append(L1, L2, L12).
append__unk(L1, L2, L12) :- append(L1, L2, L12).
member__list_any(T, L) :- member(T, L).
sort__integer(L0, L1) :- sort(L0, L1).

:- endif.