:- if(predicate_property(use_type_checker, defined)).

:- type clause_head.
:- type clause_body ---> true ; false ;
    ','(clause_body, clause_body) ; ';'(clause_body, clause_body) ; '='(any, any) ;
    as_clause_body(clause_body) ; as_clause_body(';'(clause_body, clause_body)).
:- type clause ---> (clause_head :- clause_body) ; clause(clause_head, clause_body).

:- trust_pred \+(pred).
:- trust_pred load_files(any, list(any)).
:- trust_pred nl.
:- trust_pred current_predicate(any, clause_head).
:- trust_pred predicate_property(clause_head, any).
:- trust_pred clause(clause_head, clause_body, any).
:- trust_pred clause(clause_head, clause_body).

:- pred load_file(any).
:- pred is_user_pred(clause_head).
:- pred is_built_in(clause_head).
:- pred get_clause(clause_head, clause_body, any).
:- pred get_clause_as_list(clause_head, list(clause_body)).
:- pred convert_to_list(clause_body, list(clause_body)).
:- pred hnf(clause_head, clause).
:- pred head_normal_form(clause_head, clause_body).
:- pred convert_list_into_disjunction(list(clause), clause_head, clause_body).


:- pred generate_disjunct(clause_head, clause, clause_body).
:- pred generate_equality_formula(clause_head, clause_head, clause_body).
:- pred gen_equalities(list(any), list(any), clause_body).
:- pred clever_and(clause_body, clause_body, clause_body).

:- else.

:- endif.