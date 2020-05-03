%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog Reader
%
%  Only tested in SWI Prolog, http://www.swi-prolog.org/
%
%  Copyright (c) 2020 Sophie Fortz
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(prolog_reader, [load_file/1, is_user_pred/1, is_built_in/1, get_clause/3, get_clause_as_list/2, hnf/2]).

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
load_file(File) :- (
    File=argv(_) -> (print('% ### Please supply filename as argument')::print(any), nl, fail) ; true),
    %prolog_flag(single_var_warnings, Old, off),
	%prolog_flag(redefine_warnings,OldR,off),
    load_files(File, [compilation_mode(assert_all)]),
    print('% loaded file: ')::print(any), print(File)::print(any), nl.
	%set_prolog_flag(single_var_warnings, Old),
	%set_prolog_flag(redefine_warnings, OldR).

:- pred is_user_pred(clause_head).
is_user_pred(P) :- current_predicate(_,P), predicate_property(P, interpreted).
%%predicate_property(P,dynamic). %% in sicstus
:- pred is_built_in(clause_head).
is_built_in(A) :- nonvar(A), \+(is_user_pred(A)).

:- pred get_clause(clause_head, clause_body, any).
get_clause(Head, Body, Ref) :- is_user_pred(Head),
	clause(Head, Body, Ref).

:- pred get_clause_as_list(clause_head, list(clause_body)).
get_clause_as_list(Head, ListBody) :-
	clause(Head, Body, _),
	convert_to_list(Body, ListBody).

:- pred convert_to_list(clause_body, list(clause_body)).
convert_to_list((A, B), [A|BRest]) :- !, convert_to_list(B, BRest).
convert_to_list(true, []).
convert_to_list(A, [A]) :- A \= true.

:- pred hnf(clause_head, clause).
hnf(Call, (Call :- HNF)) :- head_normal_form(Call, HNF).

:- pred head_normal_form(clause_head, clause_body).
head_normal_form(Call, HNF) :-
   findall(clause(Call, Body), clause(Call, Body), HNF_List),
   convert_list_into_disjunction(HNF_List, Call, HNF).

:- pred convert_list_into_disjunction(list(clause), clause_head, clause_body).
convert_list_into_disjunction([], _Call, false).
convert_list_into_disjunction([H], Call, HD) :- generate_disjunct(Call,H,HD).
convert_list_into_disjunction([H, H2|T], Call, as_clause_body(';'(HD,Rest))) :-
   generate_disjunct(Call, H, HD),
   convert_list_into_disjunction([H2|T], Call, Rest).

%:- use_module(library(terms),[term_variables/2]).
:- pred generate_disjunct(clause_head, clause, clause_body).
generate_disjunct(Call, clause(Copy, Body), Disjunct) :-
    generate_equality_formula(Call, Copy, EqFormula),
    clever_and(EqFormula, Body, Res),
    Res = Disjunct.
    % term_variables(Copy,CallVars),
    % get_free_variables(Body,[],CallVars,FreeVars),
    % generate_exists(FreeVars,Res,Disjunct).

:- pred generate_equality_formula(clause_head, clause_head, clause_body).
generate_equality_formula(A, B, Formula) :-
  (A =.. [Func|AArgs])::(clause_head =.. list(any)),
  (B =.. [Func|BArgs])::(clause_head =.. list(any)),
  gen_equalities(AArgs, BArgs, Formula).

:- pred gen_equalities(list(any), list(any), clause_body).
gen_equalities([], [], true).
gen_equalities([A|TA], [B|TB], Res) :-
    gen_equalities(TA, TB, TT), (
        A == B -> Res = TT ;
        clever_and('='(A, B), TT, Res)
    ).

:- pred clever_and(clause_body, clause_body, clause_body).
clever_and(true, X, X).
clever_and(Y, true, Y) :- Y \= true.
clever_and(X, Y, ','(X, Y)) :- X \= true, Y \= true.