
:- module(pl_oldtyper, [
    pl_oldtyper__init/0,

    pl_oldtyper__esig/2,
    get_legacy_term_descriptor/1,

    pl_oldtyper__clean/0
]).

pl_oldtyper__init :-
    assertz(constants(['ø'])), %% 'ø' is just some 'fresh' constant for the negatives cases to succeed.
    assertz(functions([])).

pl_oldtyper__clean :-
    retractall(warning_cut),

    retractall(number),
    retractall(int_constant(_)),
    retractall(float_constant(_)),

    retractall(constants(_)),
    retractall(functions(_)),
    retractall(list),
    retractall(integer),
    retractall(float).


get_terms(Terms, Predicates) :-
    constants(C), get_consts(C, Consts_),
    (number ->
        (int_constant(_) -> assertz(integer) ; true),
        (float_constant(_) -> assertz(float) ; true),
        Consts = Consts_
    ;
        %findall((I_, 0), (int_constant(I), format(atom(I_), '~di', [I])), IntConsts),
        %findall((R_, 0), (float_constant(R), format(atom(R_), '~ff', [R])), FloatConsts),
        (setof((I_, 0), I^(int_constant(I), format(atom(I_), '~di', [I])), IntConsts),! ; IntConsts = []),
        (setof((R_, 0), R^(float_constant(R), format(atom(R_), '~ff', [R])), FloatConsts),! ; FloatConsts = []),
        append([Consts_, IntConsts, FloatConsts], Consts)
    ),
    functions(F), get_fun(F, Functions),
    concolic_tester:labeled(Preds), get_pred(Preds, Predicates),

    append(Consts, Functions, Terms).

    /*append(Consts, Functions, Terms_),
    append(Terms_, Predicates, Terms)*/
    /*append([Consts, Functions, Predicates], Terms_),
    list_to_set(Terms_, Terms).*/ % avoid double recognition: both function & predicate!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extracting the (universal) type from the program.
% So far only atoms and functors are allowed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic number/0.
:- dynamic int_constant/1.
:- dynamic float_constant/1.

:- dynamic constants/1.
:- dynamic functions/1.
:- dynamic integer/0.
:- dynamic float/0.
:- dynamic list/0.

:- dynamic warning_cut/0.

esig_atom_list(V) :- var(V), !.
esig_atom_list([]).
esig_atom_list([A | R]) :- esig_atom(A), esig_atom_list(R).

esig_atom(A) :- A =.. [F | Args], esig_term_list(Args), length(Args, N),
    ((F/N = '!'/0, \+warning_cut) ->
        print_message(warning, 'Cut operator detected but ignored - selective determinism is not yet supported!'),
        assertz(warning_cut)
    ; true),
    (((F/N = 'is'/2 ; F/N = '=:='/2 ; F/N = '>'/2 ; F/N = '<'/2 ; F/N = '>='/2 ; F/N = '=<'/2), \+number) ->
        assertz(number)
    ; true).

esig_term_list([]).
esig_term_list([T | R]) :- esig_term(T), esig_term_list(R).

esig_term(T) :- var(T), !.
esig_term(T) :- atom(T), !, update_constants(T).
esig_term(T) :- integer(T), !, assertz(int_constant(T)).
esig_term(T) :- float(T), !, assertz(float_constant(T)).
%esig_term([]) :- !, assertz(list).
%esig_term([T | R]) :- esig_term(T), esig_term_list(R).
esig_term(T) :- esig_term_list(T), !, assertz(list).
%esig_term(T) :- (functor(T, [], 0) ; functor(T, '[|]', 2)), !, assertz(list), esig_term_list(T). 
esig_term(T) :- compound(T), !, functor(T, F, N),
    update_functions(F, N), T =.. [F | Args], esig_term_list(Args).
esig_term(T) :- string(T), !. % ???
esig_term(T) :- nl, format("ERROR. Type not supported: ~p", [T]), nl, halt.

%J-E: useless?
list([]).
list([_]).

update_constants(C) :- constants(CL), member(C, CL), !.
update_constants(C) :- constants(CL), retractall(constants(_)), !, assertz(constants([C | CL])).

update_functions(F, N) :- functions(FL), member(fun(F, N), FL), !.
update_functions(F, N) :- functions(FL), retractall(functions(_)), !, assertz(functions([fun(F, N) | FL])).

pl_oldtyper__esig(Args, Body) :-
    copy_term(Args, CopyArgs),
    esig_term_list(CopyArgs),
    esig_atom_list(Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Getting list of constants, functions and predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_consts([], []).
get_consts([C|Consts], List) :- get_consts(Consts, List_), List = [(C, 0) | List_].

get_fun([], []).
get_fun([fun(Name, Arity) | Funs], List):- get_fun(Funs, List_), List = [(Name, Arity) | List_].

get_pred([], []).
get_pred([pred(Name, Arity) | Preds],List):- get_pred(Preds, List_), List = [(Name, Arity) | List_].


get_legacy_term_descriptor(term_desc(Terms, Predicates, Int, List)) :-
    get_terms(Terms, Predicates),
    (integer -> Int = true; Int = false),
    (float -> throw("legacy typing does not allow float values yet"); true),
    (list -> List = true; List = false).