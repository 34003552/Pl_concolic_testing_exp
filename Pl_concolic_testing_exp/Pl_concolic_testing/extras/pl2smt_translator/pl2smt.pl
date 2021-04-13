
generate_list_aux(_F, range(A, B), []) :- A > B, !.
generate_list_aux(F, range(A, B), L) :-
	call(F, A, R), L = [R | NL], generate_list_aux(F, range(A + 1, B), NL).

generate_list(H, range(A, B), L) :- H =.. [F], !,
	generate_list_aux(F, range(A, B), L).
generate_list(H :- E, range(A, B), L) :- H =.. [£ | [_, _]], !,
	assertz((H :- E)), generate_list_aux(£, range(A, B), L), retract((H :- E)).


merge_stringlist([], "").
merge_stringlist([H], S) :- !, 
	format(string(S), "~w", [H]).
merge_stringlist([H | T], S) :-
	merge_stringlist(T, S_),
	format(string(S), "~w ~w", [H, S_]).


mk_pred_args([], []).
mk_pred_args([IH | IT], O) :-
	(var(IH) -> format(string(OH), "(~w Term)", [IH]) ;
		length(IT, N), format(string(OH), "(_V~d Term)", [N])
	), O = [OH | OT], mk_pred_args(IT, OT).

mk_pred_args2([], [], []).
mk_pred_args2([IH | IT], [OH | OT], Cs) :-
	(var(IH) -> ZH = IH, Cs = CT ;
		length(IT, N), format(string(ZH), "_V~d", [N]), translate_expr(IH, IHS), format(string(CH), "(= ~w ~w)", [ZH, IHS]), Cs = [CH | CT]
	), format(string(OH), "~w", [ZH]), mk_pred_args2(IT, OT, CT).


:- dynamic regpred/3.
/*register_pred(Name, Args, Body) :-
	(regpred(Name, Args, Body) -> fail ; true),
	assertz(regpred(Name, Args, Body)).*/

builtin_pred(//(_X0,_X1), "(ite (>= x!0 0) (div x!0 x!1) (div (- x!0) (- x!1))").
builtin_pred(div(_X0,_X1), "(ite (>= x!1 0) (div x!0 x!1) (- (div (- x!0) x!1)))").
builtin_pred(mod(_X0,_X1), "(ite (>= x!1 0) (mod x!0 x!1) (- (mod (- x!0) x!1)))").
builtin_pred(rem(_X0,_X1), "(ite (>= x!0 0) (rem (abs x!0) (abs x!1)) (- (rem (abs x!0) (abs x!1))))").

use_builtin_pred(Pred) :- functor(Pred, Name, Arity), builtin_pred(Pred, Body), !,
	generate_list(£(I, O) :- format(string(O), "(x!~d Term)", [I]), range(0, Arity - 1), Args),
	(regpred(Name, Args, Body) -> true ;
		merge_stringlist(Args, PArgs),
		format("(define-fun swipl::~w (~w) Term", [Name, PArgs]), nl,
		format("  ~w~n)", [Body]), nl,
		assertz(regpred(Name, Args, Body))
	).


list_translate_expr([], []).
list_translate_expr([IH | IT], [OH | OT]) :-
	translate_expr(IH, OH),
	list_translate_expr(IT, OT).


translate_expr(V, S) :- var(V), !,
	S = V.

translate_expr(X = Y, S) :- !,
	list_translate_expr([X, Y], L),
	format(string(S), "(= ~w ~w)", L).
translate_expr(X \= Y, S) :- !,
	list_translate_expr([X, Y], L),
	format(string(S), "(not (= ~w ~w))", L).

translate_expr(X is Y, S) :- !,
	list_translate_expr([X, Y], L),
	format(string(S), "(= ~w ~w)", L).
translate_expr(X + Y, S) :- !,
	list_translate_expr([X, Y], L),
	format(string(S), "(+ ~w ~w)", L).
translate_expr(X - Y, S) :- !,
	list_translate_expr([X, Y], L),
	format(string(S), "(- ~w ~w)", L).
translate_expr(X * Y, S) :- !,
	list_translate_expr([X, Y], L),
	format(string(S), "(* ~w ~w)", L).
translate_expr(X / Y, S) :- !,
	list_translate_expr([X, Y], L),
	format(string(S), "(/ ~w ~w)", L).
translate_expr(X // Y, S) :- !,
	list_translate_expr([X, Y], L), use_builtin_pred(//(_,_)),
	format(string(S), "(swipl::// ~w ~w)", L).
translate_expr(X div Y, S) :- !,
	list_translate_expr([X, Y], L), use_builtin_pred(div(_,_)),
	format(string(S), "(swipl::div ~w ~w)", L).
translate_expr(X mod Y, S) :- !,
	list_translate_expr([X, Y], L), use_builtin_pred(mod(_,_)),
	format(string(S), "(swipl::mod ~w ~w)", L).
translate_expr(X rem Y, S) :- !,
	list_translate_expr([X, Y], L), use_builtin_pred(rem(_,_)),
	format(string(S), "(swipl::rem ~w ~w)", L).

translate_expr(X > Y, S) :- !,
	list_translate_expr([X, Y], L),
	format(string(S), "(> ~w ~w)", L).
translate_expr(X < Y, S) :- !,
	list_translate_expr([X, Y], L),
	format(string(S), "(< ~w ~w)", L).
translate_expr(X >= Y, S) :- !,
	list_translate_expr([X, Y], L),
	format(string(S), "(>= ~w ~w)", L).
translate_expr(X =< Y, S) :- !,
	list_translate_expr([X, Y], L),
	format(string(S), "(<= ~w ~w)", L).

translate_expr(T, S) :- T =.. [',' | Args], !,
	list_translate_expr(Args, L), merge_stringlist(L, AS),
	format(string(S), "(and ~w)", AS).
translate_expr(T, S) :- T =.. [';' | Args], !,
	list_translate_expr(Args, L), merge_stringlist(L, AS),
	format(string(S), "(or ~w)", AS).
translate_expr(T, S) :- T =.. [F], !,
	format(string(S), "~w", [F]).
translate_expr(T, S) :- T =.. [F | Args], !,
	list_translate_expr(Args, L), merge_stringlist(L, AS),
	format(string(S), "(~w ~w)", [F, AS]).

translate_expr(_, _) :- throw("unrecognized element!").


translate_clause(:- Body, S) :- !,
	translate_expr(Body, Y),
	format(string(S), "(assert ~w)", [Y]).
translate_clause(Head :- Body, S) :- Head =.. [F | Args], !,
	length(Args, NumArgs), translate_expr(Body, LT),
	list_translate_expr(Args, AS), mk_pred_args(AS, AS__), merge_stringlist(AS__, PAS),
	mk_pred_args2(AS, AS_, WX), merge_stringlist(AS_, WW), merge_stringlist(WX, WXZ),
	generate_list(£(I, O) :- format(string(O), "(x!~d Term)", [I]), range(0, NumArgs - 1), AS2__), merge_stringlist(AS2__, PAS2),
	(regpred(F, PAS2, _) -> true ;
		generate_list(£(_I, O) :- O = "Term", range(1, NumArgs), W_), merge_stringlist(W_, W),
		format("(declare-fun ~w (~w) Bool)", [F, W]), nl,
		assertz(regpred(F, PAS2, LT))
	),
	(NumArgs = 0 -> SFCall = F ; format(string(SFCall), "(~w ~w)", [F, WW])),
	format(string(S__), "(= ~w ~w)", [SFCall, LT]), % "(= ~w ~w))"
	(AS_ = [] -> S_ = S__ ;
		(WX = [] -> S3 = S__ ;
			(WX = [_] -> MM = WXZ ;
				format(string(MM), "(and ~w)", [WXZ])
			),
			format(string(S3), "(=> ~w ~w)", [MM, S__])
		),
		format(string(S_), "(forall (~w) ~w)", [PAS, S3])
	),
	format(string(S), "(assert ~w)", [S_]).
translate_clause(Head, S) :- Head =.. [_F | _Args], !,
	translate_clause(Head :- true, S).


ask :-
	read(Clause),
	translate_clause(Clause, SMT),
	writeln(SMT),
	ask.

main :-
	writeln("---------------------------------"),
	writeln("Welcome to the SMTLIB2 translator"),
	writeln("---------------------------------"),
	ask,
	halt.

/* inline div ops */
/*translate_expr(X // Y, S) :- !,
	list_translate_expr([X, Y], L), L = [NX, _NY], append(L, L, NL_), NL = [NX | NL_],
	format(string(S), "(ite (>= ~w 0) (div ~w ~w) (div (- ~w) (- ~w)))", NL).*/
/*translate_expr(X div Y, S) :- !,
	list_translate_expr([X, Y], L), L = [_NX, NY], append(L, L, NL_), NL = [NY | NL_],
	format(string(S), "(ite (>= ~w 0) (div ~w ~w) (- (div (- ~w) ~w)))", NL).*/
/*translate_expr(X mod Y, S) :- !,
	list_translate_expr([X, Y], L), L = [_NX, NY], append(L, L, NL_), NL = [NY | NL_],
	format(string(S), "(ite (>= ~w 0) (mod ~w ~w) (- (mod (- ~w) ~w)))", NL).*/
/*translate_expr(X rem Y, S) :- !,
	list_translate_expr([X, Y], L), L = [NX, _NY], append(L, L, NL_), NL = [NX | NL_],
	format(string(S), "(ite (>= ~w 0) (rem (abs ~w) (abs ~w)) (- (rem (abs ~w) (abs ~w))))", NL).*/


/*translate(H :- B, S) :- H =.. [F | Args], !,
	list_translate_expr(Args, AS), mk_pred_args(AS, AS_), merge_stringlist(AS_, PAS), translate_expr(B, LT),
	(regpred(F, PAS, LT) -> throw("previously defined!") ;
		format(string(S), "(define-fun ~w (~w) Term ~w)", [F, PAS, LT]),
		assertz(regpred(F, PAS, LT))
	).
translate(H, S) :- H =.. [F | Args], !,
	list_translate_expr(Args, AS), mk_pred_args(AS, AS_), merge_stringlist(AS_, PAS),
	(regpred(F, PAS, true) -> throw("previously defined!") ;
		format(string(S), "(define-fun ~w (~w) Term ~w)", [F, PAS, true]),
		assertz(regpred(F, PAS, true))
	).*/

/* generate a list with "Term" */
%length(W_, NumArgs), foreach(between(1, NumArgs, I), nth1(I, W_, "Term")),

/* -- */
	/*
	(WX = [] ->
		(PAS = "" -> S_ = S__ ;
			S3 = S__,
			format(string(S_), "(forall (~w) ~w)", [PAS, S3])
		) ;
		(WX = [_] -> MM = WXZ ; format(string(MM), "(and ~w)", [WXZ])), format(string(S3), "(=> ~w ~w)", [MM, S__]),
		format(string(S_), "(forall (~w) ~w)", [PAS, S3])
	),
	*/