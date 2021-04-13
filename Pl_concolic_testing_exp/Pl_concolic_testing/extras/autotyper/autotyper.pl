
builtin_types(boolean).
builtin_types(integer).
builtin_types(float).
%builtin_types(list(_)).
%builtin_types(pair(_,_)).

builtin_preds_(ensure_loaded(_)).
builtin_preds_(call(_)).
builtin_preds_(length(_, integer)).
builtin_preds_(append(_, _, _)).
builtin_preds_(integer(integer)).
builtin_preds_(float(float)).
%builtin_preds_(is_list(list(_))).
builtin_preds_(=(T,T)). %builtin_preds_(=('=/2__T','=/2__T')).
builtin_preds_(\==(T,T)).
builtin_preds_(not(boolean)).

builtin_preds_(>(T,T)).
builtin_preds_(>=(T,T)).
builtin_preds_(=<(T,T)).
builtin_preds_(is(T,T)). % requires an expression parser!
builtin_preds_(!).
builtin_preds_(;(_,_)).
builtin_preds_(var(_)).
builtin_preds_(=:=(_,_)).
builtin_preds_(op(_,_,_)).
builtin_preds_(atomic(_)).
builtin_preds_(nonvar(_)).
builtin_preds_(assertz(_)).
builtin_preds_(retractall(_)).
builtin_preds_(true).
builtin_preds_(number(_)).

%temp
builtin_preds_(@<(_, _)).
builtin_preds_(<(_, _)).
builtin_preds_(=\=(_, _)).
builtin_preds_(member(_, _)).
builtin_preds_(set_prolog_flag(_, _)).
builtin_preds_(fail).
builtin_preds_(format(_,_)).
%builtin_preds_(data(_)). % temp: $harness
builtin_preds_(->(_,_)).
builtin_preds_(statistics(_,_)).
builtin_preds_(functor(_,_,_)).
builtin_preds_(arg(_,_,_)).
builtin_preds_(atom(_)).
builtin_preds_(==(_,_)).
builtin_preds_(compare(_,_,_)).


format_predarg(PName/PArity, PArgI, PArg) :- format(atom(PArg), '~w/~d__arg~d', [PName, PArity, PArgI]).
format_lvar(ClI, PName/PArity, LVName, GVName) :- format(atom(GVName), 'cl~d_~w/~d__~w', [ClI, PName, PArity, LVName]).


:- nb_setval('verbose', true).
print_wlink(T0-T1) :- (nb_current('verbose', true) -> format("weak link established: ~w --- ~w~n", [T0, T1]) ; true).
print_slink(T0-T1) :- (nb_current('verbose', true) -> format("strong link established: ~w === ~w~n", [T0, T1]) ; true).
print_alt(T, A) :- (nb_current('verbose', true) -> format("alternative found for ~w: ~p~n", [T, A]) ; true).


:- dynamic type_clusters/1.
:- dynamic preds_/1.

list_file_preds(FilePath, Preds) :-
	load_files(FilePath), absolute_file_name(FilePath, FilePath_),
	findall(PName/PArity, (current_predicate(_, Pred), source_file(Pred, FilePath_), functor(Pred, PName, PArity)), Preds),
	unload_file(FilePath).
init_pred_args(_PName/PArity, PArgI, []) :- PArgI >= PArity.
init_pred_args(PName/PArity, PArgI, [PArg | PArgs]) :-
	format_predarg(PName/PArity, PArgI, PArg),
	(\+type_clusters([PArg]) -> assertz(type_clusters([PArg])) ; true),
	init_pred_args(PName/PArity, PArgI + 1, PArgs).
init_predlist_args([], []).
init_predlist_args([PName/PArity | Preds], [Pred_ | Preds_]) :-
	init_pred_args(PName/PArity, 0, PArgs),
	Pred_ =.. [PName | PArgs],
	(\+preds_(Pred_) -> assertz(preds_(Pred_)) ; true),
	init_predlist_args(Preds, Preds_).
init_preds_(FilePath) :- list_file_preds(FilePath, Preds_), init_predlist_args(Preds_, _Preds__).


comment_aux1 --> [C], { \+code_type(C, end_of_line) }, !, comment_aux1 | [].
comment_aux2 --> "*/" | [_], !, comment_aux2.
comment --> "%", !, comment_aux1 | "/*", !, comment_aux2.
ws --> [C], { char_code(Ch, C), char_type(Ch, space) }, !, ws | [].
id(ID) --> [C], { code_type(C, csym) }, !, id(Cs), { ID = [C | Cs] } | [], { ID = [] }.
list_aux(T) --> ",", ws, term(T_), !, list_aux(T__), { format(codes(T), ",~s~s", [T_, T__]) } | [], { T = [] }.
list(T) --> "[", ws, "]", { format(codes(T), "[]", []) }
	| "[", ws, term(T_), ws, list_aux(T__), ws, "]", { format(codes(T), "[~s~s]", [T_, T__]) }
	| "[", ws, term(T_), ws, list_aux(T__), ws, "|", ws, term(T___), ws, "]", { format(codes(T), "[~s~s|~s]", [T_, T__, T___]) }.

term_name(TN) --> [C], { code_type(C, csym) }, !, term_name(T_), { TN = [C | T_] } | [], { TN = [] }.
term_name2(TN) --> [C], { code_type(C, prolog_symbol) ; code_type(C, csym) ; code_type(C, space) ; atom_codes('~', [C]) ; atom_codes('|', [C]) }, !, term_name2(T_), { TN = [C | T_] } | [], { TN = [] }.
term_name3(TN) --> [C], { (atom_codes('?', [C]) ; atom_codes('#', [C]) ; atom_codes('.', [C]) ; atom_codes('@', [C]) ; atom_codes('=', [C])), TN = [C] }.
term_args(TA) --> ws, ",", ws, term_binop(T_), !, term_args(Ts), { format(codes(TA), ",~s~s", [T_, Ts]) } | [], { TA = [] }.
term(T) -->
	id(T_), { T_ \= [] }, ws, "+", !, ws, term(T__), { T__ \= [] }, { format(codes(T), "~s + ~s", [T_, T__]) }
	| id(T_), { T_ \= [] }, ws, "-", ws, term(T__), { T__ \= [] }, !, { format(codes(T), "~s - ~s", [T_, T__]) } % [C], {\+atom_codes('>', [C])})
	| (id(T_), { T_ \= [] } | "exp(", id(T2_), ")", { format(codes(T_), "exp(~s)", [T2_]) }), ws, "*", ws, term(T__), { T__ \= [] }, !, { format(codes(T), "~s * ~s", [T_, T__]) }
	| id(T_), { T_ \= [] }, ws, "/", !, ws, term(T__), { T__ \= [] }, { format(codes(T), "~s / ~s", [T_, T__]) }

	% patch ex24
	| id(T_), { T_ \= [] }, ws, ">=", !, ws, term(T__), { T__ \= [] }, { format(codes(T), "~s >= ~s", [T_, T__]) }
	| id(T_), { T_ \= [] }, ws, ">", !, ws, term(T__), { T__ \= [] }, { format(codes(T), "~s > ~s", [T_, T__]) }
	| id(T_), { T_ \= [] }, ws, "=<", !, ws, term(T__), { T__ \= [] }, { format(codes(T), "~s =< ~s", [T_, T__]) }
	| id(T_), { T_ \= [] }, ws, "<", !, ws, term(T__), { T__ \= [] }, { format(codes(T), "~s < ~s", [T_, T__]) }
	
	%| id(T_), ws, "=\\=", !, ws, term_binop(T__), { format(codes(T), "~s =\\= ~s", [T_, T__]) }

	% patch $itak
	%| id(T_), ws, "=", ws, [C0], [C1], { \+atom_codes('\\=', [C0,C1]) }, !, ws, term_binop(T__), { format(codes(T), "~s = ~s", [T_, T__]) } /*, format("oii: ~s~n", [T])*/
	| id(T_), ws, "=", ws, term_binop(T__), !, { format(codes(T), "~s = ~s", [T_, T__]) }
	
	| "!", { atom_codes('!', T) }
	| "\\+", !, term(T_), { format(codes(T), "\\+~s", [T_])}
	| (term_name3(TN) | term_name(TN) | "'", ([C], { atom_codes(',', [C]), TN_ = [C] } | term_name2(TN_)), "'", { format(codes(TN), "'~s'", [TN_]) }), { TN \= [] }, (ws, "(", ws, !, term_binop(T_), term_args(Ts_), ws, ")", { format(codes(T), "~s(~s~s)", [TN, T_, Ts_])/*, atom_codes(AT, T), writeln(AT)*/ } | [], { T = TN }), {/*format("edf: ~s~n", [T])*/}
	| /*{ writeln("term_expr")},*/ term_expr(E), !,{ atom_codes(E, T)/*, writeln(term_expr-E)*/ } % patch $deriv
	| "(", ws, !, term(T_), term_args(Ts_), ws, ")", { string_codes(Ts, Ts_), format(codes(T), "'()'(~s~s)", [T_, Ts]) }
	| list(T)

	%| comment, !, term(T)
	%| zorn(T), { atom_codes(A, T), writeln(A) }
	| [], { T = [] }.


term_expr_Num_aux(N) --> [C], { code_type(C, digit) }, !, term_expr_Num_aux(N_), { N = [C | N_] } | [], { N = [] }.
term_expr_Num(N) --> [C], { code_type(C, digit) }, term_expr_Num_aux(N_), ".", [C], { \+code_type(C, end_of_line) }, term_expr_Num_aux(N__), { atom_codes(AN_, [C | N_]), atom_codes(AN__, N__), format(atom(N), '~w.~w', [AN_, AN__]) }
	| term_expr_Num_aux(N_), ".", [C], { code_type(C, digit) }, term_expr_Num_aux(N__), { atom_codes(AN_, N_), atom_codes(AN__, [C | N__]), format(atom(N), '~w.~w', [AN_, AN__]) }
	| [C], { code_type(C, digit) }, term_expr_Num_aux(N_), { atom_codes(N, [C | N_]) }.
term_expr_Var_aux(V) --> [C], { code_type(C, prolog_identifier_continue) }, !, term_expr_Var_aux(V_), { V = [C | V_] } | [], { V = [] }.
term_expr_Var(V) --> [C], { code_type(C, prolog_var_start) }, term_expr_Var_aux(V_), { V__ = [C | V_], atom_codes(V, V__) }.
term_expr_Atom(V) --> [C], { code_type(C, prolog_atom_start) }, term_expr_Var_aux(V_), { V__ = [C | V_], atom_codes(V, V__) }.
term_expr_Func_aux(V) --> ",", ws, term_expr(V_), ws, !, term_expr_Func_aux(V__), { format(codes(V), ',~w~s', [V_, V__]) } | [], { V = [] }.
term_expr_Func(V) --> "^", { V_ = '^' }, "(", term_expr(V0), ws, term_expr_Func_aux(Vs), ")", { format(atom(V), '~w(~a~s)', [V_, V0, Vs]) }.
term_expr_Rec(E) --> "(", ws, term_expr(E0), ws, ")", { format(atom(E), '(~w)', [E0]) } | term_expr_Var(E) | term_expr_Func(E) | term_expr_Atom(E) | term_expr_Num(E).
term_expr_iPro(E) --> term_expr_Rec(E0), ws, "*" , ws, !,term_expr_Pro(E1), { format(atom(E), '~w * ~w', [E0, E1]) }
	| term_expr_Rec(E0), ws, "/" , ws, !, term_expr_iPro(E1), { format(atom(E), '~w / ~w', [E0, E1]) }
	| term_expr_Rec(E).
term_expr_Pro(E) --> term_expr_Rec(E0), ws, "*" , ws, !, term_expr_Pro(E1), { format(atom(E), '~w * ~w', [E0, E1]) }
	| term_expr_Rec(E0), ws, "/" , ws, !,term_expr_iPro(E1), { format(atom(E), '~w / ~w', [E0, E1]) }
	| term_expr_Rec(E0), ws, "//" , ws, !,term_expr_iPro(E1), { format(atom(E), '~w // ~w', [E0, E1]) }
	| term_expr_Rec(E0), ws, "mod" , ws, !,term_expr_iPro(E1), { format(atom(E), '~w mod ~w', [E0, E1]) }
	| term_expr_Rec(E).
term_expr_Neg(E) --> "-", ws, term_expr_Pro(E0), { format(atom(E), '-~w', [E0]) } | term_expr_Pro(E).
term_expr_iSum(E) --> term_expr_Neg(E0), ws, "+" , ws, !,term_expr_Sum(E1), { format(atom(E), '~w + ~w', [E0, E1]) }
	| term_expr_Neg(E0), ws, "-", ws, !, term_expr_iSum(E1), { format(atom(E), '~w - ~w', [E0, E1]) }
	| term_expr_Neg(E).
term_expr_Sum(E) --> term_expr_Neg(E0), ws, "+" , ws, !, term_expr_Sum(E1), { format(atom(E), '~w + ~w', [E0, E1]) }
	| term_expr_Neg(E0), ws, "-" , ws, !,term_expr_iSum(E1), { format(atom(E), '~w - ~w', [E0, E1]) }
	| term_expr_Neg(E).
term_expr_LOp(E) --> term_expr_Sum(E0), ws, "/\\", ws, !, term_expr_LOp(E1), { format(atom(E), '~w /\\ ~w', [E0, E1]) }
	| term_expr_Sum(E0), ws, ">>", ws, !, term_expr_LOp(E1), { format(atom(E), '~w >> ~w', [E0, E1]) }
	| term_expr_Sum(E).
term_expr(E) --> term_expr_LOp(E), !,{ /*writeln(E)*//*atom_to_term(E, T, _), \+atom(T), \+var(T), writeln(T)*/ }.


zorn_aux(W) --> [C], { \+code_type(C, end_of_line) }, !, zorn_aux(W_), { W = [C | W_] } | [], { W = [] }.
zorn(W) --> [C], { /*\+atom_codes(',', [C]), \+atom_codes('(', [C]), \+atom_codes(')', [C]) */}, zorn_aux(W_), { W = [C | W_] }.

ite(T) --> "(", ws, !, ite(T_), ws, ")", { format(codes(T), "(~s)", [T_]) }
	| terms_binops(T_),ws, "->", !, ws, !, ite(T__), (ws, ";", !, ws, (comment,ws|[]), !, ite(T___), { format(codes(T), "~s -> ~s ; ~s", [T_, T__, T___])/*, format("~s~n", [T])*/ } | { format(codes(T), "~s -> ~s", [T_, T__])/*, format("oij: ~s~n", [T])*/ })
	| terms_binops(T).
binop_sym(TO) --> [C], { code_type(C, prolog_symbol) }, !, binop_sym(TO_), { TO = [C | TO_] } | [], { TO = [] }.
binop_id(TO) --> [C], { code_type(C, alnum) }, !, binop_id(TO_), { TO = [C | TO_] } | [], { TO = [] }.
binop(T) --> %"(", ws, term_binop(T_), ws, "->", ws, term_binop(T__), ws, ")", !, { format(codes(T), "(~s -> ~s)", [T_, T__]) }
	%| "(", ws, term_binop(T_), ws, "->", !, ws, term_binop(T__), ws, ";", ws, (comment,ws,!|[]), term_binop(T___), ws, ")", { format(codes(T), "(~s -> ~s ; ~s)", [T_, T__, T___]) }
	expr(E0), ws, "is", !, ws, expr(E1), { format(codes(T), "'is'(~w, ~w)", [E0, E1]) }
	| expr(E0), ws, "=\\=", !, ws, expr(E1), { format(codes(T), "'=\\\\='(~w, ~w)", [E0, E1]) }
	| expr(E0), ws, "=:=", !, ws, expr(E1), { format(codes(T), "'=:='(~w, ~w)", [E0, E1]) }
	%| term(T_), ws, "=", !, ws, term(T__), { format(codes(T), "'='(~w, ~w)", [T_, T__]) }
	| term(T_), ws, (binop_sym(TO) | binop_id(TO)), !, ws, term(T__), { TO \= [], \+atom_codes('.', TO), atom_codes(AT_, T_), atom_codes(ATO, TO), current_op(_, xfx, ATO), atom_codes(AT__, T__), format(codes(T), '~w(~w, ~w)', [ATO, AT_, AT__]) }
	| ite(T)/*, {format("~s~n", [T])}*/.

term_binop(T) --> binop(T),!/*, {format("meow: ~s~n", [T])}*/ | term(T),!/*, {format("meow: ~s~n", [T])}*/.% | zorn(T), { atom_codes(A, T), writeln(A) }.
terms_binops_aux(T) --> ws, ",", (ws,comment,!|[]), ws, term_binop(T_), (ws,comment,!|[]), ws, !, terms_binops_aux(T__), { format(codes(T), ",~s~s", [T_, T__]) }
	| ws, ";", (ws,comment|[]), ws, term_binop(T_), !, terms_binops_aux(T__), { format(codes(T), ";~s~s", [T_, T__]) }
	| [], { T = [] }.
terms_binops(T) --> "(",ws,!,terms_binops(T_),ws,")", {format(codes(T), "(~s)", [T_])}
	| term_binop(T_), terms_binops_aux(T__), { format(codes(T), "~s~s", [T_, T__]) }.


expr_Num_aux(N) --> [C], { code_type(C, digit) }, expr_Num_aux(N_), { N = [C | N_] } | [], { N = [] }.
expr_Num(N) --> [C], { code_type(C, digit) }, expr_Num_aux(N_), ".", [C], { \+code_type(C, end_of_line) }, expr_Num_aux(N__), { atom_codes(AN_, [C | N_]), atom_codes(AN__, N__), format(atom(N), '~w.~w', [AN_, AN__]) }
	| expr_Num_aux(N_), ".", [C], { code_type(C, digit) }, expr_Num_aux(N__), { atom_codes(AN_, N_), atom_codes(AN__, [C | N__]), format(atom(N), '~w.~w', [AN_, AN__]) }
	| [C], { code_type(C, digit) }, expr_Num_aux(N_), { atom_codes(N, [C | N_]) }.
expr_Var_aux(V) --> [C], { code_type(C, prolog_identifier_continue) }, !, expr_Var_aux(V_), { V = [C | V_] } | [], { V = [] }.
expr_Var(V) --> [C], { code_type(C, prolog_var_start) }, expr_Var_aux(V_), { V__ = [C | V_], atom_codes(V, V__) }.
expr_Rec(E) --> "(", ws, expr(E0), ws, ")", { format(atom(E), '(~w)', [E0]) } | expr_Var(E) | expr_Num(E).
expr_iPro(E) --> expr_Rec(E0), ws, "*" , ws, expr_Pro(E1), { format(atom(E), '~w * ~w', [E0, E1]) }
	| expr_Rec(E0), ws, "/" , ws, expr_iPro(E1), { format(atom(E), '~w / ~w', [E0, E1]) }
	| expr_Rec(E).
expr_Pro(E) --> expr_Rec(E0), ws, "*" , ws, expr_Pro(E1), { format(atom(E), '~w * ~w', [E0, E1]) }
	| expr_Rec(E0), ws, "/" , ws, expr_iPro(E1), { format(atom(E), '~w / ~w', [E0, E1]) }
	| expr_Rec(E0), ws, "//" , ws, expr_iPro(E1), { format(atom(E), '~w // ~w', [E0, E1]) }
	| expr_Rec(E0), ws, "mod" , ws, expr_iPro(E1), { format(atom(E), '~w mod ~w', [E0, E1]) }
	| expr_Rec(E).
expr_Neg(E) --> "-", ws, expr_Pro(E0), { format(atom(E), '-~w', [E0]) } | expr_Pro(E).
expr_iSum(E) --> expr_Neg(E0), ws, "+" , ws, expr_Sum(E1), { format(atom(E), '~w + ~w', [E0, E1]) }
	| expr_Neg(E0), ws, "-", ws, expr_iSum(E1), { format(atom(E), '~w - ~w', [E0, E1]) }
	| expr_Neg(E).
expr_Sum(E) --> expr_Neg(E0), ws, "+" , ws, expr_Sum(E1), { format(atom(E), '~w + ~w', [E0, E1]) }
	| expr_Neg(E0), ws, "-" , ws, expr_iSum(E1), { format(atom(E), '~w - ~w', [E0, E1]) }
	| expr_Neg(E).
expr_LOp(E) --> expr_Sum(E0), ws, "/\\", ws, expr_LOp(E1), { format(atom(E), '~w /\\ ~w', [E0, E1]) }
	| expr_Sum(E0), ws, ">>", ws, expr_LOp(E1), { format(atom(E), '~w >> ~w', [E0, E1]) }
	| expr_Sum(E).
expr(E) --> expr_LOp(E), { /*writeln(E)*/ }.


insert_alt(Type, Alt) :- %(term_variables(Alt, []) -> true ; throw("invalid alt"-Alt)), %print(Alt),nl,
	(types_(Type, Alts) -> Alts_ = [Alt | Alts], retract(types_(Type, Alts)) ; Alts_ = [Alt]),
	assertz(types_(Type, Alts_)), print_alt(Type, Alt).

link_tclust(T0, T1) :-
	((type_clusters(C), member(T0, C)) -> ! ; C = [T0]),
	((type_clusters(C_), member(T1, C_)) -> ! ; C_ = [T1]),
	(C == C_ -> C__ = C ; append(C, C_, C__)),
	(retract(type_clusters(C)) -> true ; true),
	(retract(type_clusters(C_)) -> true ; true),
	assertz(type_clusters(C__)).
build_slink(T0-T1) :- link_tclust(T0, T1), print_slink(T0-T1).
:- dynamic w_link/1.
build_wlink(L) :- ((L \= T-T, \+w_link(L)) -> assertz(w_link(L)), print_wlink(L) ; true).

ugx([], []).
ugx([X|T], [X='_'|T_]) :- ugx(T, T_).

azd3(M_) :- (\+type_clusters([M_]) -> /*writeln(M_), */assertz(type_clusters([M_])) ; true).
azd2_aux(_, _, [], []).
azd2_aux(PName/PArity, ClI, [X = _ | T], [X = M_ | T_]) :- format_lvar(ClI, PName/PArity, X, M_), azd3(M_)/*, format(atom(M_), '_var_(~w)', [M])*/, azd2_aux(PName/PArity, ClI, T, T_).
azd2(Name/Arity, ClI, Arg, S, Arg_) :- !, /*writeln(Arg), writeln(S), */azd2_aux(Name/Arity, ClI, S, M), /*writeln(M),*/ read_term_from_atom(Arg, T, [variable_names(M)]), term_variables(T, Vs), ugx(Vs, Vs_), replace_term_vars(T, Vs_, Arg_)/*, term_to_atom(T, Arg_)*//*, writeln(Arg_)*/.
azd(_, _, _, []).
azd(Name/Arity, ClI, I, ['.' | Args]) :- !, format_predarg(Name/Arity, I, W), functor(Arg_, '.', 0)/*azd2(Name/Arity, ClI, '.', [], Arg_)*/, insert_alt(W, Arg_), azd(Name/Arity, ClI, I+1, Args).
azd(Name/Arity, ClI, I, [Arg | Args]) :- atom_to_term(Arg, T, S), format_predarg(Name/Arity, I, W),
	(\+var(T) -> azd2(Name/Arity, ClI, Arg, S, Arg_), insert_alt(W, Arg_) ; format_lvar(ClI, Name/Arity, Arg, Arg_), build_slink(W-Arg_)),
	azd(Name/Arity, ClI, I+1, Args).

:- nb_setval('clindex', 1).
get_clindex(ClI) :- nb_getval('clindex', ClI), nb_setval('clindex', ClI + 1).


rzs(A, B) :- (integer(B) -> build_wlink(A-integer) ; (float(B) -> build_wlink(A-float) ; true)).

rzu_expr_aux(O, [U, V], ZZ) :- compound_name_arguments(F, O, [U, V]), number(U), number(V), !, ZZ is F.
rzu_expr_aux(_O, [U, V], ZZ) :- U = '_var_'(U_), number(V), !, insert_alt(U_, V), ZZ = U, rzs(U_, V).
rzu_expr_aux(_O, [U, V], ZZ) :- number(U), V = '_var_'(V_), !, insert_alt(V_, U), ZZ = V, rzs(V_, U).
rzu_expr_aux(_O, [U, V], ZZ) :- U = '_var_'(U_), V = '_var_'(V_), !, build_slink(U_-V_), ZZ = U.

rzu_expr(E, E_) :- compound(E), E \= '_var_'(_), compound_name_arguments(E, O, [A, B]), member(O, ['+', '-', '*', '/', '/\\']),
	((compound(A), A \= '_var_'(_)) -> rzu_expr(A, A_) ; A_ = A),
	((compound(B), B \= '_var_'(_)) -> rzu_expr(B, B_) ; B_ = B),
	rzu_expr_aux(O, [A_, B_], E_).
rzu_expr(E, E).


rzu([], [], _).
rzu([PArgE | PArgs], [PArg_ | PArgs_], Z) :- %writeln(erf-PArgE-PArg_-Z),
	(rzu_expr(PArgE, PArg), %writeln(PArg),
		(var(PArg_) ->
			((member(R=G, Z), PArg_ == R) -> %writeln(PArg_-G),
				(PArg = '_var_'(PArgW) -> G = '_var_'(GW), build_slink(PArgW-GW) ; G = '_var_'(GW), insert_alt(GW, PArg)), Z_ = Z ;
				Z_ = [PArg_=PArg | Z], (PArg = '_var_'(PArgW) -> format("link ~w using ~w~n", [PArgW, Z_]) ; insert_alt(PArg_, PArg))
			) ; ((PArg_ == boolean, PArg =.. [_N | _A]) -> term_to_atom(PArg, M), kzx(_,_,M) ;
				Z_ = Z, PArg = '_var_'(PArgW), build_wlink(PArgW-PArg_)
			)
		), rzu(PArgs, PArgs_, Z_)
	).

ioh_aux([], []).
ioh_aux([Arg | Args], [Arg_ | Args_]) :- (Arg = '_var_'(ArgW) -> Arg_ = ArgW ; Arg_ = Arg), ioh_aux(Args, Args_).
ioh(PArg, PArg_) :- PArg =.. [Name | Args], ioh_aux(Args, Args_), PArg_ =.. [Name | Args_].
rzu2([], []).
rzu2([PArg | PArgs], [PArg_ | PArgs_]) :- (PArg = '_var_'(PArgW) -> build_wlink(PArgW-PArg_) ; ioh(PArg, PArg2), insert_alt(PArg_, PArg2)), rzu2(PArgs, PArgs_).


%replace_term_vars(PArg, NGV_, PArg2) :- term_to_atom(PArg, A), read_term_from_atom(A, T, [variable_names(NGV_)]), /*atom_to_term(A,T,NGV_),*/ print(T),nl, PArg2 = T.
replace_term_vars_aux(T, [], T).
replace_term_vars_aux(T, [X=Y|Vs], T_) :- (T == X -> T_ = Y ; replace_term_vars_aux(T, Vs, T_)).
replace_term_vars_aux2([], _Vs, []).
replace_term_vars_aux2([H|T], Vs, [H_|T_]) :- replace_term_vars(H, Vs, H_), replace_term_vars_aux2(T, Vs, T_).
replace_term_vars(T, Vs, T_) :-
	(var(T) -> replace_term_vars_aux(T, Vs, T_) ;
		T =.. [TName | TArgs],
		replace_term_vars_aux(TName, Vs, TName_),
		replace_term_vars_aux2(TArgs, Vs, TArgs_),
		T_ =.. [TName_ | TArgs_], !
	).

tkj(X) :- azd3(X). % $chat_parser: assertz(type_clusters(['cl190_wh/9__M'])), assertz(type_clusters(['cl131_fronted_verb/6__Q'])),


rze_aux(_Name/_Arity, _ClI, _, [], _).
rze_aux(Name/Arity, ClI, PArg, [X = Y | T], PArg2) :- (PArg == Y -> format_lvar(ClI, Name/Arity, X, PArg2_), tkj(PArg2_), PArg2 = '_var_'(PArg2_) ; rze_aux(Name/Arity, ClI, PArg, T, PArg2)).
rzf_aux(_, _, [], _, []).
rzf_aux(Name/Arity, ClI, [NGVH | NGVT], W, [NGVH = NGVH_ | NGVT_]) :- rze_aux(Name/Arity, ClI, NGVH, W, NGVH_), rzf_aux(Name/Arity, ClI, NGVT, W, NGVT_).
rzf(Name/Arity, ClI, PArg, NGV, W, PArg2) :- rzf_aux(Name/Arity, ClI, NGV, W, NGV_), /*((Name == 'wh', member('M'=_, W)) -> writeln(PArg-NGV_); true),*/ replace_term_vars(PArg, NGV_, PArg2).
rze(_Name/_Arity, _ClI, [], _, []).
rze(Name/Arity, ClI, [PArg | PArgs], W, [PArg2 | PArgs2]) :- (term_variables(PArg, NGV) -> rzf(Name/Arity, ClI, PArg, NGV, W, PArg2) ; PArg2 = PArg), rze(Name/Arity, ClI, PArgs, W, PArgs2).
rzy(Name/Arity, ClI, T, W, U) :- T =.. [PName | PArgs], U =.. [PName | PArgs_],/*compound_name_arguments(U, PName, PArgs_),*/ same_length(PArgs, PArgs_), rze(Name/Arity, ClI, PArgs, W, PArgs2), rzu(PArgs2, PArgs_, []).%(functor(T, '(is)', 2) -> rzu(PArgs2, PArgs_, []) ; rzu_expr(PArgs2, PArgs_, [])).
rzy2(Name/Arity, ClI, T, W, U) :- T =.. [PName | PArgs], U =.. [PName | PArgs_], same_length(PArgs, PArgs_), rze(Name/Arity, ClI, PArgs, W, PArgs2), rzu2(PArgs2, PArgs_).

kzx(Name/Arity, ClI, A) :- atom_to_term(A, T, W),
	((builtin_preds_(U), rzy(Name/Arity, ClI, T, W, U) ; preds_(U), rzy2(Name/Arity, ClI, T, W, U)) -> true ;
		functor(T, N_, A_), format(string(E), "argument types within '~w/~d' are unknown!", [N_, A_]), throw(E)).

dynpr --> "dynamic", ws, id(_N), "/", id(_Ar), { }.
cl_goal(Name/Arity, ClI) --> comment, ws,!,cl_goal(Name/Arity, ClI) | (dynpr,! | term_binop(De_), { atom_codes(A, De_), /*write_canonical(A), nl,*/ kzx(Name/Arity, ClI, A) }).
clbody_aux(Name/Arity, ClI) --> ",", ws, cl_goal(Name/Arity, ClI), !, clbody_aux(Name/Arity, ClI) | [].
clbody(Name/Arity, ClI) --> ":-", ws, cl_goal(Name/Arity, ClI), clbody_aux(Name/Arity, ClI), ".".
clbodyA --> ":-", ws, { get_clindex(ClI) }, cl_goal(''/0, ClI), clbody_aux(''/0, ClI) | "(", ws, !, clbodyA, ws, ")".
clbody --> clbodyA, ".". % $main
clhead_aux(Args) --> ws, ",", ws, term_binop(T), !, clhead_aux(Args_), { atom_codes(T_, T), Args = [T_ | Args_] } | [], { Args = [] }.
clhead --> (id(Name_), { atom_codes(Name, Name_) } | "@", { Name = '@' }), ("(", ws, term_binop(T), clhead_aux(Args_), ws, ")", { atom_codes(T_, T), Args = [T_ | Args_] },! | [], { Args = [] }), { length(Args, Arity), get_clindex(ClI) }, ws, (clbody(Name/Arity, ClI) | "."), { /*format("name: ~s~n", [Name]),*/ azd(Name/Arity, ClI, 0, Args)/*, writeln(slx-Name-Args)*/ }.
gram --> ws, ((clhead | clbody | comment), !, gram | []).


%rem_float_wlinks(T) :- type_clusters(Cl), member(T, Cl), w_link()

match_wlink_int_aux([]).
match_wlink_int_aux([I|T]) :- integer(I), match_wlink_int_aux(T).
match_wlink_int(T) :- (types_(T, E) ; E = []),
	(match_wlink_int_aux(E) -> link_tclust(T, integer), format("Weak to strong link ~w === ~w !!!~n", [T, integer]), retract(w_link(T-integer)) ; %rem_float_wlinks(T) ;
		insert_alt(T, '$cast(integer)'), format("cast operator required: ~w -> ~w~n", [integer, T])
	).

match_wlink_float_aux([]).
match_wlink_float_aux([F|T]) :- float(F), match_wlink_float_aux(T).
match_wlink_float(T) :- (types_(T, E) ; E = []),
	(match_wlink_float_aux(E) -> link_tclust(T, float), format("Weak to strong link ~w === ~w !!!~n", [T, float]), retract(w_link(T-float)) ;
		insert_alt(T, '$cast(float)'), format("cast operator required: ~w -> ~w~n", [float, T])
	).

match_wlinks([]).
match_wlinks([T-integer | WLinks]) :- !, match_wlink_int(T), match_wlinks(WLinks).
match_wlinks([T-float | WLinks]) :- !, match_wlink_float(T), match_wlinks(WLinks).
%match_wlinks([T0-T1 | WLinks]) :- !, format("enforced cast operator ~w <- ~w !!!~n", [T0, T1]), format(atom(T_), '$cast(~w)', [T1]), insert_alt(T0, T_), match_wlinks(WLinks).
match_wlinks([T0-T1 | WLinks]) :- !, format("Warning: enforced weak to strong link ~w === ~w !!!~n", [T0, T1]), link_tclust(T0, T1), match_wlinks(WLinks).
match_wlinks([WLink | WLinks]) :- format("Error: unrecognized wlink ~w~n", [WLink]), match_wlinks(WLinks).
match_wlinks :- findall(WLink, w_link(WLink), WLinks), %format("Weak links before: ~w~n", [WLinks]),
	match_wlinks(WLinks),
	findall(WLink_, w_link(WLink_), WLinks_). %format("Weak links after: ~w~n", [WLinks_]).


:- dynamic types_/2.

format_tenum(TEnum, TEnum_) :- sort(TEnum, TEnum_).

tfg_aux(E, [Cluster | Clusters], I, Id) :- /*format("find ~w in ~w~n", [E, Cluster]),*/
	(member(E, Cluster) -> /*writeln(E-Cluster),*/ (\+member(integer, Cluster) -> format(atom(Id), "term~d", [I]) ; Id = integer) ;
		J is I+1, tfg_aux(E, Clusters, J, Id)
	).
tfg(E, Id) :- findall(Cluster, type_clusters(Cluster), Clusters), tfg_aux(E, Clusters, 0, Id).
identify_cluster(TEnumH, ClusterID) :- (tfg(TEnumH, Id) -> ClusterID = Id ; fail).

relink_enum_aux2([], []).
relink_enum_aux2([TEHArg | TEHArgs], [TEHArg_ | TEHArgs_]) :- /*term_to_atom(TEHArg, TEHArgA),*/ relink_enum_aux(TEHArg, TEHArg_), relink_enum_aux2(TEHArgs, TEHArgs_).
relink_enum_aux(TEnumH, TEnumH_) :- %print(TEnumH),nl,
	(identify_cluster(TEnumH, ClusterID) -> TEnumH_ = ClusterID ;
		(((atom(TEnumH) -> (TEnumH \= '.' -> atom_to_term(TEnumH, TEnumHW_, _) ; functor(TEnumHW_, TEnumH, 0)), (term_variables(TEnumHW_, []) -> TEnumHW = TEnumHW_ ; TEnumHW = TEnumH) ; TEnumHW = TEnumH),
			TEnumHW =.. [TEHName | TEHArgs]) -> relink_enum_aux2(TEHArgs, TEHArgs_), TEnumHW_ =.. [TEHName | TEHArgs_],
			TEnumH_ = TEnumHW_/*with_output_to(atom(TEnumH_), write_term(TEnumHW_, [quoted(false)]))*//*term_to_atom(TEnumHW_, TEnumH_)*/ ; TEnumH_ = TEnumH
		)
	).
relink_enum([], []).
relink_enum([TEnumH | TEnumT], [TEnumH_ | TEnumT_]) :- relink_enum_aux(TEnumH, TEnumH_), relink_enum(TEnumT, TEnumT_).

:- dynamic types/2.
mk_types3([], []).
mk_types3([TName | TNames], TEnum) :-
	(types_(TName, TEnum0) -> relink_enum(TEnum0, TEnum_) ; TEnum_ = []),
	mk_types3(TNames, TEnum__),
	append(TEnum_, TEnum__, TEnum).
mk_types2([], _).
mk_types2([Cluster | Clusters], Idx) :-
	((builtin_types(BT), member(BT, Cluster)) -> mk_types2(Clusters, Idx) ;
		mk_types3(Cluster, TEnum),
		format(string(TName), "term~d", [Idx]),
		format_tenum(TEnum, TEnum_),
		assertz(types(TName, TEnum_)),
		mk_types2(Clusters, Idx + 1)
	).
mk_types :- findall(Cluster, type_clusters(Cluster), Clusters), mk_types2(Clusters, 0).


:- dynamic preds/1.
mk_preds_arg(Cluster, I, PArg_) :-
	((builtin_types(BT), member(BT, Cluster)) -> PArg_ = BT ; format(atom(PArg_), "term~d", [I])).
mk_preds_args_aux(_PArg, [], _, _PArg_).
mk_preds_args_aux(PArg, [Cluster | Clusters], I, PArg_) :- (member(PArg, Cluster) -> mk_preds_arg(Cluster, I, PArg_) ; mk_preds_args_aux(PArg, Clusters, I + 1, PArg_)).
mk_preds_args(PArg, PArg_) :- findall(Cluster, type_clusters(Cluster), Clusters), mk_preds_args_aux(PArg, Clusters, 0, PArg_).
mk_preds_aux_aux([], []).
mk_preds_aux_aux([PArg | PArgs], [PArg_ | PArgs_]) :- mk_preds_args(PArg, PArg_), mk_preds_aux_aux(PArgs, PArgs_).
mk_preds_aux([]).
mk_preds_aux([Pred | Preds]) :- Pred =.. [PName | PArgs], mk_preds_aux_aux(PArgs, PArgs_), Pred_ =.. [PName | PArgs_], assertz(preds(Pred_)), mk_preds_aux(Preds).
mk_preds :- findall(Pred, preds_(Pred), Preds), mk_preds_aux(Preds).


proceed(FilePath) :-
	init_preds_(FilePath), !,
	phrase_from_file(gram, FilePath), !,
	match_wlinks,
	mk_types, writeln("ILK!"), mk_preds.
	%findall(TC, type_clusters(TC), TCs), print(TCs), nl.


/** Display **/

display_types_aux([EnumH], Enum_) :- !, format(string(Enum_), "~p", [EnumH]).
display_types_aux([EnumH | EnumT], Enum_) :- display_types_aux(EnumT, Enum__), format(string(Enum_), "~p ; ~w", [EnumH, Enum__]).
display_types([]).
display_types([Type-[] | Types]) :- !, format(":- type ~w.~n", [Type]), display_types(Types).
display_types([Type-Enum | Types]) :- display_types_aux(Enum, Enum_), format(":- type ~w ---> ~s.~n", [Type, Enum_]), display_types(Types).
display_preds([]).
display_preds([Pred | Preds]) :- format(":- pred ~w.~n", [Pred]), display_preds(Preds).
display :-
	findall(Type-Enum, types(Type, Enum), Types), display_types(Types),
	findall(Pred, preds(Pred), Preds), display_preds(Preds).

/*
replace_atomvars_aux([], _, []).
replace_atomvars_aux([A = _ | W], V, [A = D | W_]) :- (member(A = C, V) -> D = C ; D = A), replace_atomvars_aux(W, V, W_).
replace_atomvars(A, V, A_) :- atom_to_term(A, _T, W), replace_atomvars_aux(W, V, W_), read_term_from_atom(A, T_, [variable_names(W_)]), with_output_to(atom(A_), write_term(T_, [quoted(false)])).

prefix_atomvars_aux([], _, []).
prefix_atomvars_aux([V = _ | Vs], P, [V = M | Vs_]) :- format(atom(M), '~w~w', [P, V]), prefix_atomvars_aux(Vs, P, Vs_).
prefix_atomvars(A, P, A_) :- atom_to_term(A, _, W), prefix_atomvars_aux(W, P, V), replace_atomvars(A, V, A_).
*/

/** Main program **/

main :-
	writeln("------------------------------"),
	writeln("Welcome to the automatic typer"),
	writeln("------------------------------"),

	current_prolog_flag(argv, Args),
	(Args == [] -> (
			throw("You must specify a file to load!")
		) ; (
			[_, FilePath | _] = Args
		)
	),
	format("Loading file '~s'~n", [FilePath]), nl,
	%replace_atomvars('u(R, S, f(U, S))', ['S'='W', 'R'='T'], Z), writeln(Z),
	%prefix_atomvars('u(R, S, f(U, S))', apple, Z), writeln(Z),
	proceed(FilePath),nl,
	display,

	halt.