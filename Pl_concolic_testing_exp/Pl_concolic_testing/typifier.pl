
:- module(typifier, [
	has_type_checker/1,

	read_type_annotations/3,
	build_z3types/3,

	z3_termconstrW2smtlib/6,

	unregister_funcs/0
]).

/* ---- Type Annotations ---- */

/*ncodes_to_strings([], Cs, Ss) :- string_codes(S, Cs), Ss = [S].
ncodes_to_strings([CH | CT], Cs, Ss) :- (\+is_list(CH) -> (
		Cs = [CH | CT_], ncodes_to_strings(CT, CT_, Ss)
	) ; (
		print(Cs), nl, string_codes(S, Cs), Ss = [S | S_], ncodes_to_strings(CH, _Cs, S_)
	)).

ncodes_to_strings(NCodes, Ss) :-
	ncodes_to_strings(NCodes, _Cs, Ss).*/

has_type_checker(_File) :-
    %source_file_property(TCFile, included_in(File, _)),
    source_file_property(_TCFile, module('type_check')).

all --> [C], { char_code(Ch, C), print(Ch) }, !, all | [], { nl }.
ws --> [C], { char_code(Ch, C), char_type(Ch, space) }, !, ws | [].
identifier(I) --> [C], { char_code(Ch, C), char_type(Ch, csym), I = [C | T] }, !, identifier(T)
	| [], { I = [] }.
tplarg_aux(TA) --> [C], { char_code(Ch, C), char_type(Ch, alnum) }, !, tplarg_aux(TAT), { TA = [C | TAT] }
	| [], { TA = [] }.
tplarg(TA) --> [C], { char_code(Ch, C), char_type(Ch, upper) }, !, tplarg_aux(TAT), { TA_ = [C | TAT], atom_codes(TA, TA_) }.
%term(I) --> [C], { char_code(Ch, C), (char_type(Ch, csym) ; Ch = '(' ; Ch = ')'), I = [C | T] }, !, term(T) | [], { I = [] }.
/*term_aux(I) --> ",", !, term(J), term_aux(K), { I = [J | K]} | [], { I = [] }.
term(I) -->
	"(", !, ws, term(J), term_aux(K), ws, ")", { I = [[J | K] | T] }, term(T)
	| [C], { code_type(C, csym), I = [C | T] }, !, term(T)
	| [], { I = [] }.*/
identifierS(S) --> identifier(I), { string_codes(S, I) }.
identifierS2_aux(S) --> ",", !, ws, (tplarg(S0H_), { nb_getval(S0H_, S0H) } | identifierS2(S0H)), ws, identifierS2_aux(S0T), {S = [S0H | S0T]}
	| [], { S = [] }.
identifierS2(S) --> identifierS(S0), !, (ws, "(", (tplarg(S0H_), { nb_getval(S0H_, S0H) } | identifierS2(S0H)), ws, identifierS2_aux(S0T), ")", {S = (S0-[S0H | S0T])} | [], {S = S0}).
identifierS3_aux(S) --> ",", !, ws, tplarg(S0H), { nb_delete(S0H), nb_linkval(S0H, _) }, ws, identifierS3_aux(S0T), { S = [S0H | S0T] }
	| [], { S = [] }.
identifierS3(S) --> identifierS(S0), (ws, "(", tplarg(S0H), { nb_delete(S0H), nb_linkval(S0H, _) }, ws, identifierS3_aux(S0T), ")", {S = (S0-[S0H | S0T])} | [], {S = S0-[]}).
%termS(S) --> term(I), { ncodes_to_strings(I, S) }.
rzh(H) --> identifierS2(H__), !, { H__ = H_-L -> H = [H_ | L] ; H = H__ }.%identifierS(H_), "(", !, identifierS2(HA0), rpred_aux(HAs), ")", { H = [H_ | [HA0 | HAs]] } | identifierS(H).
rtype_aux(TElem) -->
	";", !, ws, /*termS(H),*/rzh(H), ws, rtype_aux(T), { TElem = [H | T] }
	| [], { TElem = [] }.
tyu([], []).
tyu([IH|IT], [OH|OT]) :- nb_getval(IH, OH), /*nb_delete(IH),*/ tyu(IT, OT).
rtype(TName, [TEH | TET]) -->
	":-", ws, "type", !, ws, identifierS3(TName_-Tpls), ws, "--->", ws, /*termS(TEH),*/rzh(TEH), ws, rtype_aux(TET), ws, ".",
	{
		(Tpls = [] -> TName = TName_ ; tyu(Tpls, Tpls_), /*numbervars(Tpls_, 0, _End, [functor_name('T')]),*/ TName = TName_-Tpls_)
		%format("sdf: ~w~n", [TName])
	}.
rpred_aux(PArgs) --> ",", !, ws, identifierS2(PArgH), ws, rpred_aux(PArgT), { PArgs = [PArgH | PArgT] }
	| [], { PArgs = [] }.
rpred(PName, PArgs) --> ":-", ws, "pred", !, ws, identifierS(PName), ws,
	(
		"(", !, ws, identifierS2(PArg0), ws, rpred_aux(PArgT), ws, ")", ws, { PArgs = [PArg0 | PArgT] }
		| [], { PArgs = [] }
	), ".".
trash --> "\n" | [_C], !, trash.
gram(T, P) --> rtype(Th, Ah), !, gram(Tl, P), { T = [[Th, Ah] | Tl] }
	| rpred(Ph, Bh), !, gram(T, Pl), { P = [[Ph, Bh] | Pl] }
	| trash, !, gram(T, P)
	| [], { T = [], P = [] }.

/*read_zed(Str, []) :- at_end_of_stream(Str).
read_zed(Str, [H | T]) :-
	\+at_end_of_stream(Str),
	read_line_to_string(Str, rtype(H)),
	read_zed(Str, T).*/

read_type_annotations(File, Types, Preds) :-
	%(has_type_checker(File) -> printf("Type checker: Yes\n") ; printf("Type checker: No\n")),
	%print(File), nl,
	%open(File, read, Str),
	%read_zed(Str, D),
	%close(Str),
	phrase_from_file(gram(Types, Preds), File),
	%print(Types), nl,
	%print(Preds), nl,
	register_funcs(Types),
	register_funcs2(Preds).
	%prolog_reader:extract_types().
	%prolog_reader:extract_pred_types().

/* ---- registered functions ---- */
:- dynamic regfun/3.

register_funcs_aux(_RetType, []).
register_funcs_aux(RetType, [Func | Funcs]) :-
	(is_list(Func) -> [FName | FArgs] = Func ; FName = Func, FArgs = []),
	%writeln(regfun(FName, FArgs, RetType)),
	assertz(regfun(FName, FArgs, RetType)),
	register_funcs_aux(RetType, Funcs).

register_funcs([]).
register_funcs([[RetType, Elems] | TypesT]) :-
	register_funcs_aux(RetType, Elems),
	register_funcs(TypesT).


register_funcs2([]).
register_funcs2([[PName, PArgs] | Preds]) :-
	%writeln(regfun(PName, PArgs, "Term")),
	assertz(regfun(PName, PArgs, "Term")),
	register_funcs2(Preds).

unregister_funcs :- retractall(regfun(_, _, _)).

/* ---- Type Building ---- */

build_z3_term_accs(_, _, [], []).
build_z3_term_accs(PName, N, [AccTH | AccTT], Accs) :-
	format(string(AccName), "~s_arg_~d", [PName, N]),
	Accs = [[AccName, AccTH] | Accs_],
	build_z3_term_accs(PName, N + 1, AccTT, Accs_).

build_z3_term_ctors([], []).
build_z3_term_ctors([TCPredsH | TCPredsT], Ctors) :-
	[PName, PArgTs] = TCPredsH,
	build_z3_term_accs(PName, 0, PArgTs, PAccs),
	Ctor = [PName | PAccs],
	Ctors = [Ctor | Ctors_],
	build_z3_term_ctors(TCPredsT, Ctors_).

build_z3t_term(TCPreds, TermType) :-
	build_z3_term_ctors(TCPreds, Ctors),
	%Ctors = [["term_from_t0", ["term_as_t0", "t0"]] | Ctors_],
	TermType = ["Term" | Ctors].

build_z3_other_accs(_, _, [], []).
build_z3_other_accs(PName, N, [AccTH | AccTT], Accs) :-
	format(string(AccName), "~s_arg_~d", [PName, N]),
	Accs = [[AccName, AccTH] | Accs_],
	build_z3_term_accs(PName, N + 1, AccTT, Accs_).

build_z3t_other_ctors([], []).
build_z3t_other_ctors([TCCTorsH | TCCTorsT], Ctors) :-
	(\+is_list(TCCTorsH) ->
		(
			PName = TCCTorsH,
			Ctor = PName
		) ; (
			[PName | PArgTs] = TCCTorsH,
			build_z3_other_accs(PName, 0, PArgTs, PAccs),
			Ctor = [PName | PAccs]
		)
	),
	Ctors = [Ctor | Ctors_],
	build_z3t_other_ctors(TCCTorsT, Ctors_).

build_z3t_other([], []).
build_z3t_other([TCTypesH | TCTypeT], OtherTypes) :-
	[TName, TCtors] = TCTypesH,
	build_z3t_other_ctors(TCtors, Ctors),
	OtherTypes = [[TName | Ctors] | OtherTypes_],
	build_z3t_other(TCTypeT, OtherTypes_).

include_tpl(_TName, [], []).
include_tpl(TName, [TplTypesH | TplTypesT], Dst) :-
	%text_to_string(TName, TName__), term_string(TName_, TName__), %atom_to_term(TName, TName_, _),
	/*subsumes_term(TName, TplTypesH) ; unifiable(TName, TplTypesH, _)*/
	((TName = N-L1, TplTypesH = N-L2, length(L1, LE), length(L2, LE)) -> Dst = [TplTypesH | Dst_] ; Dst = Dst_),
	include_tpl(TName, TplTypesT, Dst_).

fmtvars([], [], []).
fmtvars([IH1 | IT1], [IH2 | IT2], [IH1 = IH2 | OT]) :- fmtvars(IT1, IT2, OT).

substitute_tpl_aux_aux2(CArg, [], CArg).
substitute_tpl_aux_aux2(CArg_, [NT = V | SVarsT], CArg) :-
	(NT == CArg_/*atom_string(NT, CArg_)*/ -> CArg = V ; /*writeln("zer{"), print(CArg_), nl, print(NT), nl, writeln("}"),*/ substitute_tpl_aux_aux2(CArg_, SVarsT, CArg)).

substitute_tpl_aux_aux(CArg_, SVars, CArg) :-
	%format(">>~w~n", [CArg_]),
	%(unifiable(CArg_, N-L, R) -> format(">>>~w~n", [R]) ; true),
	(subsumes_term(N-L, CArg_) -> CArg_ = N-L, substitute_tpl_aux(L, SVars, L_), CArg = N-L_ ; substitute_tpl_aux_aux2(CArg_, SVars, CArg)).
	%writeln(CArg_),
	%writeln(SVars), halt.

substitute_tpl_aux([], _SVars, []).
substitute_tpl_aux([CArgsH_ | CArgsT_], SVars, [CArgsH | CArgsT]) :-
	substitute_tpl_aux_aux(CArgsH_, SVars, CArgsH),
	%write("SVars: "), writeln(SVars),
	%writeln("CArg {"), writeln(CArgsH_), writeln(CArgsH), writeln("}"),
	substitute_tpl_aux(CArgsT_, SVars, CArgsT).

substitute_tpl([], _SVars, []).
substitute_tpl([TCTorsH_ | TCTorsT_], SVars, [TCTorsH | TCTorsT]) :-
	TCTorsH_ = [CName | CArgs_],
	TCTorsH = [CName | CArgs],
	%writeln(CName),
	%writeln("erf"),
	substitute_tpl_aux(CArgs_, SVars, CArgs),
	%writeln("CArgs {"), writeln(CArgs_), writeln(CArgs), writeln("}"),
	substitute_tpl(TCTorsT_, SVars, TCTorsT).
	

make_tpltypes_aux(TName, TCType, Type) :-
	%writeln("hello"),
	copy_term(TCType, [TName_, TCTors_]), %format("zod: ~w~n", [TCType]),
	TName_ = N-L1,
	TName = N-L2,
	%format("zod0: ~w~n", [L1]), format("zod1: ~w~n", [L2]),
	fmtvars(L1, L2, SVars), %writeln(SVars),
	substitute_tpl(TCTors_, SVars, TCTors),
	%unifiable(TName, TName_, Unifier),
	%copy_term(TCtors_, TCtors__),
	%writeln(Unifier), writeln(TCtors__), halt,
	Type = [TName, TCTors],
	(term_variables(Type, []) -> true ; format("template instantiation failure: ~w~n", [Type]), halt).

make_tpltypes([], _TCType, []).
make_tpltypes([TNamesIH | TNamesIT], TCType, [TypesIH | TypesIT]) :-
	make_tpltypes_aux(TNamesIH, TCType, TypesIH),
	make_tpltypes(TNamesIT, TCType, TypesIT).

deploy_tpltypes([], _TplTypes, []).
deploy_tpltypes([TCTypesH | TCTypesT], TplTypes, NTCTypes) :-
	[TName, _TCtors] = TCTypesH,
	deploy_tpltypes(TCTypesT, TplTypes, NTCTypes_),
	(TName = _N-_L ->
		%format("~w => ~w~n", [TName, TplTypes]),
		%print(TName), nl, print(TplTypes), nl,
		include_tpl(TName, TplTypes, TNames),
		%writeln(TNames),
		make_tpltypes(TNames, TCTypesH, W),
		append(W, NTCTypes_, NTCTypes)
		; NTCTypes = [TCTypesH | NTCTypes_]
	).

identify_tpltypes_aux([], []).
identify_tpltypes_aux([TArgsH | TArgsT], TplTypes) :-
	(TArgsH = _N-L -> identify_tpltypes_aux(L, TplTypes_), append(TplTypes_, [TArgsH], TplTypes__) ; TplTypes__ = []),
	identify_tpltypes_aux(TArgsT, TplTypes___), append(TplTypes__, TplTypes___, TplTypes).

identify_tpltypes([], []).
identify_tpltypes([TCPredsH | TCPredsT], TplTypes) :-
	[_PName, TArgs] = TCPredsH,
	identify_tpltypes_aux(TArgs, TplTypes_),
	identify_tpltypes(TCPredsT, TplTypes__),
	append(TplTypes_, TplTypes__, TplTypes).

build_z3types(TCTypes, TCPreds, Z3Types) :-
	%writeln(TCTypes),
	build_z3t_term(TCPreds, TermType),
	%print(TermType), nl,
	identify_tpltypes(TCPreds, TplTypes),
	%writeln(TplTypes), halt,
	deploy_tpltypes(TCTypes, TplTypes, NTCTypes),
	%writeln(NTCTypes),
	build_z3t_other(NTCTypes, OtherTypes),
	%writeln(OtherTypes),
	%print(OtherTypes), nl,
	%throw("halt"),
	append(OtherTypes, [TermType], Z3Types). %Z3Types = [TermType | OtherTypes].
	%writeln("Type descriptor"), print(Z3Types), nl.


/* ---- ? ---- */

%display_recvars :- forall(recvars(VarName, VarType), (format("~s -> ", [VarName]), writeln(VarType))).
%display_zo :- forall((atom_string(VN, VarName), b_getval(VN, VarType)), (format("~s -> ", [VarName]), writeln(VarType))).

%:- dynamic recvars/2.

get_vartype(VarName, VarType) :-
	atom_string(VN, VarName), nb_getval(VN, VarType).
	%format("~s -> ", [VarName]), writeln(VarType).

get_vartypes([], []).
get_vartypes([VarName | VarNames], [VarType | VarTypes]) :-
	%atom_string(VN, VarName), nb_getval(VN, VarType),
	%format("~s -> ", [VarName]), writeln(VarType),
	get_vartype(VarName, VarType),
	get_vartypes(VarNames, VarTypes).

clean_vartypes([]).
clean_vartypes([VarName | VarNames]) :-
	atom_string(VN, VarName), nb_delete(VN), clean_vartypes(VarNames).

/*type_assert_gname([], [], []).
type_assert_gname([SMTC | SMTCs], VName, SMT) :-
	(atom_codes(']', [SMTC]) ->
		VName = [], SMT = SMTCs ;
		type_assert_gname(SMTCs, VName_, SMT), VName = [SMTC | VName_]
	).

type_assert([], []).
type_assert([SMTC | SMTCs], SMT) :-
	(atom_codes('[', [SMTC]) ->
		type_assert_gname(SMTCs, VName_, SMTCs_),
		string_codes(VName, VName_), get_vartype(VName, VType_), swiplz3:dt_translate(VType_, VType),
		format(codes(SMT_), "~s ~s", [VName, VType]),
		type_assert(SMTCs_, SMTDs_), append(SMT_, SMTDs_, SMT) ;
		SMT = [SMTC | SMTDs], type_assert(SMTCs, SMTDs)
	).
	%char_code(S, SMTC), writeln(S), type_assert(SMTCs, _).*/

name(Name) --> [C], { char_type(C, alnum) }, !, name(Cs), { Name = [C | Cs] }
	| [], { Name = [] }.
index(I) --> [C], { char_type(C, digit) }, !, index(Cs), { I = [C | Cs] }
	| [], { I = [] }.
deploy_var(Out) --> "[var(", !, name(VName_), ")]", {
		%format("erf: ~w~n", VName_),
		string_codes(VName, VName_), get_vartype(VName, VType_),
		swiplz3:dt_translate(VType_, VType), 
		format(codes(Out), "(~s ~s)", [VName, VType])
	}.
deploy_ctor(Out) --> "[ctor(", !, name(CN), "@", index(I), ")]", {
		atom_codes(CName, CN), number_codes(CIdx, I),
		format(atom(CCName), '~w.count', [CName]),
    	(nb_current(CCName, Count) -> true ; Count = 0),
		(Count = 1 -> Out = CName ;
			term_to_atom(CName-CIdx, CNA), nb_getval(CNA, CType_),
			swiplz3:dt_translate(CType_, CType), %format("yui: ~w ~w~n", [CType_, CType]),
			format(codes(Out), "(as ~s ~s)", [CName, CType])
		)
	}.
any_except(Out) --> deploy_var(O1), !, any_except(O2), { append(O1, O2, Out) }
	| deploy_ctor(O1), !, any_except(O2), { append(O1, O2, Out) }
	| [C], !, any_except(Cs), { Out = [C | Cs] }
	| [], { Out = [] }.

type_assert(In, Out) :- string_codes(In, ICs), phrase(any_except(OCs), ICs), string_codes(Out, OCs).


z3_termconstrW2smtlib(Context, OldC, C, NewVarsStr, NewVarsTypes, SMT) :-
    term_variables(OldC, OldCVars),
    term_variables(C, CVars),

    subtract(CVars, OldCVars, NewVars),
    copy_term((C, NewVars), (CC, CNewVars)),
    numbervars(CNewVars),

    z3_helper:get_varnames(CNewVars, NewVarsStr), (
        NewVarsStr = [] -> true ;
        z3_helper:assert_vars(Context, NewVarsStr)
    ),
    constrWP2smt(CC, _, _VTy, SMT_), !,
    %recvars("A", A), writeln(A), halt,
    %format("~s~n", NewVarsStr), %writeln("Point BR"),
    %writeln("BEGIN"),
    %display_recvars,%get_vartypes(NewVarsStr, NewVarsTypes),!,
    %writeln("meow!"),
    %print(NewVarsStr),nl,
    get_vartypes(NewVarsStr, NewVarsTypes),
    %print(NewVarsStr),nl,print(NewVarsTypes),nl,
    %string_codes(SMTe, SMT_),writeln(SMTe),

    type_assert(SMT_, SMT__),

    clean_vartypes(NewVarsStr),

    %writeln(NewVarsTypes),
    %writeln("END"),
    %retractall(recvars(_, _)),
    string_codes(SMT, SMT__),
    %string_codes(SMT2, SMT_),writeln(SMT2),writeln(SMT),
    %writeln(SMT),
    !.

%neofun(Name, ArgTypes, _RetType) :- length(ArgTypes, Arity), format("new function: ~s/~d", Name, Arity), nl.
rsvfun("=", [T, T], _).
rsvfun("\\=", [T, T], _).
rsvfun("exists", [_, _], _).
rsvfun("forall", [_, _], _).

/*rsvfun("nil", [], "list"-[T]).
rsvfun("insert", [T0, "list"-T1], "list"-[T0 | T1]).
rsvfun("mk-pair", [T0, T1], "pair"-[T0, T1]).*/

register_ctor(Name, Count, Value) :-
    format(atom(CName), '~w.count', [Name]),
    (nb_current(CName, Count) -> true ; Count = 0),
    term_to_atom(Name-Count, A), nb_setval(A, Value),
    NCount is Count + 1, nb_setval(CName, NCount).

constrWP2smt([C], LT, [VTy], SMT) :-
    !, conWP2smt(C, LT, VTy, SMT).
constrWP2smt(List, LT, VTy, SMT) :-
    conWP2smt_list(List, LT, VTy, SMT_),
    format(codes(SMT), "(and ~s)", [SMT_]).

conWP2smt_list([C], LT, [VTy], SMT) :-
    !, conWP2smt(C, LT, VTy, SMT).
conWP2smt_list([C|R], LT, VTy, SMT) :-
    conWP2smt(C, LT1, VTy1, SMT1),
    conWP2smt_list(R, LT2, VTy2, SMT2),
    append(LT1, LT2, LT), append([VTy1], VTy2, VTy),
    format(codes(SMT), "~s ~s", [SMT1, SMT2]).

/* expression rooted by a binary operator */
conWP2smt(T, LT, VTy, SMT) :- functor(T, F, 2), transfTW(F, Fmt), !,
    arg(1, T, Arg1), conWP2smt(Arg1, LT1, VTA0, SMT1),
    arg(2, T, Arg2), conWP2smt(Arg2, LT2, VTA1, SMT2),
    append(LT1, LT2, LT),
    format(codes(SMT), Fmt, [SMT1, SMT2]),
    write_to_chars(F, OPName), string_chars(S, OPName),
    rsvfun(S, [VTA0, VTA1], VTy).
    %(regfun(S, [VTA0, VTA1], VTy) ; rsvfun(S, [VTA0, VTA1], VTy) ; writeln(regfun(S, [VTA0, VTA1], VTy)), format("Point binop: ~w~n", [T])),
    %format("Binop ~w: ~w @ ~w -> ~w ~n", [F, T, [VTA0, VTA1], VTy]).

/* var declaration */
conWP2smt(T, LT, VTy, SMT) :- functor(T, var, 1), !,
	transfTW(var, Fmt),
    arg(1, T, Arg1), conWP2smt(Arg1, LT, _VTA0, SMT1),
    T = var(VN), write_to_chars(VN, VNCs), string_chars(S, VNCs),
    format(codes(SMT), Fmt, [SMT1]),
    %(recvars(S, VTyT) -> VTyT = VTy ; assertz(recvars(S, VTy))),
    %format("Var ~w: ~w~n", [VN, T]),
    atom_string(U, S), nb_linkval(U, VTy).
    %(regfun(T, [VTA0], VTy) ; format("Point var: ~w~n", [T])).

/* variable */
conWP2smt(T, LT, VTy, SMT) :- functor(T, '$VAR', 1), !, 
    LT = [],
    %format("Var: ~w~n", [T]),
    write_to_chars(T, SMT), string_chars(S, SMT),
    atom_string(U, S), nb_linkval(U, VTy).
    %(recvars(S, VTyT) -> VTyT = VTy ; assertz(recvars(S, VTy))).

/* integer */
conWP2smt(T, LT, VTy, SMT) :- integer(T), !,
    LT = [],
    atom_codes(T, SMT_),
    format(codes(SMT), "~s", [SMT_]), VTy = "integer".
    %format("Integer: ~w~n", [T]).

/* float */
conWP2smt(T, LT, VTy, SMT) :- float(T), !,
    LT = [],
    atom_codes(T, SMT_),
    format(codes(SMT), "~s", [SMT_]), VTy = "float".
    %format("Real: ~w~n", [T]).

/* boolean */
conWP2smt(T, LT, VTy, SMT) :- (T == true ; T == false), !,
    LT = [],
    atom_codes(T, SMT_),
    format(codes(SMT), "~s", [SMT_]), VTy = "boolean".
    %format("Boolean: ~w~n", [T]).

/* pair */
conWP2smt(T, LT, VTy, SMT) :- functor(T, '-', 2), !,
	arg(1, T, Arg1), conWP2smt(Arg1, LT0, VTy0, SMT0),
	arg(2, T, Arg2), conWP2smt(Arg2, LT1, VTy1, SMT1),
	append(LT0, LT1, LT),
	format(codes(SMT), "(mk-pair ~s ~s)", [SMT0, SMT1]), VTy = "pair"-[VTy0, VTy1].

/* nil */
conWP2smt(T, LT, VTy, SMT) :- T = [], !,
	LT = [], VTy = "list"-[_], register_ctor('nil', Ref, VTy),
    format(codes(SMT), "[ctor(~w@~w)]", [nil, Ref]).

/* list */
conWP2smt(T, LT, VTy, SMT) :- functor(T, '[|]', 2), !,
	VTy = "list"-[VTh], VTt = VTy,
    get_args_listW(T, 1, LTH, VTh, Head),
    get_args_listW(T, 2, LTT, VTt, Tail),
    append(LTH, LTT, LT), /*append(VTh, VTt, VTy_), */
    %format("List: [~w|~w] @ [~w|~w]~n", [Head, Tail, VTh, VTt]),
    format(codes(SMT), "(insert ~s ~s)", [Head, Tail]).

/* term/0 */
conWP2smt(T, LT, VTy, SMT) :- functor(T, N, 0), !,
    LT = [(N, 0)],
    write_to_chars(N, SMT),
    string_chars(S, SMT),
    (regfun(S, [], VTy) -> true ; format("Invalid atom: ~w~n", [S])).
    %(regfun(S, [], VTy) ; writeln(regfun(S, [], VTy)), format("Point atom: ~s -> ~w~n", [S, VTy])).
    %format("Atom: ~w @ ~w~n", [T, VTy]).

/* term/Arity */
conWP2smt(T, LT, VTy, SMT) :- functor(T, N, Arity), !,
    write_to_chars(N, SMT1),
    list_of_argsW(T, Arity, LT_, VTy_, SMT2),
    LT = [(N, Arity) | LT_],
    format(codes(SMT), "(~s ~s)", [SMT1, SMT2]),
    string_chars(S, SMT1),
    (regfun(S, VTy_, VTy) -> true ;
    	format("Invalid functor: ~w/~d~n", [S, Arity]),
    	forall(regfun(S, UX0, UX1), format("> available option: ~w -> ~w~n", [UX0, UX1]))
    ).
    %format("Point functor: ~w/~d | ~w -> ~w~n", [S, Arity, VTy_, VTy]).
    
    %(regfun(S, VTy_, VTy) ; writeln(regfun(S, VTy_, VTy)), format("Point functor: ~w~n", [T])).
    %format("Functor ~w: ~w @ ~w -> ~w~n", [N, T, VTy_, VTy]).

/* unsupported term */
conWP2smt(T, LT, _VTy, _SMT) :-
    LT = [],
    throw(unsupported_constraint(T)).


/* Take the Nth arguments of the functor T (useful for lists) */
get_args_listW(T, N, LT, VTy, SMT) :- %[VTy]
    arg(N, T, A),
    conWP2smt(A, LT, VTy, SMT).

/* Create the list of the arguments of the functor T */
list_of_argsW(T, 1, LT, [VTy], Args) :-
    arg(1, T, A),
    conWP2smt(A, LT, VTy, Args).

list_of_argsW(T, I, LT, VTy, Args) :-
    I_ is (I - 1),
    list_of_argsW(T, I_, LT1, VTy0, Args_),
    arg(I, T, A),
    conWP2smt(A, LT2, VTy1, SMT),
    format(codes(Args), "~s ~s", [Args_, SMT]),
    append(LT1, LT2, LT),
    append(VTy0, [VTy1], VTy).


/* binary operators */

%transfTW(>, "(> ~s ~s)").
/*transfT(<, "(< ~s ~s)").
transfT(>=, "(>= ~s ~s)").
transfT(=<, "(<= ~s ~s)").
transfT(*, "(* ~s ~s)").
transfT(+, "(+ ~s ~s)").
transfT(-, "(- ~s ~s)").
transfT(div, "(div ~s ~s)").
transfT(mod, "(mod ~s ~s)").
transfT(rem, "(rem ~s ~s)").
*/
transfTW(=, "(= ~s ~s)").
transfTW(\=, "(not (= ~s ~s))").
transfTW(forall, "(forall ~s ~s)").
transfTW(exists, "(exists ~s ~s)").

/* unary operators */
transfTW(var, "([var(~s)])").
