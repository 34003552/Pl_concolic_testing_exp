:- style_check(-singleton).
:- use_module(type_check).
:- style_check(+singleton).

:- use_module(swiplz3).

/** Tools **/

atoms_strings([], []).
atoms_strings([AH | AT], [SH | ST]) :-
	atom_string(AH, SH), atoms_strings(AT, ST).

atoms_to_terms([], []).
atoms_to_terms([AH | AT], [TH | TT]) :-
	atom_to_term(AH, TH, _), atoms_to_terms(AT, TT).

chars_codes([], []).
chars_codes([Char | Chars], [Code | Codes]) :-
	char_code(Char, Code), chars_codes(Chars, Codes).

replace_chars([], _, _, []).
replace_chars([SrcH | SrcT], Old, New, [DstH | DstT]) :-
	(nth0(I, Old, SrcH) -> (nth0(I, New, Rep), DstH = Rep) ; DstH = SrcH),
	replace_chars(SrcT, Old, New, DstT).

switch(_, [], Goal) :- call(Goal).
switch(X, [Val: Goal | Cases], DefaultGoal) :-
	(X = Val -> call(Goal) ; switch(X, Cases, DefaultGoal)).

/** SMTLIB2 parser **/

is_ws(Ch) :- char_type(Ch, space).
smtlib2_is_inst_char(Ch) :- char_type(Ch, csym) ; member(Ch, ['$', '-']).
smtlib2_is_arg_char(Ch) :- char_type(Ch, csym) ; member(Ch, ['/', '=', '+', '-', '*', '<', '<=', '>', '>=']).

ws --> [C], { char_code(Ch, C), is_ws(Ch) }, !, ws | smtlib2_comment | [].
smtlib2_comment --> ";", smtlib2_comment_aux.
smtlib2_comment_aux --> "\n" | [C], { char_code(Ch, C), char_type(Ch, ascii) }, !, smtlib2_comment_aux.
smtlib2_grammar --> ws, smtlib2_block, ws, !, smtlib2_grammar | [].
smtlib2_block --> "(", ws, smtlib2_inst(Inst), ws, smtlib2_args(Args), ws, ")", { execute(Inst, Args) }.
smtlib2_inst(Inst) --> smtlib2_inst_aux([], Inst).
smtlib2_inst_aux(Chars, Inst) -->
	[C], { char_code(Ch, C), smtlib2_is_inst_char(Ch), append(Chars, [Ch], NChars) }, !, smtlib2_inst_aux(NChars, Inst)
	| { string_chars(Inst, Chars) }.
smtlib2_args(Args) -->
	smtlib2_arg(ArgsH), { ArgsH \= '' }, ws, !, smtlib2_args(ArgsT), { Args = [ArgsH | ArgsT] }
	| [], { Args = [] }.
smtlib2_arg(Arg) --> smtlib2_arg_aux([], Arg).
smtlib2_arg_aux(Chars, Arg) -->
	[C], { char_code(Ch, C), smtlib2_is_arg_char(Ch), append(Chars, [C], NChars) }, !, smtlib2_arg_aux(NChars, Arg)
	| "(", ws, !, smtlib2_args(NArgs), ws, ")", { Arg = NArgs }
	| { atom_chars(Arg, Chars) }.

/** Z3 interpreter **/

init_context :-
	z3_mk_config(),
	z3_set_param_value("model", "true"),
	z3_mk_context(ContextID),
	z3_mk_solver(ContextID),
	z3_del_config(),
	assert(context_id(ContextID)).

del_context :-
	context_id(ContextID),
	z3_del_solver(ContextID),
	z3_del_context(ContextID),
	retract(context_id(ContextID)).

display_get_model(_, [], []).
display_get_model(Fmt, [VarName | VarNames], [VarValue | VarValues]) :-
	format(string(String), Fmt, [VarName, VarValue]),
	writeln(String),
	display_get_model(Fmt, VarNames, VarValues).

execute(Instruction, Args) :-
	context_id(ContextID),
	length(Args, Argc),
	switch(Instruction, [
		"$push":
		(
			(Argc = 0 -> true ; throw("'$push' expects no arguments!")),
			z3_push(ContextID)
		),
		"$pop":
		(
			(Argc = 1 -> true ; throw("'$pop' expects 1 argument!")),
			z3_pop(ContextID, [])
		),
		"$mk_int_vars":
		(
			(Argc >= 1 -> true ; throw("'$mk_int_vars' expects at least 1 argument!")),
			atoms_strings(Args, Args_S),
			z3_mk_int_vars(ContextID, Args_S)
		),
		"$mk_term_type":
		(
			(Argc >= 3 -> true ; throw("'$mk_term_type' expects at least 3 arguments!")),
			[Terms_A, NeedInt, NeedLists] = Args,
			atoms_to_terms(Terms_A, Terms),
			z3_mk_term_type(ContextID, Terms, NeedInt, NeedLists)
		),
		"$mk_term_vars":
		(
			(Argc >= 1 -> true ; throw("'$mk_term_vars' expects at least 1 argument!")),
			atoms_strings(Args, Args_S),
			z3_mk_term_vars(ContextID, Args_S)
		),
		"$assert_int_string":
		(
			(Argc = 1 -> true ; throw("'$assert_int_string' expects 1 argument!")),
			[Assertion] = Args,
			write_to_chars(Assertion, Codes),
			chars_codes(Chars, Codes),
			replace_chars(Chars, ['[', ']', ','], ['(', ')', ' '], NChars),
			string_chars(String, NChars),
			z3_assert_int_string(ContextID, String)
		),
		"$assert_term_string":
		(
			(Argc = 3 -> true ; throw("'$assert_term_string' expects 3 arguments!")),
			[Assertion, NeedInt, NeedLists] = Args,
			write_to_chars(Assertion, Codes),
			chars_codes(Chars, Codes),
			replace_chars(Chars, ['[', ']', ','], ['(', ')', ' '], NChars),
			string_chars(String, NChars),
			z3_assert_term_string(ContextID, String, NeedInt, NeedLists)
		),
		"$check":
		(
			(Argc = 0 -> true ; throw("'$check' expects no arguments!")),
			(z3_check(ContextID) -> writeln("SAT") ; writeln("UNSAT or UNKNOWN"))
		),
		"$print_model":
		(
			(Argc = 0 -> true ; throw("'$print_model' expects no arguments!")),
			z3_print_model(ContextID, String),
			writeln(String)
		),
		"$get_model_intvar_eval":
		(
			(Argc >= 1 -> true ; throw("'$get_model_intvar_eval' expects at least 1 argument!")),
			atoms_strings(Args, VarNames),
			get_model_var_eval(ContextID, VarNames, VarValues),
			display_get_model('(define-const ~s Int ~d)', VarNames, VarValues)
		),
		"$get_model_termvar_eval":
		(
			(Argc >= 1 -> true ; throw("'$get_model_termvar_eval' expects at least 1 argument!")),
			atoms_strings(Args, VarNames),
			get_model_varT_eval(ContextID, VarNames, VarValues),
			display_get_model('(define-const ~s Term ~w)', VarNames, VarValues)
		),

		"declare-const":
		(
			(Argc == 2 -> true ; throw("'declare-const' expects 2 arguments!")),
			[VarName, VarType] = Args,
			switch(VarType, [
				'Int':
					execute("$mk_int_vars", [VarName]),
				'Term':
					execute("$mk_term_vars", [VarName])
			],
				(
					format(string(Error), "Unrecognized datatype: ~w", [VarType]),
					throw(Error)
				)
			)
		),
		"push":
		(
			(Argc = 0 -> true ; throw("'push' expects no arguments!")),
			execute("$push", [])
		),
		"pop":
		(
			(Argc = 0 -> true ; throw("'pop' expects no arguments!")),
			execute("$pop", [_])
		),
		"check-sat":
		(
			(Argc = 0 -> true ; throw("'check-sat' expects no arguments!")),
			execute("$check", [])
		),
		"reset":
		(
			(Argc = 0 -> true ; throw("'reset' expects no arguments!")),
			del_context(),
			init_context()
		),
		"declare-datatypes":
		(
			(Argc = 2 -> true ; throw("'declare-datatypes' expects 2 arguments!")),
			throw("Not yet implemented!")
		)
	],
		(
			format(string(String), "Unrecognized instruction: ~s", [Instruction]),
			throw(String)
		)
	).

/** Main program **/

main :-
	writeln("-----------------------------"),
	writeln("Welcome to the swiplz3 tester"),
	writeln("-----------------------------"),

	current_prolog_flag(argv, Args),
	(Args == [] -> (
			throw("You must specify a script to load!")
		) ; (
			[ScriptPath | _] = Args
		)
	),

	init_context(),

	write("Loading script from: "), writeln(ScriptPath), nl,
	phrase_from_file(smtlib2_grammar, ScriptPath),

	del_context().