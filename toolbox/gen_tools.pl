
:- module(gen_tools, [
    use_dual_foreign_library/0,
    parse_file_lines/2,
    generate_list/3,
    replace_chars/3,
    switch/3,
    convnestedlist/3
]).

:- module_transparent([
    use_local_foreign_library/1,
    use_dual_foreign_library/0,
    parse_file_lines/2,
    generate_list/3,
    replace_chars/3,
    switch/3,
    convnestedlist/3
]).

use_local_foreign_library(Lib) :-
    prolog_load_context(directory, AbsDir),
    relative_file_name(AbsDir, './', RelDir),
    (RelDir = '' -> RelLib = Lib ; directory_file_path(RelDir, Lib, RelLib)),
    (current_prolog_flag(windows, true) -> Spec = foreign(RelLib) ; Spec = RelLib),
    use_foreign_library(Spec),              % load using install
    at_halt(unload_foreign_library(Spec)).  % unload using uninstall

use_dual_foreign_library :- context_module(Module), use_local_foreign_library(Module).


parse_line(end_of_file, _Gram).
parse_line(Line, Gram) :- Line =.. List, phrase(Gram, List).

parse_stream_lines(Stream, _Gram) :- at_end_of_stream(Stream), !.
parse_stream_lines(Stream, Gram) :-
    read(Stream, Line),
    parse_line(Line, Gram),
    parse_stream_lines(Stream, Gram).

parse_file_lines(File, Gram) :-
    open(File, read, Stream),
    strip_module(Gram, Mod, Gram_),
    parse_stream_lines(Stream, Mod:Gram_),
    close(Stream).


generate_list_aux(_P, range(I, N, _S), []) :- I >= N, !.
generate_list_aux(P, range(I, N, S), [O | L]) :-
    call(P, I, O), J is I + S,
    generate_list_aux(P, range(J, N, S), L).

generate_list(P, range(I, N, S), L) :- !,
    generate_list_aux(P, range(I, N, S), L).
generate_list(P, N, L) :- integer(N), !,
    generate_list(P, range(0, N, 1), L).
generate_list(P, range(I, N), L) :-
    generate_list(P, range(I, N, 1), L).


replace_chars_aux(A, [], A).
replace_chars_aux(A, [A -> B | _], B) :- !.
replace_chars_aux(A, [_ | T], B) :- replace_chars_aux(A, T, B).

replace_chars(I, Rep, O) :- convlist({Rep}/[A,B]>>replace_chars_aux(A,Rep,B), I, O).


switch(_, [], Goal) :-
    (Goal =.. ['{}', Goal_] -> true ; Goal_ = Goal), call(Goal_).
switch(X, [Val: Goal | Cases], DefaultGoal) :-
    (X = Val ->
        (Goal =.. ['{}', Goal_] -> true ; Goal_ = Goal), call(Goal_)
    ;
        switch(X, Cases, DefaultGoal)
    ).


convnestedlist(_, [], []).
convnestedlist(Goal, [IH | IT], [OH | OT]) :- is_list(IH), !,
    convnestedlist(Goal, IH, OH), convnestedlist(Goal, IT, OT).
convnestedlist(Goal, [IH | IT], [OH | OT]) :-
    call(Goal, IH, OH), convnestedlist(Goal, IT, OT).


/*
atoms_strings([], []).
atoms_strings([AH | AT], [SH | ST]) :-
    atom_string(AH, SH), atoms_strings(AT, ST).
*/

/*
chars_codes([], []).
chars_codes([Char | Chars], [Code | Codes]) :-
    char_code(Char, Code), chars_codes(Chars, Codes).
*/

/*
atoms_to_terms([], []).
atoms_to_terms([AH | AT], [TH | TT]) :-
    atom_to_term(AH, TH, _), atoms_to_terms(AT, TT).
*/

/*
replace_atomvars_aux([], _, []).
replace_atomvars_aux([A = _ | W], V, [A = D | W_]) :- (member(A = C, V) -> D = C ; D = A), replace_atomvars_aux(W, V, W_).
replace_atomvars(A, V, A_) :- atom_to_term(A, _T, W), replace_atomvars_aux(W, V, W_), read_term_from_atom(A, T_, [variable_names(W_)]), with_output_to(atom(A_), write_term(T_, [quoted(false)])).

prefix_atomvars_aux([], _, []).
prefix_atomvars_aux([V = _ | Vs], P, [V = M | Vs_]) :- format(atom(M), '~w~w', [P, V]), prefix_atomvars_aux(Vs, P, Vs_).
prefix_atomvars(A, P, A_) :- atom_to_term(A, _, W), prefix_atomvars_aux(W, P, V), replace_atomvars(A, V, A_).

:- replace_atomvars('u(R, S, f(U, S))', ['S'='W', 'R'='T'], Z), writeln(Z).
:- prefix_atomvars('u(R, S, f(U, S))', apple, Z), writeln(Z).
*/