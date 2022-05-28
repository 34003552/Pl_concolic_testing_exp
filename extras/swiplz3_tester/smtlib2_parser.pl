
:- module(smtlib2_parser, [parse_file/2]).

comment_aux --> [C], { code_type(C, end_of_line) } | [_], !, comment_aux | [].
comment --> ";", !, comment_aux.

ws --> [C], { code_type(C, space) }, !, ws | comment | [].

id_aux(NCs) -->
    [C], { char_code('-', C) ; code_type(C, prolog_identifier_continue) }, !, id_aux(Cs), { NCs = [C | Cs] }
    | [], { NCs = [] }.
id(Id) --> [C], { char_code('$', C) ; code_type(C, prolog_var_start) ; code_type(C, prolog_atom_start) }, id_aux(Cs), { atom_codes(Id, [C | Cs]) }.
num_aux(NCs) --> [C], { char_code('.', C) }, !, num_aux(Cs), { \+member(C, Cs), NCs = [C | Cs] }.
num_aux(NCs) --> [C], { code_type(C, digit) }, !, num_aux(Cs), { NCs = [C | Cs] } | [], { NCs = [] }.
num(Num) --> [C], { code_type(C, digit) }, !, num_aux(Cs), { atom_codes(Num, [C | Cs]) }.
symbol_aux(NCs) -->
    [C], { code_type(C, prolog_symbol) }, !, symbol_aux(Cs), { NCs = [C | Cs] }
    | [], { NCs = [] }.
symbol(Sym) --> [C], { code_type(C, prolog_symbol), \+char_code('(', C), \+char_code(')', C) }, !, symbol_aux(Cs), { atom_codes(Sym, [C | Cs]) }.

inst_name(InstName) --> id(Id), { atom_string(Id, InstName) }.
inst_args(InstArgs) -->
    (id(Arg) | num(Arg) | symbol(Arg) | "(", ws, !, inst_args(Arg), ws, ")"), ws, !, inst_args(Args), { InstArgs = [Arg | Args] }
    | [], { InstArgs = [] }.

instruction --> "(", ws, inst_name(Inst), ws, inst_args(Args), ws, ")", {
        nb_getval('executor', Executor),
        call(Executor, Inst, Args)
    }.
grammar --> ws, (instruction, !, grammar | []).

parse_file(ScriptPath, Executor) :-
    nb_setval('executor', Executor),
    phrase_from_file(grammar, ScriptPath).