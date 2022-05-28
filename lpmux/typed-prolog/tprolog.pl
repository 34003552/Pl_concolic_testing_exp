
:- module(tprolog, [read_type_annotations/3]).

packet_aux(Cat, Delim, List) -->
    Delim, !, ws, packet(Cat, Delim, List) |
    [], { List = [] }.
packet(Cat, Delim, List) -->
    call(Cat, E), !, ws, packet_aux(Cat, Delim, List_), { List = [E | List_] } |
    [], { List = [] }.
    
ws --> [C], { code_type(C, space) }, !, ws | [].


identifier_aux2(I) --> [C], { \+char_code('\'', C) , I = [C | T] }, !, identifier_aux2(T) | { I = [] }.
identifier_aux(I) -->
    [C], { code_type(C, csym), I = [C | T] }, !, identifier_aux(T) |
    "'", !, identifier_aux2(I), "'" |
    [], { I = [] }.
identifier(I) --> identifier_aux(I_), { atom_codes(I, I_) }.
variable_aux(TA) -->
    [C], { code_type(C, prolog_identifier_continue) }, !, variable_aux(TAT), { TA = [C | TAT] } |
    [], { TA = [] }.
variable(TA) -->
    [C], { code_type(C, prolog_var_start) }, !, variable_aux(TAT), { TA_ = [C | TAT], atom_codes(TA, TA_) }.

:- dynamic required_builtin_types/1.
cmpnd(Cmpnd) -->
    variable(Var), !, { Cmpnd = '$VAR'(Var) } |
    identifier(PName), (
        ws, "(", !, ws, packet(cmpnd, ",", PArgs), ws, ")" | [], { PArgs = [] }
    ), {
        Cmpnd =.. [PName | PArgs],
        ((\+required_builtin_types(Cmpnd), member(Cmpnd, [integer, float, boolean, list(_), pair(_,_)])) ->
            assertz(required_builtin_types(Cmpnd)) ; true)
    }.
rtype(TName, TEnum) -->
    ":-", ws, "type", !, ws, cmpnd(TName_), ws, "--->", ws, packet(cmpnd, ";", TEnum_), ws, ".", {
        with_output_to(atom(VNA), write_term([TName_ | TEnum_], [numbervars(true), quoted(true)])),
        read_term_from_atom(VNA, [TName | TEnum], [])
    }.
rpred(Pred) -->
    ":-", ws, "pred", !, ws, cmpnd(Pred), ws, ".".
trash --> "\n" | [_C], !, trash.
gram(T, P) -->
    rtype(Th, Ah), !, gram(Tl, P), { T = [Th-Ah | Tl] } |
    rpred(Pred), !, gram(T, Pl), { P = [Pred | Pl] } |
    trash, !, gram(T, P) |
    [], { T = [], P = [] }.

read_type_annotations(File, Types, PredTypes) :-
    phrase_from_file(gram(Types_, PredTypes), File),

    findall(BT, required_builtin_types(BT), BuiltinTypes),
    retractall(required_builtin_types(_)),
    append([BuiltinTypes, Types_], Types).