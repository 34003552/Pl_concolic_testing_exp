
:- module(mparser, [load_file/1]).

:- ensure_loaded('../../toolbox/print_tools').
:- ensure_loaded(mlib).


get_stvar_name(HA, VName) :- atom_to_term(HA, H, _), H = '$VAR'(H0), ground(H0), atom_chars(H0, ['!' | VName]).

init_stvars([]).
init_stvars([H | T]) :- (get_stvar_name(H, VName) ->
        format(atom(Idx), '~s.count', [VName]),
        (nb_current(Idx, I) -> format(atom(IdxBk), '~s.old_count', [VName]), nb_setval(IdxBk, I) ; true),
        nb_setval(Idx, 0) ; true
    ), init_stvars(T).

/*split_stvars([], []).
split_stvars([H | T], Args_) :- (get_stvar_name(H, VName) ->
        format(atom(Idx), '~s.count', [VName]),
        (nb_current(Idx, I) -> true ; format(atom(W), 'state variable ~s uninitialized!', [VName]), throw(W)),
        format(atom(VName0), '_~s~d', [VName, I]), format(atom(VName1), '_~s~d', [VName, I+1]),
        nb_setval(Idx, I+1),
        Args_ = ['$VAR'(VName0), '$VAR'(VName1) | Args__] ; Args_ = [H | Args__]
    ), split_stvars(T, Args__).*/
split_stvars([], []).
split_stvars([H | T], Args_) :- (get_stvar_name(H, VName) ->
        format(atom(Idx), '~s.count', [VName]),
        (nb_current(Idx, I) -> true ; format(atom(W), 'state variable ~s uninitialized!', [VName]), throw(W)),
        format(atom(VName0), '_~s~d', [VName, I]), format(atom(VName1), '_~s~d', [VName, I+1]),
        nb_setval(Idx, I+1),
        Args_ = [VName0, VName1 | Args__] ; Args_ = [H | Args__]
    ), split_stvars(T, Args__).

/*clean_stvars([], []).
clean_stvars([H | T], Args_) :- (get_stvar_name(H, VName) ->
        format(atom(Idx), '~s.count', [VName]),
        nb_getval(Idx, I),
        format(atom(VName0), '_~s0', [VName]), format(atom(VName_), '_~s~d', [VName, I]),
        format(atom(IdxBk), '~s.old_count', [VName]), (nb_current(IdxBk, J) -> nb_setval(Idx, J) ; nb_delete(Idx)),
        Args_ = ['$VAR'(VName0), '$VAR'(VName_) | Args__] ; Args_ = [H | Args__]
    ), clean_stvars(T, Args__).*/
clean_stvars([], []).
clean_stvars([H | T], Args_) :- (get_stvar_name(H, VName) ->
        format(atom(Idx), '~s.count', [VName]),
        nb_getval(Idx, I),
        format(atom(VName0), '_~s0', [VName]), format(atom(VName_), '_~s~d', [VName, I]),
        format(atom(IdxBk), '~s.old_count', [VName]), (nb_current(IdxBk, J) -> nb_setval(Idx, J) ; nb_delete(Idx)),
        Args_ = [VName0, VName_ | Args__] ; Args_ = [H | Args__]
    ), clean_stvars(T, Args__).

destructure_head_aux(_I, [], [], []).
destructure_head_aux(I, [H|T], [H_|T_], A_) :-
    (atom_to_term(H, '$VAR'(_), _) -> H_ = H, A_ = A ;
        format(atom(H_), '_A~d', [I]),
        format(atom(K), "apply_known_pred(_:'=', [~w, ~w])", [H_, H]),
        A_ = [K | A]
    ), destructure_head_aux(I + 1, T, T_, A).
destructure_head(Args, Args_, As) :-
    destructure_head_aux(0, Args, Args_, As_),
    atomics_to_string(As_, ", ", As).


comment_aux1 --> [C], { \+code_type(C, end_of_line) }, !, comment_aux1 | [].
comment_aux2 --> "*/" | [_], !, comment_aux2.
comment --> "%", !, comment_aux1 | "/*", !, comment_aux2.
ws --> [C], { code_type(C, space) }, !, ws | comment, !, ws | [].

id_aux(NCs) --> [C], { code_type(C, prolog_identifier_continue) }, !, id_aux(Cs), { NCs = [C | Cs] } | [], { NCs = [] }.
id(ID) --> [C], { code_type(C, prolog_atom_start) }, !, id_aux(Cs), { atom_codes(ID, [C | Cs]) }.

var_aux(NCs) --> [C], { code_type(C, prolog_identifier_continue) }, !, var_aux(Cs), { NCs = [C | Cs] } | [], { NCs = [] }.
var(Var) --> [C], { code_type(C, prolog_var_start) ; char_code('!', C) }, !, var_aux(Cs), { atom_codes(V, [C | Cs]), Var = '$VAR'(V) }.

args_aux(Cat, L) --> ",", !, ws, call(Cat, A), ws, args_aux(Cat, L_), { L = [A | L_] } | [], { L = [] }.
args(Cat, L) --> call(Cat, A), !, ws, args_aux(Cat, L_), { L = [A | L_] } | [], { L = [] }.
alts_aux(Cat, L) --> ";", !, ws, call(Cat, A), ws, alts_aux(Cat, L_), { L = [A | L_] } | [], { L = [] }.
alts(Cat, L) --> call(Cat, A), !, ws, alts_aux(Cat, L_), { L = [A | L_] } | [], { L = [] }.

num_aux(NCs) --> [C], { code_type(C, digit) }, !, num_aux(Cs), { NCs = [C | Cs] } | [], { NCs = [] }.
num(N) --> [C], { code_type(C, digit) ; char_code('-', C) }, !, num_aux(Cs), { number_codes(N, [C | Cs]) }.

string_aux(Cs) --> "\"", !, { Cs = [] } | [C], !, string_aux(Cs_), { Cs = [C | Cs_] }.
string(S) --> "\"", !, string_aux(S_), { string_codes(S, S_) }.

list(L) --> "[", !, ws, args(expr, L_), (ws, "|", !, ws, term(T) | [], { T = [] }), ws, "]", { /*convlist([I,O]>>(I = '$VAR'(V)), L_, L__),*/ append(L_, T, L) }.

module --> "module", !, ws, id(Name), {
    nb_setval('module', Name),
    vv_format("declared module: ~s~n", [Name])
}.
end_module --> "end_module", !, ws, id(Name), {
    vv_format("declared end_module: ~s~n", [Name])
}.
interface --> "interface", !, {
    vv_format("@interface~n")
}.
import_module --> "import_module", !, ws, id(Mod), {
    mlib:import_module_preds(Mod),
    mlib:import_module_funcs(Mod),
    mlib:import_module_types(Mod),

    vv_format("imported module: ~s~n", [Mod])
}.
det_cat(D_) --> id(D), { mlib:known_dets(D), atom_string(D, D_) }.
mode_cat(M_) --> id(M), { mlib:known_modes(M, _), atom_string(M, M_) }.
type(T_) --> ftor(T), { /*mlib:known_types(T, _),*/ term_string(T, T_) }.
mode --> "mode", !, ws, id(Name), ws, "(", ws, args(mode_cat, ArgModes), ws, ")", ws, "is", ws, det_cat(Det), {
    
    atomics_to_string(ArgModes, ", ", ArgModes_),
    vv_format("declared mode: ~s(~s) is ~s~n", [Name, ArgModes_, Det])
}.
pred --> "pred", !, ws, id(Name), ws, "(", ws, args(type, ArgTypes), ws, ")", {
    
    atomics_to_string(ArgTypes, ", ", ArgTypes_),
    vv_format("declared predicate: ~s(~s)~n", [Name, ArgTypes_]),

    format(atom(P), '~s(~s)', [Name, ArgTypes_]), assertz(mlib:known_pred_types(P))
}.
pred_mode_arg(Type-Mode) --> term(Type), ws, "::", ws, term(Mode).%type(Type), ws, "::", ws, mode_cat(Mode).
pred_mode --> "pred", !, ws, id(Name), ws, "(", ws, args(pred_mode_arg, Args), ws, ")", ws, "is", ws, det_cat(Det), {
    
    convlist([Type-Mode, S]>>format(string(S), "~w::~w", [Type, Mode]), Args, Args_),
    atomics_to_string(Args_, ", ", Args__),
    vv_format("declared predicate: ~s(~s) is ~s~n", [Name, Args__, Det]),

    convlist([Type-_Mode, S]>>format(string(S), "~w", [Type]), Args, ArgTypes), atomics_to_string(ArgTypes, ", ", ArgTypes_),
    format(atom(P), '~s(~s)', [Name, ArgTypes_]), assertz(mlib:known_pred_types(P))
}.
func --> "func", !, ws, id(Name), (ws, "(", ws, args(type, ArgTypes), ws, ")" | [], { ArgTypes = [] }), ws, "=", ws, type(RetType), {
    
    atomics_to_string(ArgTypes, ", ", ArgTypes_),
    vv_format("declared function: ~s(~s) = ~s~n", [Name, ArgTypes_, RetType]),

    format(atom(P), '~s(~s, ~s)', [Name, ArgTypes_, RetType]), assertz(mlib:known_func_types(P))
}.
func_mode --> "func", !, ws, id(Name), (ws, "(", ws, args(pred_mode_arg, Args), ws, ")" | [], { Args = [] }), ws, "=", ws, "(", ws, pred_mode_arg(Ret), ws, ")", ws, "is", ws, det_cat(Det), {
    
    convlist([Type-Mode, S]>>format(string(S), "~s::~s", [Type, Mode]), Args, Args_),
    atomics_to_string(Args_, ", ", Args__),
    Ret = RetType-RetMode, format(string(Ret_), "(~s::~s)", [RetType, RetMode]),
    vv_format("declared function: ~s(~s) = ~s is ~s~n", [Name, Args__, Ret_, Det]),

    convlist([Type-_Mode, S]>>format(string(S), "~w", [Type]), Args, ArgTypes), atomics_to_string(ArgTypes, ", ", ArgTypes_),
    format(atom(P), '~s(~s, ~s)', [Name, ArgTypes_, RetType]), assertz(mlib:known_func_types(P))
}.

implementation --> "implementation", !, {
    vv_format("@implementation~n")
}.
typedecl --> "type", !, ws, id(Name), (ws, "(", !, ws, args(expr, Args), ws, ")" | [], { Args = [] }), (ws, "--->", !, ws, alts(expr, Enum) | [], { Enum = [] }), {

    F =.. [Name | Args],
    %print(hjx-Enum),nl,
    assertz(mlib:known_types(F, Enum)),
    convlist([I,O]>>term_to_atom(I,O), Enum, Enum__), atomics_to_string(Enum__, " ; ", Enum_),
    (Enum == [] ->
        vv_format("declared type: ~w~n", [F]) ;
        vv_format("declared type: ~w ---> ~s~n", [F, Enum_])
    )
}.
typealias --> "type", !, ws, id(_Name), (ws, "(", !, ws, args(expr, _Args), ws, ")" | [], { _Args = [] }), ws, "==", ws, ftor(_Type).
instdecl --> "inst", !, ws, id(_Name), ws, "for", ws, id(_N), "/", num(_A), ws, "--->", ws, alts(term, _Enum).
modealias --> "mode", !, ws, id(_Name), ws, "==", ws, term(_T).
clhead(F) --> id(ID), ws, "(", ws, args(expr, Vs), ws, ")", { F =.. [ID | Vs] }.
rsvprfx --> module | end_module | interface | import_module | modealias | typealias | pred | mode | pred_mode | func | func_mode | implementation | typedecl | instdecl.
ftor(F) --> (id(Mod), "." | []), id(FName), !, (ws, "(", !, ws, args(expr, FArgs), ws, ")" | [], { FArgs = [] }), {
    F_ =.. [FName | FArgs], (ground(Mod) -> F = Mod:F_ ; F = F_)
    }.
numexpr(E) --> (var(T0) | num(T0)), ws, "-", !, ws, (var(T1) | num(T1)), {
    format(atom(A), '~w - ~w', [T0, T1]),
    convlist([I,O]>>(I = '$VAR'(V), O = (V = I)), [T0, T1], VNs),
    read_term_from_atom(A, E, [variable_names(VNs)])
    /*writeln(A), atom_to_term(A, E, _)*/
    }.
closure_mode(C) --> id(_)/*mode_cat(_Ret)*/, ws, "is", ws, det_cat(_), { C = "$LAMBDA" }.
closure_def(C) --> expr(Ret), /*{ init_stvars([]) },*/ (ws, clbody(B),! | [], { B = true }), { format(atom(C), '[~w]>>(~w)', [Ret, B]) }.
closure(C) --> "(func)", ws, "=", !, ws, (closure_mode(C),! | closure_def(C)).
term(Term) --> num(Term) | string(Term) | var(Term) | ftor(Term) | id(Term) | list(Term) | numexpr(Term) /*| closure(Term)*/.

goal_call(G) --> (id(Mod), "." | []), id(PName), !, ws, ("(", !, ws, args(expr, PArgs), ws, ")" | [], { PArgs = [] }), {
    PArgs = PArgs__, %convlist([I,O]>>atom_to_term(I, O, _), PArgs, PArgs__),
    split_stvars(PArgs__, PArgs_),
    %print(erf-PArgs-PArgs__-PArgs_),
    %length(PArgs_, PArity),
    (var(Mod) -> M = '_'; M = Mod),
    %format(atom(G), '(known_preds(~w, ~w/~d, ~w) -> apply(~w, ~p) ; fail)', [M, PName, PArity, W, W, PArgs_])
    
    format(atom(G), 'apply_known_pred(~w:~w, ~w)', [M, PName, PArgs_])/*, print(G),nl*/
}.
goal_if(G) --> "if", !, ws, goal_and(C), ws, "then", ws, goal_and(G0), ws, "else", ws, goal_and(G1), {
    format(atom(G), '~w -> ~w ; ~w', [C, G0, G1])
}.

goal_infix_pos(G) --> var(V), ws, ">", ws, "0", { format(atom(G), '~p > 0', [V]) }.
goal_infix_le(G) --> "N =< 2", { format(atom(G), '~p =< 2', ['$VAR'('N')]) }.
goal_infix_eq(G) --> /*"X = 1", !, { format(atom(G), '~p =:= 1', ['$VAR'('X')]) }
    |*/ "X = A + B", !, { format(atom(G), '~p = ~p + ~p', ['$VAR'('X'), '$VAR'('A'), '$VAR'('B')]) }
    %| "Stanza", ws, "=", ws, expr(S), { format(atom(G), '~p = ~w', ['$VAR'('Stanza'), S]) }.
    | var(V), ws, "=", ws, expr(S), { format(atom(G), "apply_known_pred(_:'=', [~p, ~w])", [V, S])/*, print(G),nl*/ }.
goal_infix(G) --> goal_infix_le(G) | goal_infix_pos(G) /*| goal_infix_eq0(G) | goal_infix_eq1(G)*/ | goal_infix_eq(G).% | goal_xfib(G). %expr, ws, "=", ws, expr, {}.
goal(G) --> "(", !, ws, goal(GH), ws, clbody_aux(GT), ws, ")", {
        (GT == [] -> G_ = GH ; atomics_to_string(GT, ', ', W), format(atom(G_), '~w, ~w', [GH, W])),
        format(atom(G), '(~w)', [G_])
    }
    | goal_infix(G) | goal_if(G) | goal_call(G),! | goal_or(G).
goal_and(G) --> goal(G0), ws, (",", !, ws, goal_and(G_), { format(atom(G), '~w, ~w', [G0, G_]) } | [], { G = G0 }).
goal_or(G) --> goal_and(G0), ws, (";", !, ws, goal_or(G_), { format(atom(G), '~w ; ~w', [G0, G_]) } | [], { G = G0 }).

aleph(Goal) --> rsvprfx, { Goal = true } | goal(Goal).
clbody_aux(Goal) --> ",", !, ws, aleph(GoalH), ws, clbody_aux(GoalT), { Goal = [GoalH | GoalT] } | [], { Goal = [] }.
clbody(Goal) --> ":-", !, ws, aleph(GoalH), ws, clbody_aux(GoalT), {
    (GoalT == [] -> Goal = GoalH ; atomics_to_string(GoalT, ', ', W), format(atom(Goal), '~w, ~w', [GoalH, W]))
}.
cl --> (clhead(F), ! | [], { F = '' }), ws, { F =.. [PName | PArgs], init_stvars(PArgs) }, (clbody(G_), ws | [], { G_ = true }), ".", {
    clean_stvars(PArgs, PArgs___),
    PArgs___ = PArgs__, %destructure_head(PArgs___, PArgs__, Y),
    G = G_, %(Y == "" -> G = G_ ; format(atom(G), '~w, ~w', [Y, G_])),

    format(atom(A), '~w>>(~w)', [PArgs__, G]),
    %print(cl-A),nl,
    atom_to_termX(A, Goal, _),
    length(PArgs__, PArity),
    mlib:add_user_pred(PName/PArity, Goal),

    ((F == '', G == true) -> true ;
        (PName = '' -> vv_format("defined clause :- ~a~n", [G]) ;
            F_ =.. [PName | PArgs__], vv_format("defined clause ~w :- ~a~n", [F_, G])
        )
    )
}.

expr_if(E) --> "if", !, ws, goal(C), ws, "then", ws, expr(E0), ws, "else", ws, expr(E1), {
        format(atom(E), 'expr_if(~w, ~w, ~w)', [C, E0, E1])
    }.

expr_string(E) -->
    expr_rec(E0), ws, "++", !,ws, expr_string(E1), { format(atom(E), "string:'++'(~w, ~w)", [E0, E1]) }
    | expr_sum(E).

expr_list(E) --> ("[",ws,"]", !, { L_ = [], T = [] }
    | "[", !, ws, args(expr, L_), (ws, "|", !, ws, expr(T) | [], { T = [] }), ws, "]"), {
    atomics_to_string(L_, ', ', S), (T = [] -> format(atom(E), "[~s]", [S]) ; format(atom(E), "[~s | ~s]", [S, T]))
    %print(E),nl
    }.

atom_aux(Cs) --> "'", !, { Cs = [] } | [C], atom_aux(Cs_), { Cs = [C | Cs_] }.
atom(A) --> "'", !, atom_aux(Cs), { atom_codes(A, Cs) }.

expr_prim(E) -->
    var(V), { /*print(V),nl,*/ format(atom(E), "~p", [V]) }
    | num(N), { format(atom(E), "~p", [N]) }
    | string(S), { format(atom(E), '"~s"', [S]) }
    | atom(A), { format(atom(E), "'~a'", [A]) }
    | expr_list(L), { format(atom(E), "~w", [L]) }
    %| ftor(F),{ F =.. [Name | ArgsE], convlist([I,O]>>term_to_atom(I,O),ArgsE, Args), atomics_to_string(Args, ", ", Args_), format(atom(E), "~p(~w)", [Name, Args_]), print(E),nl }
    | ftor(F), {
        (Mod:F_ = F -> true ; F_ = F),
        with_output_to(atom(F__), write_term(F_, [quoted(false)])),
        (ground(Mod) -> format(atom(E), "~w:~w", [Mod, F__]) ; format(atom(E), "~w", [F__]))
        %print(esz-E),nl
    }.

expr_rec(E) -->
    "(", !, ws, expr(E0), ws, ")", { format(atom(E), '(~w)', [E0]) }
    | expr_prim(E).

expr_pro(E) -->
    expr_rec(E0), ws, "*", ws, expr_pro(E1), { format(atom(E), '~w * ~w', [E0, E1]) }
    | expr_rec(E0), ws, "//", ws, expr_pro(E1), { format(atom(E), '~w // ~w', [E0, E1]) }
    | expr_rec(E0), ws, "rem", ws, expr_pro(E1), { format(atom(E), '~w rem ~w', [E0, E1]) }
    | expr_rec(E).
expr_neg(E) -->
    "-", ws, expr_pro(E0), { format(atom(E), '-~w', [E0]) }
    | expr_pro(E).
expr_sum(E) -->
    expr_neg(E0), ws, "+", ws, expr_sum(E1), { format(atom(E), '~w + ~w', [E0, E1]) }
    | expr_neg(E0), ws, "-", ws, expr_sum(E1), { format(atom(E), '~w - ~w', [E0, E1]) }
    | expr_neg(E).

expr(E) --> expr_if(E),! | closure(E),! | expr_string(E),! | expr_sum(E).
/*expr(_E) --> lazy_list_location(file(Name, Line, LinePos, CharNo)), {
        format(string(S), "error while parsing '~w' at line ~d:~d with '~c'(~d)~n", [Name, Line, LinePos, CharNo, CharNo]), throw(S)
    }.*/
%exprT(E) --> expr(E_), { read_term_from_atom(E_, E, [variable_names(W)]), foreach(member(K=V, W), V = '$VAR'(K)), print(W),nl }.
atom_to_termX(I, O, _) :- read_term_from_atom(I, O, [variable_names(_W)]) /*,foreach(member(K=V, W), (V = '$VAR'(K), var_property(V, name(K))))*/.

funcdef --> id(FName), (ws, "(", !, ws, args(expr, FArgs), ws, ")" | [], { FArgs = [] }), ws, "=", ws, expr(Ret), ws, { append([FArgs, [Ret]], FArgs_), init_stvars(FArgs_) }, (clbody(G_), ws | [], { G_ = true }), ".", {
    clean_stvars(FArgs_, FArgs___),
    destructure_head(FArgs___, FArgs__, Y),
    (Y == "" -> G = G_ ; format(atom(G), '~w, ~w', [Y, G_])),

    format(atom(A), '~w>>(~w)', [FArgs__, G]),
    %print(funcdef0-A),nl,
    atom_to_term(A, Call, _),
    %print(funcdef1-Call),nl,
    length(FArgs, FArity),
    mlib:add_user_func(FName/FArity, Call),

    vv_format("defined function: ~s/~d~n", [FName, FArity])
}.

dcg --> id(DName), (ws, "(", !, ws, args(expr, DArgs), ws, ")" | [], { DArgs = [] }), ws, "-->", ws, goal_or(_G), {
    writeln(DName-DArgs),
    G = "apply_known_pred(_:'strip', [_S0, _S1]), apply_known_pred(_:'form_words', [_S1, _S2]), apply_known_pred(_:'words_to_strings', [_S2, _S])",
    (DArgs == [] -> format(atom(A), '[_S0, _S]>>(~w)', [G]) ;
        atomics_to_string(DArgs, ", ", DArgs_),
        format(atom(A), '[~w, _S0, _S]>>(~w)', [DArgs_, G])
    ),
    
    print(dcg-A),nl,
    atom_to_term(A, Call, _),
    length(DArgs, DArity_), DArity is DArity_ + 2,
    mlib:add_user_pred(DName/DArity, Call),

    vv_format("defined dcg clause: ~s/~d~n", [DName, DArity])
}.

gram --> ws, ((funcdef | cl | dcg/*| comment*/), !, gram | [],!).
gram --> ws, lazy_list_location(file(Name, Line, LinePos, CharNo)), {
        format(string(S), "error while parsing '~w' at line ~d:~d with '~c'(~d)~n", [Name, Line, LinePos, CharNo, CharNo]), throw(S)
    }.

load_file(File) :-
    mlib:reset,
    mlib:import_module_preds('_'),
    mlib:import_module_funcs('_'),
    mlib:import_module_types('_'),
    %mlib:import_module_modes('_'),
    %mlib:import_module_dets('_'),
    %mlib:import_module_pred_types('_'),

    phrase_from_file(gram, File).


/*list_auxW(T) --> ",", ws, expr(T_), !, list_auxW(T__), { format(atom(T), ",~w~w", [T_, T__]) } | [], { T = [] }.
listW(T) --> "[", ws, "]", { format(atom(T), "[]", []) }
    | "[", ws, expr(T_), ws, list_auxW(T__), ws, "]", { format(atom(T), "[~w~w]", [T_, T__]) }
    | "[", ws, expr(T_), ws, list_auxW(T__), ws, "|", ws, expr(T___), ws, "]", { format(atom(T), "[~w~w|~w]", [T_, T__, T___]) }.*/

%string(S0), ws, "++", !, ws, string(S1), { format(atom(E), '"~s~s"', [S0, S1]) }
    %(var(V), { format(atom(E0), "~p", [V]) } | string(S), { format(atom(E0), '"~s"', [S]) }), ws, "++", !, ws, expr_string(E1), { format(atom(E), "string:'++'(~w, ~w)", [E0, E1]) }
    %| string(S), { format(atom(E), '"~s"', [S]) }.
/*
expr(E) --> ("0", { E = '0' } | "1", { E = '1' } | "fib(N - 1)", { E = 'fib(N - 1)' } | "fib(N - 2)", { E = 'fib(N - 2)' }
    | num(N), { format(atom(E), "~p", [N]) } | list(L), { format(atom(E), "~p", [L]) } | var(V), { format(atom(E), "~p", [V]) }
    | string(S), { format(atom(E), '"~s"', [S]) } | expr_if(E) | ftor(F), { format(atom(E), "~p", [F]) }
    %| term(T), { format(atom(E), "~p", [T]) }
    | "(", !, ws, expr(E0), ws, ")", { format(atom(E), "(~w)", [E0]) }).
expr(E) --> expr(E0), ws, expr_binop(Op), !, {writeln(Op)}, ws, expr(E1), { Op = Mod:F -> format(atom(E), "~p:'~p'(~w, ~w)", [Mod, F, E0, E1]), print(E) ; format(atom(E), "'~p'(~w, ~w)", [Op, E0, E1]) }.
expr_binop(E) -->
    "++", { E = 'string':'++' }
    | "+", { E = '+' } | "-", { E = '-' } | "*", { E = '*' } | "//", { E = '//' } | "rem", { E = 'rem' }.
*/
/*funcdef --> id(FName), (ws, "(", ws, args(term, FArgs), ws, ")" | [], { FArgs = [] }), ws, "=", ws, expr(E), ws, ".", {
    length(FArgs, FArity),
    
    append([FArgs, [Ret]], FArgs_),
    format(atom(A), '~w>>(~w = ~w)', [FArgs_, Ret, E]),
    %print(A),nl,
    atom_to_term(A, Call, _),
    mlib:add_user_func(FName/FArity, Call),

    vv_writef("defined function: ~s/~d~n", [FName, FArity])
}.
funcdef --> id(FName), (ws, "(", ws, args(term, FArgs), ws, ")" | [], { FArgs = [] }), ws, "=", ws, var(Ret), ws, clbody(G), ws, "." {
    length(FArgs, FArity),
    %print(Ret),nl,
    append([FArgs, [Ret]], FArgs_),
    format(atom(A), '~w>>(~w)', [FArgs_, G]),
    %print(A),nl,
    atom_to_term(A, Call, _),
    mlib:add_user_func(FName/FArity, Call),

    vv_writef("defined function: ~s/~d~n", [FName, FArity])
}.*/

/*expr(E) --> expr(E0), ws, "++", !, ws, expr(E1), { format(atom(E), "string:'++'(~w, ~w)", [E0, E1]) }.
expr(E) --> expr(E0), ws, "+", !, ws, expr(E1), { format(atom(E), "'+'(~w, ~w)", [E0, E1]) }.
expr(E) --> expr(E0), ws, "-", !, ws, expr(E1), { format(atom(E), "~w - ~w", [E0, E1]) }.
expr(E) --> expr(E0), ws, "*", !, ws, expr(E1), { format(atom(E), "~w + ~w", [E0, E1]) }.
expr(E) --> expr(E0), ws, "//", !, ws, expr(E1), { format(atom(E), "~w // ~w", [E0, E1]) }.
expr(E) --> expr(E0), ws, "rem", !, ws, expr(E1), { format(atom(E), "~w rem ~w", [E0, E1]) }.*/
/*goal_call --> id(Mod), ".", id(PName), !, ws, "(", ws, args(term, PArgs), ws, ")", {
    split_stvars(PArgs, PArgs_),
    length(PArgs_, PArity),
    (known_preds(Mod, PName/PArity, Goal) -> apply(Goal, PArgs_) ;
        format(string(S), "missing predicate ~s:~s/~d!", [Mod, PName, PArity]), throw(S)
    )
}.
goal_if --> "(", ws, "if", ws, cond, ws, "then", ws, "X = 1", ws, "else", ws, "fib(N - 1, A), fib(N - 2, B), X = A + B", ws, ")", { }.
goal_fib --> "fib(10, X)".
goal(Goal) --> goal_call | goal_if | goal_fib.*/

/*goal_xfib(G) --> "fib(N - 1, A)", { format(atom(G), 'fib(N - 1, A)', []) }
    | "fib(N - 2, B)", { format(atom(G), 'fib(N - 2, B)', []) }.*/
%goal_infix_eq0(G) --> var(V), ws, "=", ws, "0", { format(atom(G), '~p =:= 0', [V]) }.
%goal_infix_eq1(G) --> var(V), ws, "=", ws, "1", { format(atom(G), '~p =:= 1', [V]) }.

/*goal2(G) --> goal(GH), ws, clbody_aux(G1),
    | [], NG = G.*/
%goal(Goal) --> "c", { Goal = 'writeln(\"Hello\")' }.

%cond --> "N =< 2" | "N = 0" | "N = 1".
%if --> "if", ws, cond, ws, "then", ws, "X = 1", ws, "else", ws, "X = fib(N - 1) + fib(N - 2)".
/*funcdef2 --> id(Func), ws, "(", ws, args(term, _Args), ws, ")", ws, "=", ws, "X", ws, ":-", ws, "(", ws, if, ws, ")", ws, ".", {
    format("defined function: ~s~n", [Func])
}.*/
/*funcdef2 --> id(Func), ws, "(", ws, args(term, _Args), ws, ")", ws, "=", ws, var(_Ret), ws, clbody(_G), ws, ".", {
    format("defined function: ~s~n", [Func])
}.*/

%enum_alts_aux(Cat, Enum) --> ";", !, ws, term(EnumH), ws, enum_alts_aux(Cat, EnumT), { Enum = [EnumH | EnumT] } | [], { Enum = [] }.
%enum_alts(Cat, Enum) --> call(term, EnumH), ws, enum_alts_aux(Cat, EnumT), { Enum = [EnumH | EnumT] }.

/*args(Cat, L) --> call(Cat, A), { atom(A) -> atom_chars(A, ['!' | B_]) ; fail }, !, {
        format(atom(Idx), '~s.count', [B_]),
        (nb_current(Idx, I) ->
            format(atom(B0), '_~s~d', [B_, I]), format(atom(B1), '_~s~d', [B_, I+1]), nb_setval(Idx, I+1) ;
            format(atom(B0), '_~s0', [B_]), format(atom(B1), '_~s', [B_]), nb_setval(Idx, 0)
        )
    }, ws, (",", !, ws, args(Cat, L_) | [], { L_ = [] }), { L = [B0, B1 | L_] }.*/

    %goal_a --> "io.write_string", !, ws, "(", ws, string(S), ws, ",", ws, "!IO", ws, ")", { mformat(S, []) }.
%goal_a2 --> "io.write_string", !, ws, "(", ws, string(S), ws, ",", ws, "IOState_in", ws, ",", ws, "IOState_out", ws, ")", { mformat(S, []) }.
%goal_b --> "io.write_int", !, ws, "(", ws, "fib(10)", ws, ",", ws, "!IO", ws, ")", { format("~d", [55]) }.
%goal_c --> "io.nl", !, ws, "(", ws, "!IO", ws, ")", { nl }.
%goal_d --> "io.format", !, ws, "(", ws, string(S), ws, ",", ws, ("[i(fib(10))]" | "[i(X)]"), ws, ",", ws, "!IO", ws, ")", { mformat(S, [55]) }.