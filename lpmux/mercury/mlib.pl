
:- module(mlib, [
    import_module_preds/1, import_module_funcs/1,

    known_preds/3, known_funcs/3, known_dets/1, known_modes/2, known_types/2, known_pred_types/1, known_func_types/1,

    add_user_pred/2, add_user_func/2,

    apply_known_pred/2,
    call_main/0,

    get_clause_as_list/2,

    mlib_to_std_types/1, mlib_to_std_predtypes/1,

    reset/0
]).

replace_substrings_aux(String, [], String).
replace_substrings_aux(String, [SubString -> NewSubString | T], NewString) :-
    (append([Front, SubString, Back], String) ->
        append([Front, NewSubString, Back], Result),
        replace_substrings_aux(Result, [SubString -> NewSubString | T], NewString_)
        ; NewString_ = String
    ), replace_substrings_aux(NewString_, T, NewString).

replace_substrings(String, Rep, NewString) :-
    string_codes(String, Codes),
    convlist([A->B, C->D]>>(string_codes(A,C), string_codes(B,D)), Rep, Rep_),
    replace_substrings_aux(Codes, Rep_, NewCodes),
    string_codes(NewString, NewCodes).

mformat(S, L) :-
    %print(S),nl,print(L),nl,
    replace_substrings(S, ["\\n" -> "~n", "%d" -> "~d", "%c" -> "~c", "%s" -> "~s"], S_),
    convlist([I,O]>>(I =.. [F | Args], (
        F == 'i' -> [Arg0] = Args, eval_expr(Arg0, O) ;
        O = I
    )), L, L_),
    format(S_, L_).

eval_expr(E, E) :- string(E), !.
eval_expr(E, E) :- is_list(E), !.
eval_expr(E, V) :- on_exception(_Exception, V is E, fail), !.
%eval_expr(E, V) :- var('$VAR'(E)), V = '$VAR'(E), !.
%eval_expr(E, V) :- number(E), !, V = E.
%eval_expr(E, V) :- E = 'X', !, V = 155. % use nb_getval!
eval_expr(E, V) :- (Mod:F = E -> true ; F = E), nonvar(F), !,
    F =.. [FName | FArgs], length(FArgs, FArity),
    (known_funcs(Mod, FName/FArity, Call) *->
        append([FArgs, [R]], FArgs_), /*writeln(uu-FName-Call),*/ apply(Call, FArgs_), (catch(eval_expr(R, V), _Ex, V = R) -> true ; V = R) ;
        atom(E), !, V = E%writeln(E),!,V = E
    ).
%eval_expr(E, E) :- atom(E), !.
eval_expr(E, _) :- format(string(S), "'~p' is not an evaluable expression!", [E]), throw(S).


%builtin_funcs('_', expr_if/3, [Cond, E0, E1, O]>>(Cond -> eval_expr(E0, O) ; eval_expr(E1, O))).
%builtin_funcs('_', apply/1, [F, R]>>(F = FArgs>>_, append([FArgs, [R]], FArgs_), writeln(apply(F, FArgs_)), apply(F, FArgs_), true)).
%builtin_funcs('_', apply/1, [F, R]>>(eval_expr(F,R), writeln(R))).
builtin_funcs('_', apply/1, [F, R]>>(F = FArgs>>_, apply(F, FArgs), append([_FArgs, [R]], FArgs))).
builtin_funcs('_', expr_if/3, [Cond, E0, E1, O]>>(Cond -> O = E0 ; O = E1)).
builtin_funcs('_', '+'/2, [A, B, C]>>(eval_expr(A, A_), eval_expr(B, B_), C = A_ + B_)).
builtin_funcs('_', '-'/2, [A, B, C]>>(eval_expr(A, A_), eval_expr(B, B_), number(A), number(B), C = A_ - B_)).
builtin_funcs('_', '*'/2, [A, B, C]>>(eval_expr(A, A_), eval_expr(B, B_), C = A_ * B_)).
builtin_funcs('_', '//'/2, [A, B, C]>>(eval_expr(A, A_), eval_expr(B, B_), C = A_ // B_)).
builtin_funcs('_', 'rem'/2, [A, B, C]>>(eval_expr(A, A_), eval_expr(B, B_), C = A_ rem B_)).
builtin_funcs('string', format/2, [Fmt, Args, O]>>(with_output_to(string(O), mformat(Fmt, Args)))).
builtin_funcs('string', '++'/2, [S0, S1, O]>>(eval_expr(S0, S0_), eval_expr(S1, S1_), string_concat(S0_, S1_, O))).
builtin_funcs('char', 'det_int_to_decimal_digit'/1, [I, O]>>(O is I + 48)).
%builtin_funcs('term', functor/3, [Name, Args, Ctx, R]>>(R = functor(Name, Args, Ctx))).
%builtin_funcs('term', atom/1, [A, R]>>(R = atom(A))).

:- dynamic known_funcs/3.
known_funcs('_', Func, Call) :- builtin_funcs('_', Func, Call),!. % useless: killed by retractall
%known_funcs('', fib/1, [I,O]>>(I = 10, O = 55)).
%known_funcs('', beer_stanza/1, [I,O]>>((I > 0 -> format(string(O), "~d", [I]) ; O = "M!M"))).
%known_funcs('', beer_stanza/1, [I,O]>>((I is 0 -> O = "Go to the store and buy some more!\n" ; O = "Aleph!\n"))).

import_module_funcs(Mod) :- foreach(builtin_funcs(Mod, Func, Call), assert(known_funcs(Mod, Func, Call))).

add_user_func(FName/FArity, Call) :-
    (nb_current('module', Mod) -> true ; Mod = 'user'),
    assertz(known_funcs(Mod, FName/FArity, Call)).

builtin_preds('_', debug_print/1, [I]>>(print(I),nl)). % DEBUG ONLY!
builtin_preds('_', true/0, []>>true).
builtin_preds('_', '='/2, [A, B]>>on_exception(_Exception, A =:= B, (A = B))).
builtin_preds('io', write_string/3, [S, _IO0, _IO1]>>(eval_expr(S, S_), string(S_), mformat(S_, []))). % (string(S) -> S_ = S ; eval_expr(S, S_))
builtin_preds('io', write_int/3, [I, _IO0, _IO1]>>(eval_expr(I, I_), integer(I_), mformat("%d", [I_]))). %(integer(I) -> J = I ; eval_expr(I, J))
builtin_preds('io', write_char/3, [C, _IO0, _IO1]>>(eval_expr(C, C_), (number(C_) -> C__ = C_ ; char_code(C_, C__)), mformat("%c", [C__]))). %(integer(I) -> J = I ; eval_expr(I, J))
builtin_preds('io', nl/2, [_IO0, _IO]>>nl).
builtin_preds('io', format/4, [Fmt, Args, _IO0, _IO1]>>mformat(Fmt, Args)).
builtin_preds('io', command_line_arguments/3, [Args, _IO0, _IO1]>>current_prolog_flag(argv, [_ | Args])).
builtin_preds('io', open_input/4, [File, Result, _IO0, _IO1]>>(open(File, read, Stream) -> set_input(Stream), Result = ok(Stream) ; Result = error(123))). %?
builtin_preds('io', close_input/3, [Stream, _IO0, _IO1]>>close(Stream)). %?
builtin_preds('io', progname/4, [DefaultProgname, Progname, _IO0, _IO1]>>(current_prolog_flag(executable, Path), atom_string(Path, Progname) ; Progname = DefaultProgname)). %?
builtin_preds('io', error_message/2, [_Error, Message]>>(Message = "Error")). %?
builtin_preds('io', write_strings/3, [Strings, IO0, IO1]>>(builtin_preds('io', write_string/3, Call), forall(member(S, Strings), apply(Call, [S, IO0, IO1])))). %?
builtin_preds('io', set_input_stream/4, [Stream, OldStream, _IO0, _IO1]>>(current_input(OldStream), set_input(Stream))).
builtin_preds('io', read_line_as_string/3, [Result, _IO0, _IO1]>>((current_input(Stream), read_line_to_string(Stream, R)) -> (R = end_of_file -> Result = eof ; Result = ok(R)) ; Result = error(123))). %?
builtin_preds('io', input_stream_name/3, [StreamName, _IO0, _IO1]>>(current_input(Stream), stream_property(Stream, alias(StreamName)))). %?
builtin_preds('io', flush_output/2, [_IO0, _IO1]>>(current_output(Stream), flush_output(Stream))).
builtin_preds('io', input_stream/3, [Stream, _IO0, _IO1]>>current_input(Stream)).
builtin_preds('io', read_line/4, [Stream, Result, _IO0, _IO1]>>(read_line_to_codes(Stream, R) -> (R = end_of_file -> Result = eof ; Result = ok(R)) ; Result = error(123))).
builtin_preds('string', 'to_int'/2, [S, I]>>(eval_expr(S, S_), number_string(I, S_), integer(I))).
builtin_preds('string', 'int_to_base_string'/3, [I, B, S]>>(eval_expr(I, I_), integer(I_), eval_expr(B, B_), integer(B_), format(string(B__), "~~~dr", [B_]), format(string(S), B__, [I_]))).
builtin_preds('string', 'length'/2, [S, Len]>>string_length(S, Len)).
builtin_preds('string', 'from_char_list'/2, [Chars, String]>>string_chars(String, Chars)).
builtin_preds('string', 'to_upper'/2, [String, UpperCase]>>string_upper(String, UpperCase)).
builtin_preds('char', 'is_whitespace'/1, [Char]>>char_type(Char, space)).
builtin_preds('list', 'reverse'/2, [List1, List2]>>reverse(List1, List2)).
builtin_preds('list', 'append'/3, [List1, List2, List1AndList2]>>append(List1, List2, List1AndList2)).
builtin_preds('assoc_list', reverse_members/2, [AL1, AL2]>>convlist([K-V,V-K]>>true, AL1, AL2)).
builtin_preds('map', search/3, [Map, Key, Value]>>member(Key-Value, Map)). %get_dict(Key, Map, Value)
builtin_preds('map', from_assoc_list/2, [AL, Map]>>(Map = AL)). %dict_create(Map, _, AL)
builtin_preds('term_io', read_term/3, [Term, _IO0, _IO1]>>(read_term(Term_, [variable_names(VarSet)]) -> (Term_ == end_of_file -> Term = eof ; Ctx = context("CTX", 123), (var(Term_) -> Term__ = variable(Term_, Ctx) ; Term_ =.. [TN | TA], Term__ = functor(TN, TA, Ctx)), Term = term(VarSet, Term__)) ; Term = error("ERROR", 123))).
builtin_preds('term_io', write_term/4, [VarSet, Term, _IO0, _IO1]>>((Term == eof -> Term_ = end_of_file ; (Term = functor(TN, TA, Ctx) -> Term_ =.. [TN | TA] ; Term = variable(Term_, Ctx))), write_term(Term_, [variable_names(VarSet)]))).
builtin_preds('io', set_exit_status/3, [Status, _IO0, _IO1]>>(set_prolog_flag(exit_status, Status))).
builtin_preds('string', format/3, [Fmt, Args, O]>>(with_output_to(string(O), mformat(Fmt, Args)))).


:- dynamic known_preds/3.
known_preds('_', Pred, Goal) :- builtin_preds('_', Pred, Goal). % useless: killed by retractall

import_module_preds(Mod) :- foreach(builtin_preds(Mod, Pred, Call), assert(known_preds(Mod, Pred, Call))).

add_user_pred(PName/PArity, Goal) :-
    (nb_current('module', Mod) -> true ; Mod = 'user'),
    assertz(known_preds(Mod, PName/PArity, Goal)).

apply_known_pred(Mod:Pred, Args) :- length(Args, Arity),
    %(known_preds(Mod, Pred/Arity, Goal) -> apply(Goal, Args) ; fail).
    %print(a-Args),nl,
    convlist([I,O]>>(catch(eval_expr(I, O), _Exception, O = I) -> true ; O = I), Args, Args_),
    %print(b-Args_),nl,
    (known_preds(Mod, Pred/Arity, Goal) *-> /*writeln(yue-Pred/Arity-Args),*/ apply(Goal, Args_)/*, writeln(yuf-Args)*/ ;
        format(string(S), "The predicate ~w:~w/~d does not exist!", [Mod, Pred, Arity]), throw(S)
    ).
/*apply_known_pred(Mod:Pred, Args) :-
    atomics_to_string(Args, ", ", ArgsS),
    format(string(S), "Unable to call ~w:~w(~w)", [Mod, Pred, ArgsS]).*/

call_main :- (known_preds(_, 'main'/_, Goal) -> apply(Goal, [_IO0, _IO]) ; fail).


known_dets('det').
known_dets('semidet').
known_dets('multi').
known_dets('nondet').
known_dets('failure').
known_dets('erroneous').
known_dets('cc_multi').
known_dets('cc_nondet').


builtin_modes('in').
builtin_modes('out').

:- dynamic known_modes/2.
known_modes(M, _) :- builtin_modes(M). % useless: killed by retractall
known_modes('di', _).
known_modes('uo', _).


builtin_types('io', _).
builtin_types('io':'input_stream', _).
builtin_types('int', _).
builtin_types('string', _).
builtin_types(list(_), _).

:- dynamic known_types/2.

import_module_types(Mod) :- foreach((builtin_types(Type, Enum), (Type == Mod ; Type == Mod:_T)), assert(known_types(Type, Enum))).


zared(A, Gs, A_) :-
    foldl([QS, DF, GH, JK]>>(
        (   nonvar(QS), functor(QS, FName, FArity), known_funcs(_, FName/FArity, FArgs_>>_), !,
            QS =.. [FName | FArgs],
            zared(FArgs, KGX, FArgs__),
            append(FArgs__, [DF], FArgs_),
            QS_ =.. [FName | FArgs_],
            append([GH, KGX, [QS_]], JK)
        ;
            zarof(QS, LM, DF),
            append(GH, LM, JK)
        )
    ), A, A_, [], Gs).

zarof(W, Gs, G) :- % temporary patch!
    (   var(W), !,
        Gs = [], G = W
    ;   (W = apply_known_pred(_:P, A), ! ; W =.. [P | A]), !,
        zared(A, Gs, A_),
        G =.. [P | A_]
    ).

get_clause_as_list(Head, ListBody) :-
    (var(Head) -> true ; functor(Head, PName, PArity)),
    (known_preds(_, PName/PArity, PArgs>>Body) ; known_funcs(_, PName/FArity, PArgs>>Body), PArity is FArity + 1),
    Head =.. [PName | PArgs],
    convert_to_list(Body, ListBody_),
    maplist([W, Gs]>>(zarof(W, Gs_, G), append(Gs_, [G], Gs)), ListBody_, ListBody__), append(ListBody__, ListBody).

get_clause(Head, Body) :- 
    get_clause_as_list(Head, B), convert_to_list(Body, B).

convert_to_list((A, B), [A | BRest]) :- !, convert_to_list(B, BRest).
convert_to_list(true, []).
convert_to_list(A, [A]) :- A \= true.

:- dynamic known_pred_types/1.
:- dynamic known_func_types/1.

mlib_to_std_types(StdTypes) :-
    findall(T_-E_, (
        mlib:known_types(T, E),
        (T == int -> T_ = integer ; T_ = T),
        (var(E) -> E_ = E ; convlist([I,O]>>atom_to_term(I,O,_), E, E_))), Types_),
    /*partition([T-E]>>var(E), Types_, BuiltinTypes_, Types),
    convlist([T-E, T_]>>(T == int -> T_ = integer ; T_ = T), BuiltinTypes_, BuiltinTypes),
    append([BuiltinTypes, Types], StdTypes)*/
    StdTypes = Types_.

mlib_to_std_predtypes(StdPredTypes) :-
    findall(Pred_, (mlib:known_pred_types(Pred), re_replace("\\bint\\b"/ga, "integer", Pred, Pred__), atom_to_term(Pred__, Pred_, _)), PredTypes),
    findall(Func_, (mlib:known_func_types(Func), re_replace("\\bint\\b"/ga, "integer", Func, Func__), atom_to_term(Func__, Func_, _)), FuncTypes),
    append(PredTypes, FuncTypes, StdPredTypes).


reset :-
    retractall(known_preds(_,_,_)),
    retractall(known_funcs(_,_,_)),
    %retractall(known_dets(_)),
    retractall(known_modes(_,_)),
    retractall(known_types(_,_)),
    retractall(known_pred_types(_)),
    retractall(known_func_types(_)).