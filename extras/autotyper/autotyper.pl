
:- use_module('../../lpmux/prolog/pl_newtyper').

/** Display **/

display_types_aux([EnumH], Enum_) :- !, format(string(Enum_), "~p", [EnumH]).
display_types_aux([EnumH | EnumT], Enum_) :- display_types_aux(EnumT, Enum__), format(string(Enum_), "~p ; ~w", [EnumH, Enum__]).
display_types([]).
display_types([Type-[] | Types]) :- !, format(":- type ~w.~n", [Type]), display_types(Types).
display_types([Type-Enum | Types]) :- display_types_aux(Enum, Enum_), format(":- type ~w ---> ~s.~n", [Type, Enum_]), display_types(Types).
display_preds([]).
display_preds([Pred | Preds]) :- format(":- pred ~w.~n", [Pred]), display_preds(Preds).
display :-
    findall(Type-Enum, pl_newtyper:types(Type, Enum), Types), display_types(Types),
    findall(Pred, pl_newtyper:preds(Pred), Preds), display_preds(Preds).

/** Main program **/

main :-
    writeln("------------------------------"),
    writeln("Welcome to the automatic typer"),
    writeln("------------------------------"),

    current_prolog_flag(argv, Args),
    (Args == [] ->
        throw("You must specify a file to load!")
    ;
        [FilePath | _] = Args
    ),
    format("Loading file '~s'~n", [FilePath]), nl,

    pl_newtyper:proceed(FilePath), nl,
    format('~`+t~`:|'), nl,
    display,

    halt.