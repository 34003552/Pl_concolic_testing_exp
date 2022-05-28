
:- module(cli_options, [get_options/4, match_options/2]).

arg_to_term(Arg, ArgT) :-
    catch((
        ArgT = atom(Term), !, Term = Arg ;
        ArgT = integer(Term), !, atom_number(Arg, Term), integer(Term) ;
        ArgT = term(Term), !, atom_to_term(Arg, Term, _)
    ), _, fail) -> true ;
    functor(ArgT, DstType, _),
    format(string(S), "Command-line argument '~w' cannot be read as ~w!", [Arg, DstType]),
    print_message(error, S).

get_options([], _, [], []) :- !.
get_options(Inputs, OptRecognizer, [RecOption | RecOptions], RemInputs_) :-
    append([Option | Args], RemInputs, Inputs),
    call(OptRecognizer, [Option | ArgTs], RecOption),
    maplist(arg_to_term, Args, ArgTs), !,
    get_options(RemInputs, OptRecognizer, RecOptions, RemInputs_).
get_options([Input | RemInputs], OptRecognizer, RecognizedOptions, [Input | RemInputs_]) :-
    get_options(RemInputs, OptRecognizer, RecognizedOptions, RemInputs_).

match_options(_, []).
match_options(Options, [Option: Goal | Cases]) :- !,
    (member(Option, Options) ->
        (Goal =.. ['{}', Goal_] -> true ; Goal_ = Goal), call(Goal_)
    ; true),
    match_options(Options, Cases).
match_options(Options, [Option | Cases]) :-
    match_options(Options, [Option: true | Cases]).