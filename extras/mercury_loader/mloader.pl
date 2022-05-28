
:- use_module('../../toolbox/print_tools').
:- use_module('../../lpmux/mercury/mparser').

main :-
    writeln("-----------------------------"),
    writeln("Welcome to the mercury loader"),
    writeln("-----------------------------"),

    print_tools:assert_very_verbose,

    current_prolog_flag(argv, Args),
    (Args == [] ->
        throw("You must specify a file to load!")
    ;
        [FilePath | _] = Args
    ),

    mparser:load_file(FilePath),
    format('~`+t~`:|'), nl,
    %mlib:call_main,
    mlib:(known_preds(_, 'main'/_, Goal) -> apply(Goal, [_IO0, _IO]) ;
        read(Goal), Goal =.. [PName | PArgs], length(PArgs, PArity),
        known_preds(_, PName/PArity, Goal_), apply(Goal_, PArgs)
    ),
    halt.
