
:- use_module(print_tools).
:- use_module(concolic_algo).
:- use_module(samples).

recognized_option(['-cg', Q], cg(Q)).
recognized_option(['-sg', Q], sg(Q)).
recognized_option(['-ground', G], ground(G)).
recognized_option(['-depth', Depth], depth(Depth)).
recognized_option(['-timeout', Delay], timeout(Delay)).
recognized_option(['-trace'], with_trace).
recognized_option(['-interactive'], interactive).
recognized_option(['-file', NT], file(NT)).
recognized_option(['-v'], verbose).
recognized_option(['-verbose'], verbose).
recognized_option(['-vv'], very_verbose).
recognized_option(['-very_verbose'], very_verbose).
recognized_option(['-help'], help).

recognize_option(Inputs, RecognizedOption, RemainingInputs) :-
    recognized_option(Heads, RecognizedOption),
    append(Heads, RemainingInputs, Inputs).

get_options([], RecognizedOptions, RemainingInputs) :- !,
    RecognizedOptions = [], RemainingInputs = [].
get_options(Inputs, RecognizedOptions, RemainingInputs) :- (
        recognize_option(Inputs, RecOption, RemInputs) -> (
            RecognizedOptions = [RecOption | RecOptions],
            RemainingInputs = RemInputs_
        ) ; (
            RecognizedOptions = RecOptions,
            RemainingInputs = [Input | RemInputs_], [Input | RemInputs] = Inputs
        )
    ),
    get_options(RemInputs, RecOptions, RemInputs_).

reject_option(Option, Exception) :- 
    nl, printf("### Illegal command-line option: ~p", [Option]),
    nl, printf("### Exception: \"~w\"", [Exception]),
    nl, halt.

parse_options(_, []).
parse_options(Options, [Option: Goal | Cases]) :-
    (member(Option, Options) -> catch(Goal, Exception, reject_option(Option, Exception)) ; true),
    parse_options(Options, Cases).

argument_to_term(Argument, Term) :-
    on_exception(_Exception, atom_to_term(Argument, Term, _), (
            format(string(S), "~p is not a valid term", [Argument]), throw(S)
        )
    ).

print_help. % TODO!

:- dynamic interactive/0. %% used to decide which clause of write_form to use

main :-
    prolog_flag(argv, ArgV),
    get_options(ArgV, Options, _RemArgV), !, 
    parse_options(Options, [
        verbose:
        (
            assert_verbose
        ),
        very_verbose:
        (
            assert_very_verbose
        ),
        cg(ConcreteGoal_):
        (
            argument_to_term(ConcreteGoal_, ConcreteGoal)
        ),
        sg(SymbolicGoal_): % useless ???
        (
            argument_to_term(SymbolicGoal_, _SymbolicGoal)
        ),
        depth(Depth_):
        (
            argument_to_term(Depth_, Depth),
            (integer(Depth) -> true ; throw("depth expects an integer"))
        ),
        timeout(Timeout_):
        (
            argument_to_term(Timeout_, Timeout),
            (integer(Timeout) -> true ; throw("timeout expects an integer"))
        ),
        with_trace:
        (
            WithTrace = true
        ),
        ground(Ground_):
        (
            argument_to_term(Ground_, Ground)
        ),
        interactive:
        (
            assertz(interactive)
        ),
        help:
        (
            print_help
        ),
        file(File_):
        (
            File = File_
        )
    ]),
    (WithTrace = true -> true ; WithTrace = false), !,

    (ground([ConcreteGoal, Ground, Depth, Timeout, WithTrace, File]) -> true ; print_help, fail),

    v_printf("ConcreteGoal = '~w'~n", [ConcreteGoal]),
    v_printf("Ground       = '~w'~n", [Ground]),
    v_printf("Depth        = '~w'~n", [Depth]),
    v_printf("Timeout      = '~w'~n", [Timeout]),
    v_printf("WithTrace    = '~w'~n", [WithTrace]),
    v_printf("File         = '~w'~n", [File]),

    concolic_algo:main(ConcreteGoal, Ground, Depth, Timeout, WithTrace, File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some benchmarks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(load_samples).

test :- sample(_), fail.
test.

%az :- Term = f(X,Y,Z), numbervars(Term, 0, _End), write_canonical(Term).


%az :- b_setval('A', A), A = 2, b_getval('A', B), writeln(B).

/*azf_(I) :- term_to_atom('nil'-I, A2), nb_getval(A2, Z), writeln(Z).
azf :- (nb_current('nil.count', Count) -> true ; Count = 0),
    term_to_atom('nil'-Count, A), nb_setval(A, 7),
    foreach(between(0, Count, I), azf_(I)),
    NCount is Count + 1, nb_setval('nil.count', NCount).*/



%azf :- register_ctor('nil', Ref, "Vug"), term_to_atom(nil-Ref, A), nb_getval(A, V), format("~w@~w: ~w~n", [nil, Ref, V]).