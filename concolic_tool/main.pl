
:- use_module('../toolbox/print_tools').
:- use_module('../toolbox/cli_options').
:- use_module(concolic_tester).
:- use_module(samples).

recognized_option(['-v'], verbose).
recognized_option(['-verbose'], verbose).
recognized_option(['-vv'], very_verbose).
recognized_option(['-very_verbose'], very_verbose).
recognized_option(['-cg', term(Q)], cg(Q)).
recognized_option(['-sg', term(Q)], sg(Q)).
recognized_option(['-depth', integer(Depth)], depth(Depth)).
recognized_option(['-timeout', integer(Timeout)], timeout(Timeout)).
recognized_option(['-trace'], with_trace).
recognized_option(['-ground', term(G)], ground(G)).
recognized_option(['-interactive'], interactive).
recognized_option(['-file', atom(NT)], file(NT)).
recognized_option(['-help'], help).


print_help. % TODO!

:- dynamic interactive/0. %% used to decide which clause of write_form to use

main :-
    prolog_flag(argv, ArgV),
    cli_options:get_options(ArgV, recognized_option, Options, _RemArgV), !,
    cli_options:match_options(Options, [
        verbose: assert_verbose,
        very_verbose: assert_very_verbose,
        cg(ConcreteGoal),
        sg(_SymbolicGoal), % useless ???
        depth(Depth),
        timeout(Timeout),
        with_trace: WithTrace = true,
        ground(Ground),
        interactive: assertz(interactive),
        file(File),
        help: print_help
    ]),
    (WithTrace = true -> true ; WithTrace = false), !,

    (ground([ConcreteGoal, Ground, Depth, Timeout, WithTrace, File]) -> true ; print_help, fail),

    v_format("ConcreteGoal = ~p~n", [ConcreteGoal]),
    v_format("Ground       = ~p~n", [Ground]),
    v_format("Depth        = ~p~n", [Depth]),
    v_format("Timeout      = ~p~n", [Timeout]),
    v_format("WithTrace    = ~p~n", [WithTrace]),
    v_format("File         = ~p~n", [File]),

    concolic_helper:main(ConcreteGoal, Ground, Depth, Timeout, WithTrace, File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some benchmarks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(load_samples).