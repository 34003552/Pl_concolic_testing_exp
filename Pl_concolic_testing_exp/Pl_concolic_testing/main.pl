:- style_check(-singleton).
:- use_module('extras/type_check/type_check').
:- style_check(+singleton).

%use_type_checker.

:- include('extras/headers/h_main.pl').

:- ensure_loaded(prolog_reader).
:- use_module(print_tools).
:- use_module(concolic_algo).
:- use_module(samples).

:- dynamic filename/1.
:- dynamic depthk/1. %% max term depth

:- dynamic cli_option/1.
:- dynamic cli_initial_cg/1.
:- dynamic cli_initial_sg/1.
:- dynamic cli_initial_ground/1.
:- dynamic cli_initial_depth/1.
:- dynamic cli_initial_timeout/1.
:- dynamic cli_initial_trace/0.
:- dynamic cli_initial_file/1.

main :-
    prolog_flag(argv, ArgV),
    get_options(ArgV, Options, RemArgV), !, (
        member__flag(verbose, Options) -> (
            assert_verbose, print__list_flag(Options), nl
        ) ; (
            member__flag(very_verbose, Options) -> (
                assert_verbose, assert_very_verbose,
                print__list_flag(Options), nl
            ) ; true
        )
    ), (
        (
            member__flag(cg(CG), Options),
            convert_entry_to_term(CG, CGT),
            assertz(cli_initial_cg(CGT)), fail
        ) ; true
    ), (
        (
            member__flag(sg(SG), Options),
            convert_entry_to_term(SG, SGT),
            assertz(cli_initial_sg(SGT)), fail
        ) ; true
    ), (
        (
            member__flag(depth(K), Options),
            convert_entry_to_term(K, KT),
            assertz(cli_initial_depth(KT)), fail
        ) ; true
    ), (
        (
            member__flag(timeout(K), Options),
            convert_entry_to_term(K, KT),
            assertz(cli_initial_timeout(KT)), fail
        ) ; true
    ), (
        member__flag(with_trace, Options) -> assertz(cli_initial_trace) ; true
    ), (
        (
            member__flag(ground(Ground), Options),
            convert_entry_to_term(Ground, GroundT),
            assertz(cli_initial_ground(GroundT)), fail
        ) ; true
    ), (
        member__flag(interactive, Options) -> assertz(interactive) ; true
    ), (
        (
            member__flag(help, Options) ; RemArgV = []
        ) -> print_help ; true
    ), (
        (
            member__flag(file(FILE), Options),
            assertz(cli_initial_file(FILE)), fail
        ) ; true
    ), main_cli.

:- dynamic interactive/0. %% used to decide which clause of write_form to use

main_cli :-
    cli_initial_cg(Q),
    cli_initial_ground(NR),
    cli_initial_depth(K),
    cli_initial_timeout(T),
    cli_initial_file(File),!, (
        cli_initial_trace ->
            main(Q, NR, K, T, true, File) ;
            main(Q, NR, K, T, false, File)
    ).

:- dynamic with_trace/0.

main(Q, NR, K, T, Trace, File) :-
    retractall(with_trace), (
        Trace -> assertz(with_trace) ; true
    ),
    catch(call_with_time_limit(T, mainT(Q, NR, K, File)), X, error_process(X)).

error_process(time_limit_exceeded) :-
    write('Timeout exceeded'), nl, print_test_cases, halt.
error_process(X) :- write('Unknown Error' : X), nl, halt.

%print_test_cases/0

get_options([], Rec, Rem) :- !, Rec = [], Rem = [].
get_options(Inputs, RecognisedOptions, RemOptions) :- (
    recognise_option(Inputs, Flag, RemInputs) -> (
        RecognisedOptions = [Flag | RecO2],
        assertz(cli_option(Flag)),
        RemO2 = RemOptions
    ) ; (
        Inputs = [H | RemInputs],
        RemOptions = [H | RemO2],
        RecO2 = RecognisedOptions)
    ),
    get_options(RemInputs, RecO2, RemO2).

recognise_option(Inputs, Flag, RemInputs) :-
    recognised_option(Heads, Flag),
    append__any(Heads, RemInputs, Inputs).

recognised_option(['-cg', Q], cg(Q)).
recognised_option(['-sg', Q], sg(Q)).
recognised_option(['-ground', G], ground(G)).
recognised_option(['-depth', Depth], depth(Depth)).
recognised_option(['-timeout', Depth], timeout(Depth)).
recognised_option(['-trace'], with_trace).
recognised_option(['-interactive'], interactive).
recognised_option(['-file', NT], file(NT)).
recognised_option(['-v'], verbose).
recognised_option(['-verbose'], verbose).
recognised_option(['-vv'], very_verbose).
recognised_option(['-very_verbose'], very_verbose).
recognised_option(['-help'], help).

convert_entry_to_term(CLIGOAL, Term) :-
    on_exception(Exception, (
            atom_codes(CLIGOAL, Codes),
            read_from_chars(Codes, Term)
        ), (
            nl, print('### Illegal Command-Line Goal: "'),
            print(CLIGOAL), print('"'), nl,
            format("### Use following format: \"Goal.\"~n",[]),
            print('### Exception: '), print(Exception), nl,
            halt
        )
    ).

print_help.

assert_interactive :- assertz(interactive).





%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some benchmarks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(load_samples).