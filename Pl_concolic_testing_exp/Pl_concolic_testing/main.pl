:- style_check(-singleton).
:- use_module(type_check).
:- style_check(+singleton).

:- ensure_loaded(prolog_reader).
:- use_module(print_tools).
:- use_module(concolic_algo).
:- use_module(samples).


:- type flag_t --->
    testcase(any, list(any)) ; pred(any, integer) ; fun(any, integer) ; verbose ; very_verbose ;
    cg(any) ; sg(any) ; depth(any) ; timeout(any) ; with_trace ;
    ground(any) ; interactive ; help ; file(any).

:- pred main.
main :-
    prolog_flag(argv, ArgV),
    get_options(ArgV, Options, RemArgV), !, (
        member(verbose, Options)::member(flag_t, list(flag_t)) -> (
            assert_verbose, print(Options)::print(list(flag_t)), nl
        ) ; (
            member(very_verbose, Options)::member(flag_t, list(flag_t)) -> (
                assert_verbose, assert_very_verbose,
                print(Options)::print(list(flag_t)), nl
            ) ; true
        )
    ), (
        (
            member(cg(CG), Options)::member(flag_t, list(flag_t)),
            convert_entry_to_term(CG, CGT),
            assertz(cli_initial_cg(CGT)), fail
        ) ; true
    ), (
        (
            member(sg(SG), Options)::member(flag_t, list(flag_t)),
            convert_entry_to_term(SG, SGT),
            assertz(cli_initial_sg(SGT)), fail
        ) ; true
    ), (
        (
            member(depth(K), Options)::member(flag_t, list(flag_t)),
            convert_entry_to_term(K, KT),
            assertz(cli_initial_depth(KT)), fail
        ) ; true
    ), (
        (
            member(timeout(K), Options)::member(flag_t, list(flag_t)),
            convert_entry_to_term(K, KT),
            assertz(cli_initial_timeout(KT)), fail
        ) ; true
    ), (
        member(with_trace, Options)::member(flag_t, list(flag_t)) -> assertz(cli_initial_trace) ; true
    ), (
        (
            member(ground(Ground), Options)::member(flag_t, list(flag_t)),
            convert_entry_to_term(Ground, GroundT),
            assertz(cli_initial_ground(GroundT)), fail
        ) ; true
    ), (
        member(interactive, Options)::member(flag_t, list(flag_t)) -> assertz(interactive) ; true
    ), (
        (
            member(help, Options)::member(flag_t, list(flag_t)) ; RemArgV = []
        ) -> print_help ; true
    ), (
        (
            member(file(FILE), Options)::member(flag_t, list(flag_t)),
            assertz(cli_initial_file(FILE)), fail
        ) ; true
    ), main_cli.

% JE: wrong signature!!!
%:- pred main(any, list(integer), integer, integer, boolean, any).
main(Q, NR, K, T, Trace, File) :-
    retractall(with_trace), (
        Trace -> assertz(with_trace) ; true
    ),
    catch(call_with_time_limit(T, mainT(Q, NR, K, File)), X, error_process(X)).

:- pred error_process(any).
error_process(time_limit_exceeded) :-
    write('Timeout exceeded'), nl, print_test_cases, halt.
error_process(X) :- write('Unknown Error' : X), nl, halt.


:- pred get_options(list(any), list(flag_t), list(any)).
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

:- pred recognise_option(list(any), flag_t, list(any)).
recognise_option(Inputs, Flag, RemInputs) :-
    recognised_option(Heads, Flag),
    append(Heads, RemInputs, Inputs)::append(list(any), list(any), list(any)).

:- pred recognised_option(list(any), flag_t).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some benchmarks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(load_samples).