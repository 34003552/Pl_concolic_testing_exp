
:- module(samples, [load_samples/0, sample/1, samples/0, test/0]).

:- use_module('../toolbox/gen_tools').

:- dynamic samples_loaded/0.
:- dynamic sample/2.

load_samples :- File = "samples.txt",
    (exists_file(File) -> parse_file_lines(File, sample_gram), assertz(samples_loaded) ; true).

sample_gram --> ['=', Name, ArgsList], { assertz(sample(Name, ArgsList)) }.

run_sample(Name, TimeLimit) :-
    (samples_loaded ->
        sample(Name, ArgsList_), 
        (ground(TimeLimit) ->
            nth0(3, ArgsList_, Delay, R),
            (Delay =< TimeLimit -> ArgsList = ArgsList_ ;
                nth0(3, ArgsList, TimeLimit, R)
            )
        ; ArgsList = ArgsList_),
        %format("Loading sample: ~s", [Name]), nl,
        once(apply(concolic_helper:main, ArgsList))
    ;
        throw("sample description file has not been loaded")
    ).

run_samples(TimeLimit) :-
    run_sample(_, TimeLimit), 
    format('~`=t~`:|'), nl,
    fail.
run_samples(_TimeLimit).

sample(Name) :- run_sample(Name, _).

samples :- run_samples(_).

test :- run_samples(10).