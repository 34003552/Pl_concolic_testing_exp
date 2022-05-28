
/* Logic Programming Multiplexer */
:- module(lpmux, [
    load_program/1, unload_program/0,
    use_experimental_pltyper/0,

    current_filename/1,
    current_language/1,
    current_types/1, current_predtypes/1,

    get_clause_body_as_list/2,
    get_program_clause_body_as_list/2
]).

:- use_module('typed-prolog/tprolog').
:- use_module('prolog/pl_oldtyper').
:- use_module('prolog/pl_newtyper').
:- use_module('mercury/mlib', except([get_clause_as_list/2])).
:- use_module('mercury/mparser', except([load_file/1])).

:- dynamic use_experimental_pltyper/0.
%use_experimental_pltyper.

:- dynamic current_filename/1.

lpmux__init(File) :-
    %lpmux__clean,
    assertz(current_filename(File)),
    lpmux:pl_oldtyper__init.

lpmux__clean :-
    retractall(current_filename(_)),
    lpmux:pl_oldtyper__clean,
    retractall(current_language(_)),
    retractall(current_types(_)),
    retractall(current_predtypes(_)).

get_file_extension(File, Ext) :-
    atom_chars(File, FCs),
    append(_, ['.' | ECs], FCs), \+member('.', ECs), !,
    atom_chars(Ext, ECs).

is_type_check_loaded_from_file(File) :-
    absolute_file_name(File, File_),
    module_property('type_check', file(TCFile)),
    source_file_property(TCFile, load_context(_, File_:_, _)).


:- dynamic current_language/1.
:- dynamic current_types/1.
:- dynamic current_predtypes/1.

:- module_transparent silent_load_files/2.
silent_load_files(Files, Options) :- % load files without printing any message
    open_null_stream(NS),
    assertz(user:message_hook(T,K,Ls)),
    with_output_to(NS, load_files(Files, Options)),%prolog_reader:load_file(File),
    retract(user:message_hook(T,K,Ls)),
    close(NS).

load_program(File) :-
    lpmux__init(File),

    get_file_extension(File, Ext),
    (   Ext = 'pl', !,
        flush_output(user),
        @(silent_load_files(File, [compilation_mode(assert_all)]), instrumented_program),
        flush_output(user),
        (is_type_check_loaded_from_file(File) ->
            assert(current_language('typed-prolog')),

            tprolog:read_type_annotations(File, StdTypes, StdPredTypes),

            foreach(member(T, StdTypes), assertz(current_types(T))),
            foreach(member(T, StdPredTypes), assertz(current_predtypes(T)))
        ;
            assert(current_language('prolog')),

            (\+use_experimental_pltyper -> true ;
                pl_newtyper:proceed(File),
                
                pl_newtyper:pl_newtyper_to_std_types(StdTypes),
                pl_newtyper:pl_newtyper_to_std_predtypes(StdPredTypes),

                foreach(member(T, StdTypes), assertz(current_types(T))),
                foreach(member(T, StdPredTypes), assertz(current_predtypes(T)))
            )
        )
    ;   Ext = 'm', !,
        assert(current_language('mercury')),
        mparser:load_file(File),

        mlib:mlib_to_std_types(StdTypes),
        foreach(member(T, StdTypes), assertz(current_types(T))),
        mlib:mlib_to_std_predtypes(StdPredTypes),
        foreach(member(T, StdPredTypes), assertz(current_predtypes(T))),

        set_prolog_flag(instrumented_program:access_level, system),
        %forall(predicate_property(system:Builtin, built_in), set_prolog_flag(instrumented_program:Builtin, fail)),
        forall(mlib:get_clause(H, B), assertz(instrumented_program:(H :- B)))
    ;
        format(string(S), "Unrecognized file extension: ~a~n", [Ext]), throw(S)
    ),
    format("Loaded file: ~w", [File]), nl.

unload_program :-
    current_filename(File),
    /*write("Unloading "), writeln(File),*/
    unload_file(File),
    unload_file(instrumented_program),
    lpmux__clean.


clause_body_as_list((A;B), A_B) :- var(A_B), !,
    (clause_body_as_list(A, A_B) ; clause_body_as_list(B, A_B)).
clause_body_as_list((A,B), AB_) :- var(AB_), !,
    clause_body_as_list(A, A_), clause_body_as_list(B, B_), append(A_, B_, AB_).
clause_body_as_list(true, []) :- !.
clause_body_as_list(A, [A]) :- !.
clause_body_as_list((A,B), [H | T]) :-
    clause_body_as_list(A, [H]), clause_body_as_list(B, T).

get_clause_body_as_list(Head, ListBody) :-
    current_predicate(_, Head),
    (predicate_property(Head, built_in) -> true ; % bypass privacy restriction
        clause(Head, Body, _),
        clause_body_as_list(Body, ListBody)
    ).

get_program_clause_body_as_list(Head, ListBody) :-
    (current_language('mercury') ->
        mlib:get_clause_as_list(Head, ListBody)
    ;
        lpmux:get_clause_body_as_list(instrumented_program:Head, ListBody),
        ((current_language('prolog'), \+use_experimental_pltyper) ->
            Head =.. [_P | Args],
            pl_oldtyper:pl_oldtyper__esig(Args, ListBody) %% for extracting types
        ; true)
    ).