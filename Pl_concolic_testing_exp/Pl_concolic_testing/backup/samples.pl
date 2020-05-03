:- module(samples, [load_samples/0, sample/1]).

load_samples :- File = "samples.txt", (exists_file(File) -> (open(File, read, Stream), read_file(Stream, _Lines), close(Stream), asserta(samples_loaded)) ; true).

read_file(Stream, []) :- at_end_of_stream(Stream).
read_file(Stream, [X|L]) :- \+at_end_of_stream(Stream), read(Stream, X), parse(X), read_file(Stream, L).

parse(X) :- X =.. List, phrase(sentence(Statement), List), assertz(Statement).

sentence(Statement) --> [ '=', Name, ArgsList ], { Statement = sample(Name, ArgsList) }.

set_current_sample([_,_,_,_,_,NewFile]) :- (
		predicate_property(current_sample_file(OldFile),defined) -> (
			retract(current_sample_file(OldFile)), /*write("Unloading "), writeln(OldFile),*/ unload_file(OldFile) 
		) ; true
	),
	asserta(current_sample_file(NewFile)).

sample(Name) :- predicate_property(samples_loaded,defined) -> (
		sample(Name, ArgsList), set_current_sample(ArgsList), apply(main, ArgsList)
	) ; throw("the samples description file has not been loaded").
