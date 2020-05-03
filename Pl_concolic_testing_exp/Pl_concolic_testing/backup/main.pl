:- use_module(concolic_tool).
:- use_module(samples).

main(_,_,_,_,_,_) :- writeln("Meow").
main :- writeln("Hello World").

:- initialization(load_samples).