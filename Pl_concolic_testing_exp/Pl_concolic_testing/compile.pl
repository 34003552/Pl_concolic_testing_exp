:- ['main'].
:- unload_file('extras/type_check/type_check').
:- qsave_program('concolic_tool',[stand_alone(true)]).
:- halt.