:- ['main'].
:- qsave_program('concolic_tool',[stand_alone(true),goal((main, halt))]).
:- halt.