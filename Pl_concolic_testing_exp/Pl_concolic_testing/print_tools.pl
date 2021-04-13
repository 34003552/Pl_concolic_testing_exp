%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some pretty-printing utilities..
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(print_tools, [
        assert_verbose/0, assert_very_verbose/0,

        printf/1, printf/2,
        v_printf/1, v_printf/2,
        vv_printf/1, vv_printf/2
    ]).

:- dynamic verbose/0.
:- dynamic very_verbose/0.

assert_verbose :- verbose -> true ; assertz(verbose).

assert_very_verbose :- very_verbose -> true ; assert_verbose, assertz(very_verbose).

printf(Text) :- write(user, Text).
printf(Format, Args) :- format(user, Format, Args).

v_printf(Text) :- verbose -> printf(Text) ; true.
v_printf(Format, Args) :- verbose -> printf(Format, Args) ; true.

vv_printf(Text) :- very_verbose -> printf(Text) ; true.
vv_printf(Format, Args) :- very_verbose -> printf(Format, Args) ; true.