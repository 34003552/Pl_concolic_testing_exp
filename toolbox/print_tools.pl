%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some pretty-printing utilities..
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(print_tools, [
    assert_verbose/0, assert_very_verbose/0,

    v_format/1, v_format/2,
    vv_format/1, vv_format/2
]).

:- dynamic verbose/0.
:- dynamic very_verbose/0.

assert_verbose :- verbose -> true ; assertz(verbose).

assert_very_verbose :- very_verbose -> true ; assert_verbose, assertz(very_verbose).

:- module_transparent([v_format/1, v_format/2, vv_format/1, vv_format/2]).

%format(Text) :- format(user, Text, []).
%format(Format, Args) :- format(user, Format, Args).

v_format(Text) :- verbose -> format(Text) ; true.
v_format(Format, Args) :- verbose -> format(Format, Args) ; true.

vv_format(Text) :- very_verbose -> format(Text) ; true.
vv_format(Format, Args) :- very_verbose -> format(Format, Args) ; true.