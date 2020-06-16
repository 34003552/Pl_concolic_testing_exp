:- if(predicate_property(use_type_checker, defined)).

:- trust_pred nl(any).

:- pred verbose.
:- pred very_verbose.

:- pred assert_verbose.
:- pred assert_very_verbose.

:- pred vprint(any).
:- pred vprintln(any).
:- pred vprintln_atom(any).
:- pred vvprint(any).
:- pred vvprintln(any).
:- pred vvprintln_atom(any).
:- pred println(any).
:- pred print_atom(any).
:- pred println_atom(any).

:- else.

:- endif.