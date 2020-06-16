:- if(predicate_property(use_type_checker, defined)).

:- type flag_t --->
    testcase(any, list(any)) ; pred(any, integer) ; fun(any, integer) ; verbose ; very_verbose ;
    cg(any) ; sg(any) ; depth(any) ; timeout(any) ; with_trace ;
    ground(any) ; interactive ; help ; file(any).

:- pred main.

% JE: wrong signature!!!
%:- pred main(any, list(integer), integer, integer, boolean, any).

:- pred error_process(any).

:- pred get_options(list(any), list(flag_t), list(any)).

:- pred recognise_option(list(any), flag_t, list(any)).

:- pred recognised_option(list(any), flag_t).


:- pred append__any(list(any), list(any), list(any)).
:- pred member__flag(flag_t, list(flag_t)).
:- pred print__list_flag(list(flag_t)).
%:- pred print__any(any).


append__any(L1, L2, L12) :- append(L1, L2, L12) :: append(list(any), list(any), list(any)).
member__flag(T, L) :- member(T, L) :: member(flag_t, list(flag_t)).
print__list_flag(T) :- print(T) :: print(list(flag_t)).
%print__any(T) :- print(T) :: print(any).

:- else.

append__any(L1, L2, L12) :- append(L1, L2, L12).
member__flag(T, L) :- member(T, L).
print__list_flag(T) :- print(T).
%print__any(T) :- print(T).

:- endif.