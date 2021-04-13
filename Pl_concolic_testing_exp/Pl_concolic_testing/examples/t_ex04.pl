:- style_check(-singleton).
:- use_module('../extras/type_check/type_check').
:- style_check(+singleton).

:- type unk0 ---> monday ; thuesday ; wednesday ; thursday ; friday ; saturday ; sunday ; eastern ; first_of_may ; christmas ; new_years_day ; friday_the_13th.
:- type unk1 ---> sunny ; rainy ; foggy ; windy.
:- type unk2 ---> go_to_work ; go_out_to_the_nature ; visit_the_golf_club ; wash_your_car ; go_out_to_the_town ; visit_the_bridge_club ; enjoy_yourself_at_home ;
	it_is_fun_to_learn_Japanese ; you_had_better_stay_in_bed.
:- type unk3 ---> workday ; weekend ; feastday ; badday.
:- type unk4 ---> nice ; nasty.

:- pred what_to_do_today(unk0, unk1, unk2).
what_to_do_today( _today, _weather, _program ):-
    kind_of_day( _today, _daykind ),
    kind_of_weather( _weather, _weatherkind ),
    proposal( _daykind, _weatherkind, _program ).

:- pred kind_of_day(unk0, unk3).
kind_of_day( monday, workday ). 
kind_of_day( thuesday, workday ).
kind_of_day( wednesday, workday ).
kind_of_day( thursday, workday ).
kind_of_day( friday, workday ).
kind_of_day( saturday, weekend ).
kind_of_day( sunday, weekend ).
kind_of_day( eastern, feastday ).
kind_of_day( first_of_may, feastday ).
kind_of_day( christmas, feastday ).
kind_of_day( new_years_day, badday ).
kind_of_day( friday_the_13th, badday ).

:- pred kind_of_weather(unk1, unk4).
kind_of_weather( sunny, nice ).
kind_of_weather( rainy, nasty ).
kind_of_weather( foggy, nasty ).
kind_of_weather( windy, nasty ).

:- pred proposal(unk3, unk4, unk2).
proposal( workday, _, go_to_work ).
proposal( weekend, nice, go_out_to_the_nature ).
proposal( weekend, nice, visit_the_golf_club ).
proposal( weekend, nice, wash_your_car ).
proposal( weekend, nasty, go_out_to_the_town ).
proposal( weekend, nasty, visit_the_bridge_club ).
proposal( weekend, nasty, enjoy_yourself_at_home ).
proposal( weekend, _, it_is_fun_to_learn_Japanese ).
proposal( badday, _, you_had_better_stay_in_bed ).
proposal( feastday, _weather, _program ) :-
    proposal( weekend, _weather, _program ).
