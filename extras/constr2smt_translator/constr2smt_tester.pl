
:- use_module('../../concolic_tool/constr2smt').


mk_const(VN, VT_) :-
    constr2smt:dt_translate(VT_, VT),
    format("(declare-const ~a ~s)", [VN, VT]),nl.

mk_func(F_, Args_, Ret_, Body, Rec) :-
    constr2smt:dt_translate(Ret_, Ret),
    (Body == "" ->
        atomics_to_string(Args_, " ", Args),
        format("(declare-fun ~a (~s) ~s)", [F_, Args, Ret])
    ;
        length(Args_, Arity),
        gen_tools:generate_list([I, O]>>(
            nth0(I, Args, Arg_),
            constr2smt:dt_translate(Arg_, Arg),
            format(string(O), "(x!~d ~s)", [I, Arg])
        ), Arity, NArgs_),
        atomics_to_string(NArgs_, " ", Args),
        (Rec == false ->
            format("(define-fun ~a (~s) ~s ~s)", [F_, Args, Ret, Body])
        ;
            format("(define-fun-rec ~a (~s) ~s ~s)", [F_, Args, Ret, Body])
        )
    ),nl.

:- dynamic declared_funcs/3.

ask :-
    read(Clause_),

    ((VarSelector/Clause = Clause_, VarSelector =.. ['{}' | VarSelection]) ->
        term_variables(VarSelection, SelVars),
        term_variables(Clause, Vars),
        include({Vars}/[W]>>(member(V, Vars), V == W), SelVars, SelectedVars)
    ;
        Clause = Clause_,
        term_variables(Clause, Vars),
        SelectedVars = []
    ),
    numbervars(Vars),
    maplist([Var, VarN:_VarT]>>with_output_to(atom(VarN), write(Var)), SelectedVars, NewVars),

    constr2smt:termconstr2smtlib([Clause], NewVars, RqFuncs, SMT),

    foreach(member(VN:VT, NewVars), mk_const(VN, VT)),

    foreach((member(func(F_, Args_, Ret_, Body, Rec), RqFuncs), \+declared_funcs(F_, Args_, Ret_)), (
        mk_func(F_, Args_, Ret_, Body, Rec),
        assertz(declared_funcs(F_, Args_, Ret_))
    )),

    format("(assert ~s)", [SMT]),nl,
    ask.

main :-
    writeln("--------------------------------------------"),
    writeln("Welcome to the SMTLIB2 constraint translator"),
    writeln("--------------------------------------------"),
    ask,
    halt.