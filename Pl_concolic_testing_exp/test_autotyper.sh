#!/bin/bash

cd ./Pl_concolic_testing
swipl -l "./extras/autotyper/autotyper.pl" -g main _ "./examples/bench/chat_parser.pl" #&> "../crash.txt" #"./examples/typ.pl"

# OK with bar, boh, ex0, ex01, ex02, ex03, ex04, ex05, ex06, ex07, ex08, ex09, ex10, ex11, ex12, ex13, ex14, ex15, ex16, ex17, ex18, ex19, ex20, ex21, ex22, ex23, ex24, ex25, ex26, ex27, foo
	# $boyer, $browse, $chat_parser, $crypt, $dynamic_unit_clause, $fast_mu, $harness, $itak, $meta_qsort, $mu, $nreverse, $nreverse_builtin, $poly, $primes, $qsort, $queens, $query, $sendmore, $tak, $zebra 
# annoying problems with $main@user_preds(load_files/1),$prover@user_defined_ops, $flatten@parse_clause, $unify@DCG, $reducer@DCG, $simple_analyzer@DCG

read -p "Press [Enter] key to continue..."