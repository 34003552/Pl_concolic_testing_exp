#!/bin/bash

./build_swiplz3.sh
cd ./Pl_concolic_testing
swipl -l "./compile.pl"
./concolic_tool -vv -cg "p(s(a))" -ground "[1]" -depth "2" -timeout "10" -file "examples/ex01.pl" -trace #&> "../crash.txt"
 read -p "Press [Enter] key to continue..."