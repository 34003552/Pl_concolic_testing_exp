#!/bin/bash

#./build_swiplz3.sh

cd ./concolic_tool
swipl -q --goal=main --toplevel=halt --stand_alone=true -o concolic_tool -c main.pl
./concolic_tool -vv -cg "p(s(a))" -ground "[1]" -depth "2" -timeout "10" -file "examples/ex01.pl" -trace #&> "../crash.txt"

read -p "Press [Enter] key to continue..."