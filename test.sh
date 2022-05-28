#!/bin/bash

#./build_swiplz3.sh

cd ./concolic_tool
# -- without arguments --
swipl -l "./main.pl" #&> "./crash.txt"
# -- with arguments --
#swipl -l "./main.pl" -t main -- -vv -cg "p(s(a))" -ground "[1]" -k -depth "2" -timeout "10" -file "examples/ex01.pl" -trace #&> "./crash.txt"

read -p "Press [Enter] key to continue..."