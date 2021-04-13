#!/bin/bash

./build_swiplz3.sh
cd ./Pl_concolic_testing
swipl -l "./extras/swiplz3_tester/swiplz3_tester.pl" -g main #"./extras/z3_scripts/test5.txt" #>& "../crash.txt"

read -p "Press [Enter] key to continue..."