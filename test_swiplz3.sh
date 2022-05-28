#!/bin/bash

#./build_swiplz3.sh

cd ./extras/swiplz3_tester
# -- without arguments --
swipl -l "./swiplz3_tester.pl" -g main #>& "./crash.txt"
# -- with arguments --
#swipl -l "./swiplz3_tester.pl" -g main -- "./scripts/sample(cex1).txt"

read -p "Press [Enter] key to continue..."