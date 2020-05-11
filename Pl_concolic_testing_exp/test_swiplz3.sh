#!/bin/bash

cd ./Pl_concolic_testing
cd ./swiplz3
g++ -std=c++17 -fpic -c z3/z3Tools.cpp
g++ -std=c++17 -fpic -c z3/z3Bundle.cpp
g++ -std=c++17 -fpic -c z3/z3Manager.cpp
g++ -std=c++17 -fpic -c cppbridge.cpp
swipl-ld -fpic -c swiplc.c
cd ..
swipl-ld -shared -o swiplz3 ./swiplz3/swiplc.o ./swiplz3/cppbridge.o ./swiplz3/z3Manager.o ./swiplz3/z3Bundle.o ./swiplz3/z3Tools.o -lstdc++ -lz3
swipl -l "./swiplz3_tester.pl" -g main "./scripts/cex7p0.txt" #>& "../crash.txt"
 read -p "Press [Enter] key to continue..."
