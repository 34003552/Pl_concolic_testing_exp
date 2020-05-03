#!/bin/bash

cd ./Pl_concolic_testing
cd ./swiplz3
#swipl-ld -fpic -c cpp_code.cpp
swipl-ld -fpic -c swiplz3.c
cd ..
swipl-ld -shared -o swiplz3 ./swiplz3/swiplz3.o #./swiplz3/cpp_code.o
swipl -l "./compile.pl" -g halt
./concolic_tool

read -p "Press [Enter] key to continue..."