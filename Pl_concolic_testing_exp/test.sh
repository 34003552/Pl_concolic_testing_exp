#!/bin/bash

./build_swiplz3.sh
cd ./Pl_concolic_testing
swipl -l "./main.pl" #&> "../crash.txt"
read -p "Press [Enter] key to continue..."