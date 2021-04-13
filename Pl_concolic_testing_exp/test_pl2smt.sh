#!/bin/bash

cd ./Pl_concolic_testing
swipl -l "./extras/pl2smt_translator/pl2smt.pl" -g main

read -p "Press [Enter] key to continue..."