#!/bin/bash

cd ./extras/constr2smt_translator
swipl -l "./constr2smt_tester.pl" -g main

read -p "Press [Enter] key to continue..."