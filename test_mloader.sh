#!/bin/bash

cd ./extras/mercury_loader
swipl -l "./mloader.pl" -g main -- "../../concolic_tool/examples/mercury/samples/eliza.m"

#beer			OK
#calculator		DCG?
#calculator2	try?
#cat			OK with "../../README.md"
#e				OK
#eliza			OK
#expand_terms	OK
#hello			OK
#interpreter	type?
#sort			operators?
#ultra_sub		"!." ?

read -p "Press [Enter] key to continue..."