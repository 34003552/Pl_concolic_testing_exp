#!/bin/bash

cd ./Pl_concolic_testing

cd ./swiplz3
swipl-ld -fpic -c pl2cpp/*.c # swiplc.c ctypes.c iolog.c
g++-9 -std=c++17 -fpic -c cppbridge.cpp
g++-9 -std=c++17 -fpic -c z3/*.cpp # z3Manager.cpp z3Bundle.cpp z3Tools.cpp
g++-9 -std=c++17 -fpic -c z3/datatypes/*.cpp # z3DatatypeHolder.cpp z3DatatypeFactory.cpp z3Datatype.cpp

cd ..
swipl-ld -shared -o swiplz3 ./swiplz3/pl2cpp/*.o ./swiplz3/*.o -lstdc++ -lz3