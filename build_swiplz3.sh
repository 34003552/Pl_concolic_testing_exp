#!/bin/bash

C_COMPILER=gcc
CPP_COMPILER=g++-9

cd ./swiplz3

C_FILES=$(find ./src -type f -name "*.c") # swiplc.c ctypes.c iolog.c
CPP_FILES=$(find ./src -type f -name "*.cpp") # cppbridge.cpp z3Manager.cpp z3DatatypeHolder.cpp z3FuncDeclHolder.cpp z3Tools.cpp

swipl-ld -cc $C_COMPILER -fpic -c $C_FILES
swipl-ld -c++ $CPP_COMPILER -cc-options,-std=c++17 -fpic -c $CPP_FILES

O_FILES=$(find ./src -type f -name "*.o")

BUILD_DIR=./build

mkdir -p $BUILD_DIR
mv $O_FILES $BUILD_DIR

swipl-ld -shared -o swiplz3 $BUILD_DIR/*.o -lstdc++ -lz3