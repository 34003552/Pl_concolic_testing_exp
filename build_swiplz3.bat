@echo off
setlocal EnableDelayedExpansion

set C_COMPILER=gcc
set CPP_COMPILER=g++

cd .\swiplz3

for /f "delims=" %%a in ('dir /s /b .\src\*.c') do set "C_FILES=!C_FILES! %%a"
for /f "delims=" %%a in ('dir /s /b .\src\*.cpp') do set "CPP_FILES=!CPP_FILES! %%a"

swipl-ld -cc %C_COMPILER% -cc-options,-fpic -c %C_FILES%
swipl-ld -c++ %CPP_COMPILER% -cc-options,-std=gnu++17,-fpic -c %CPP_FILES%

set BUILD_DIR=.\build

if not exist %BUILD_DIR% mkdir %BUILD_DIR%
for /f "delims=" %%a in ('dir /s /b .\src\*.obj') do move %%a %BUILD_DIR% >nul

swipl-ld -shared -o swiplz3 %BUILD_DIR%\*.obj -lstdc++ -lz3