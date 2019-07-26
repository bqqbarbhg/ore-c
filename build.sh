#!/usr/bin/env bash

mkdir -p proj
gcc -g -Werror -std=gnu99 src/*.c -o proj/test-gcc
clang -g -Werror -std=gnu99 src/*.c -o proj/test-clang
 
