#!/bin/bash

# Check that self-compiling works and produces the same output.
check() {
    make
    echo 'n' | timeout 10 ./blynn classy $1 classy2 &&
    echo 'n' | timeout 10 ./blynn classy2 $1 classy3 &&
    printf '\n' && diff -qs classy2 classy3;
} 
check $1
