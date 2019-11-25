#!/bin/bash

# Very meta. We self-compile three times because if a change was made
# the compiler's output in classy.hs, we need to compile two more
# times to propagate that change.
check_compile() {
    echo 'n' | timeout 10 ./blynn classy "$1" classy2 &&
    echo 'n' | timeout 10 ./blynn classy2 "$1" classy3 &&
    echo 'n' | timeout 10 ./blynn classy3 "$1" classy4 &&
    printf '\n' && diff -qs classy3 classy4;
} 
if [[ $1 == "" ]]
then
    echo "./check_compile.sh <compiler source>"
    exit
fi
check_compile "$1"
