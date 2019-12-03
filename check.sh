#!/bin/bash

TIMEOUT=60
# Check that self-compiling works
stage1() {
    echo 'n' | timeout $TIMEOUT ./blynn classy "$1" classy2 ||
        (echo 'Stage 1 fail' && exit 1)
}

stage2() {
    echo 'n' | timeout $TIMEOUT ./blynn classy2 "$1" classy3 ||
        (echo 'Stage 2 fail' && exit 1)
}

check() {
    stage1 "$1" &&
    stage2 "$1" &&
    printf '\n' &&
    diff -qs classy2 classy3
} 
if [[ $1 == "" ]]
then
   echo "./check.sh <compiler source>"
   exit
fi
check "$1"
