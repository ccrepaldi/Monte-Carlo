#!/bin/bash

N=8

for i in $(seq 2 ${N})
do
    ./ex4p1 $((10 ** i)) >> problem1_output.txt
    echo "Call with Npoints = 1e${i} done!"

done
