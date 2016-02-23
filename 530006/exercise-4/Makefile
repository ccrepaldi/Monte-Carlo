#!/bin/bash/

.PHONY: all clean

all: ex4p1 ex4p2

ex4p1: mtmod.o ex4p1.o
	gfortran -o $@ $^

ex4p2: mtmod.o ex4p2.o
	gfortran -o $@ $^

%.o: %.f90
	gfortran -c -O2 $<

clean:
	rm -f *.o *.mod
