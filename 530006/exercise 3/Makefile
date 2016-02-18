#!/bin/bash/

.PHONY: all clean

all: ex3p1 ex3p2 ex3p3 ex3p4

ex3p1: mtmod.o ex3p1.o
	gfortran -o $@ $^

ex3p2: mtmod.o ex3p2.o
	gfortran -o $@ $^

ex3p3: mtmod.o ex3p3.o
	gfortran -o $@ $^

ex3p4: mtmod.o ex3p4.o
	gfortran -o $@ $^

%.o: %.f90
	gfortran -c -O2 $<

clean:
	rm -f *.o *.mod
