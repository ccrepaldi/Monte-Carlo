#!/bin/bash/

.PHONY: all clean

all: ex1p1 ex1p2 ex1p3 ex1p4

ex1p1: mtmod.o ex1p1.o
	gfortran -o $@ $^

ex1p2: mtmod.o ex1p2.o
	gfortran -o $@ $^

ex1p3: mtmod.o ex1p3.o
	gfortran -o $@ $^

ex1p4: mtmod.o ex1p4.o
	gfortran -o $@ $^

%.o: %.f90
	gfortran -c -O2 $<

clean:
	rm -f *.o *.mod
