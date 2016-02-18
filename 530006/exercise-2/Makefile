#!/bin/bash/

.PHONY: all clean

all: ex2p1 ex2p2 ex2p3 ex2p3b ex2p4 ex2p4b

ex2p1: mtmod.o rngmod.o ex2p1.o
	gfortran -o ex2p1 ex2p1.o mtmod.o rngmod.o

ex2p2: mtmod.o rngmod.o ex2p2.o
	gfortran -o ex2p2 ex2p2.o mtmod.o rngmod.o

ex2p3: mtmod.o rngmod.o ex2p3.o
	gfortran -o ex2p3 ex2p3.o mtmod.o rngmod.o

ex2p3b: mtmod.o rngmod.o ex2p3b.o
	gfortran -o ex2p3b ex2p3b.o mtmod.o rngmod.o

ex2p4: ex2p4.f90
	gfortran -o ex2p4 ex2p4.f90

ex2p4b: ex2p4b.f90
	gfortran -o ex2p4b ex2p4b.f90

mtmod.o: mtmod.f90
	gfortran -c -O2 mtmod.f90

rngmod.o: rngmod.f90
	gfortran -c -O2 rngmod.f90

ex2p1.o: ex2p1.f90
	gfortran -c -O2 ex2p1.f90

ex2p2.o: ex2p2.f90
	gfortran -c -O2 ex2p2.f90

ex2p3.o: ex2p3.f90
	gfortran -c -O2 ex2p3.f90

ex2p3b.o: ex2p3b.f90
	gfortran -c -O2 ex2p3b.f90

clean:
	rm -f *.o *.mod
