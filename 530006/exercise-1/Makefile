#!/bin/bash/

.PHONY: all clean

all: ex1p2 ex1p3 ex1p4 ex1p5 ex1p6

ex1p2: mtmod.o rngmod.o ex1p2.o
	gfortran -o ex1p2 ex1p2.o mtmod.o rngmod.o

ex1p3: mtmod.o rngmod.o ex1p3.o
	gfortran -o ex1p3 ex1p3.o mtmod.o rngmod.o

ex1p4: mtmod.o rngmod.o ex1p4.o
	gfortran -o ex1p4 ex1p4.o mtmod.o rngmod.o

ex1p5: ex1p5.f90
	gfortran -o ex1p5 ex1p5.f90

ex1p6: mtmod.o rngmod.o ex1p6.o
	gfortran -o ex1p6 ex1p6.o mtmod.o rngmod.o

mtmod.o: mtmod.f90
	gfortran -c -O2 mtmod.f90

rngmod.o: rngmod.f90
	gfortran -c -O2 rngmod.f90

ex1p2.o: ex1p2.f90
	gfortran -c -O2 ex1p2.f90

ex1p3.o: ex1p3.f90
	gfortran -c -O2 ex1p3.f90

ex1p4.o: ex1p4.f90
	gfortran -c -O2 ex1p4.f90

ex1p6.o: ex1p6.f90
	gfortran -c -O2 ex1p6.f90

clean:
	rm -f *.o *.mod
