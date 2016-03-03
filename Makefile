# The compiler
FC    =    gfortran

# Flags that are sent to the compiler
FCFLAGS    =    -Wall

all:
	gfortran $(FCFLAGS) src/tictactoe.f90 -o bin/tictactoe

clean:
	@ rm bin/*

run:
	cd bin; ./tictactoe