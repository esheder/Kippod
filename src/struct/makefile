SRCOBJ = $(wildcard *.f03)
MODOBJ = $(SRCOBJ:.f03=.mod)
VPATH = ../build
BD = ../build

.PHONY : all clean test

all: materials.o reflectors.o isotopes.o rods.o cores.o

materials.o : 
	$(F03) $(FFLAGS) -c $(MODP) materials.f03 -o $(BD)/$@

reflectors.o : materials.o
	$(F03) $(FFLAGS) -c $(MODP) reflectors.f03 -o $(BD)/$@

isotopes.o : 
	$(F03) $(FFLAGS) -c $(MODP) isotopes.f03 -o $(BD)/$@

rods.o : materials.o
	$(F03) $(FFLAGS) -c $(MODP) rods.f03 -o $(BD)/$@

cores.o : reflectors.o rods.o
	$(F03) $(FFLAGS) -c $(MODP) cores.f03 -o $(BD)/$@

clean:
	@rm -f *.mod *.o

test: all ;
