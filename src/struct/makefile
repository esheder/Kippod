SRCOBJ = $(wildcard *.f03)
MODOBJ = $(SRCOBJ:.f03=.mod)
VPATH = ../build
BD = ../build

all: materials.o reflectors.o isotopes.o

materials.o : 
	$(F03) $(FFLAGS) -c $(MODP) materials.f03 -o $(BD)/$@

reflectors.o : materials.o
	$(F03) $(FFLAGS) -c $(MODP) reflectors.f03 -o $(BD)/$@
isotopes.o : 
	$(F03) $(FFLAGS) -c $(MODP) isotopes.f03 -o $(BD)/$@

clean:
	@rm -f *.mod *.o

test: all ;
