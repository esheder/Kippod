SRCOBJ = $(wildcard *.f03)
MODOBJ = $(SRCOBJ:.f03=.mod)
VPATH = ../build
MODP = -I../build -J../build

all: clean materials.mod

materials.mod : materials.f03 exceptions.mod
	$(F03) $(FFLAGS) -c $(MODP) materials.f03
clean:
	rm -f *.mod *.o
