SRCOBJ = $(wildcard *.f03)
MODOBJ = $(SRCOBJ:.f03=.mod)
VPATH = ../build
MODP = -I../build -J../build
BD = ../build

all: materials.o

materials.o : 
	$(F03) $(FFLAGS) -c $(MODP) materials.f03 -o $(BD)/$@

clean:
	@rm -f *.mod *.o

test: all ;
