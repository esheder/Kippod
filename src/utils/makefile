
IODIR = IO
BINDIR = ../bin/
SRCOBJ = $(wildcard *.f03)
MODOBJ = $(SRCOBJ:.f03=.mod)

all: clean $(MODOBJ)

lists.mod : lists.f03 exceptions.mod
	$(F03) $(FFLAGS) -c lists.f03

%.mod : %.f03
	$(F03) $(FFLAGS) -c $^

clean :
	rm -f *.mod *.o *~
