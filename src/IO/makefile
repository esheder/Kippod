IODIR = IO
BINDIR = ../bin/
SRCOBJ = $(wildcard *.f03)
MODOBJ = $(SRCOBJ:.f03=.mod)
VPATH = ../utils

all: clean parsers.mod

parsers.mod : parsers.f03 exceptions.mod mathutils.mod lists.mod parameters.mod
	$(F03) $(FFLAGS) -c -I../utils parsers.f03 
#coreparsers.mod : coreparsers.f03 parsers.mod
#	$(F03) $(FFLAGS) -c -I../utils coreparsers.f03

clean :
	rm -f *.mod *.o *~

