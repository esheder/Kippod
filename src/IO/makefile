IODIR = IO
BINDIR = ../bin/
SRCOBJ = $(wildcard *.f03)
MODOBJ = $(SRCOBJ:.f03=.mod)
VPATH = ../build
MODP = -I../build -J../build

all: clean parsers.mod materialparsers.mod

parsers.mod : parsers.f03 exceptions.mod mathutils.mod lists.mod parameters.mod
	$(F03) $(FFLAGS) -c $(MODP) parsers.f03

materialparsers.mod : materialparsers.f03 seclists.mod exceptions.mod parsers.mod linelists.mod parameters.mod
	$(F03) $(FFLAGS) -c $(MODP) materialparsers.f03 
#coreparsers.mod : coreparsers.f03 parsers.mod
#	$(F03) $(FFLAGS) -c -I../utils coreparsers.f03

clean :
	rm -f *.mod *.o *~

