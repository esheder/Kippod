
IODIR = IO
BINDIR = ../bin/
SRCOBJ = $(wildcard *.f03)
MODOBJ = $(SRCOBJ:.f03=.mod)
MODP = -I../build -J../build

all: clean $(MODOBJ)

linelists.mod : linelists.f03 lists.mod exceptions.mod parameters.mod
	$(F03) $(FFLAGS) -c $(MODP) linelists.f03

lists.mod : lists.f03 exceptions.mod
	$(F03) $(FFLAGS) -c $(MODP) lists.f03

seclists.mod : seclists.f03 lists.mod linelists.mod parameters.mod
	$(F03) $(FFLAGS) -c $(MODP) seclists.f03

%.mod : %.f03
	$(F03) $(FFLAGS) -c $(MODP) $^

clean :
	rm -f *.mod *.o *~
