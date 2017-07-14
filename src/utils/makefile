
IODIR = IO
BINDIR = ../bin/
SRCOBJ = $(wildcard *.f03)
MODOBJ = $(SRCOBJ:.f03=.mod)
VPATH = ../build
BD = ../build

all: $(MODOBJ)

LLTest.o : ;
%.mod : %.o ;

linelists.o : lists.mod exceptions.mod parameters.mod
	$(F03) $(FFLAGS) -c $(MODP) linelists.f03 -o $(BD)/$@

lists.o : exceptions.mod
	$(F03) $(FFLAGS) -c $(MODP) lists.f03 -o $(BD)/$@

seclists.o : lists.mod linelists.mod parameters.mod
	$(F03) $(FFLAGS) -c $(MODP) seclists.f03 -o $(BD)/$@


%.o : %.f03
	$(F03) $(FFLAGS) -c $(MODP) $^ -o $(BD)/$@

clean :
	rm -f *.mod *.o *~

test : all ;
