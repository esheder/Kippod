
IODIR = IO
BINDIR = ../bin/
SRCOBJ = $(wildcard *.f03)
MODOBJ = $(SRCOBJ:.f03=.o)
VPATH = ../build
BD = ../build

.PHONY : all clean test
all: $(MODOBJ)

LLTest.o : ;

linelists.o : lists.o exceptions.o parameters.o
	$(F03) $(FFLAGS) -c $(MODP) linelists.f03 -o $(BD)/$@

lists.o : exceptions.o
	$(F03) $(FFLAGS) -c $(MODP) lists.f03 -o $(BD)/$@

seclists.o : lists.o linelists.o parameters.o
	$(F03) $(FFLAGS) -c $(MODP) seclists.f03 -o $(BD)/$@
reallists.o : lists.o parameters.o
	$(F03) $(FFLAGS) -c $(MODP) reallists.f03 -o $(BD)/$@

trees.o : lists.o
	$(F03) $(FFLAGS) -c $(MODP) trees.f03 -o $(BD)/$@



%.o : %.f03
	$(F03) $(FFLAGS) -c $(MODP) $^ -o $(BD)/$@

clean :
	rm -f *.mod *.o *~

test : all ;
