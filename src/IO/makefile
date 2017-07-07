SRCOBJ = $(wildcard *.f03)
MODOBJ = $(SRCOBJ:.f03=.mod)
BD = ../build
VPATH = $(BD)
MODP = -I$(BD) -J$(BD)

all: parsers.o materialparsers.o reflectorparsers.o

%.mod : %.o ;

parsers.o : 
	$(F03) $(FFLAGS) -c $(MODP) parsers.f03 -o $(BD)/$@

materialparsers.o: parsers.mod
	$(F03) $(FFLAGS) -c $(MODP) materialparsers.f03 -o $(BD)/$@
reflectorparsers.o: parsers.mod
	$(F03) $(FFLAGS) -c $(MODP) reflectorparsers.f03 -o $(BD)/$@

MatParseTest.x : materialparsers.o
	$(F03) $(FFLAGS) $(MODP) $(BD)/materialparsers.o $(BD)/parsers.o $(BD)/lists.o \
	$(BD)/linelists.o $(BD)/exceptions.o $(BD)/seclists.o $(BD)/mathutils.o $(BD)/strutils.o \
	MatParseTest.f03 -o MatParseTest.x
#coreparsers.mod : coreparsers.f03 parsers.mod
#	$(F03) $(FFLAGS) -c -I../utils coreparsers.f03

test : clean MatParseTest.x
	@./MatParseTest.x > sol1
	@echo "  Wrong word count in material line. CNT was   6 but must be   4 Line was: My line is stupid and fails" > sol2
	@diff sol1 sol2
	@rm sol1 sol2 2>/dev/null
clean :
	@rm -f *.mod *.o *~ *.x

