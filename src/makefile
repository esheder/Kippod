
IODIR = IO
STRCTDIR = struct
SOLVEDIR = solvers
UTILDIR = utils
BINDIR = ../bin
BUILDIR = build
BD = ../build
srcdirs = . $(IODIR) $(SOLVEDIR) $(STRCTDIR) $(UTILDIR) $(BUILDIR)

F03 = gfortran
FFLAGS = -O2
MKFLGS = --no-print-directory
#MODP = -I$(BD) -module $(BD)
MODP = -I$(BD) -J$(BD)

export F03
export FFLAGS
export MODP

all: clean cutils structures cIO csolvers ;

cutils:
	@$(MAKE) -C utils $(MKFLGS)
cIO: cutils
	@$(MAKE) -C IO $(MKFLGS)
csolvers: cutils
	@$(MAKE) -C solvers $(MKFLGS)
structures: cutils
	@$(MAKE) -C struct $(MKFLGS)


solvetest:
	@$(MAKE) -C solvers $(MKFLGS) test
utiltest: 
	@$(MAKE) -C utils $(MKFLGS) test
structtest: 
	@$(MAKE) -C struct $(MKFLGS) test
iotest:
	@$(MAKE) -C IO $(MKFLGS) test


test: utiltest structtest solvetest iotest ;

.PHONY: clean test iotest structtest utiltest solvetest structures csolvers cIO cutils all
clean:
	@rm -f LLTest.x
	@for i in $(srcdirs);  do \
		rm -f $$i/*.o $$i/*.mod $$i/*~ $$i/*.x ; \
	done
