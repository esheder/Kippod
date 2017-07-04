
IODIR = IO
STRCTDIR = struct
SOLVEDIR = solvers
UTILDIR = utils
BINDIR = ../bin
srcdirs = . $(IODIR) $(SOLVEDIR) $(STRCTDIR) $(UTILDIR)

F03 = gfortran
FFLAGS = -O2

export F03
export FFLAGS

all: cutils cIO csolvers structures

cutils:
	$(MAKE) -C utils
cIO: cutils
	$(MAKE) -C IO
csolvers: cutils
	cd solvers && make
structures: cutils
	cd struct && make

.PHONY: clean
clean:
	rm -f LLTest.x
	for i in $(srcdirs);  do \
		rm -f $$i/*.o $$i/*.mod $$i/*~ ; \
	done
