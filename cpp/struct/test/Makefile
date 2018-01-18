COMP = g++
FLAGS = -std=c++17 -w -Wfatal-errors
TGEN = cxxtestgen
TFLAGS = --error-printer --have-std
CXXTEST = /usr/include/cxxtest
VPATH = ../

.PHONY : all clean testsparsevec

all : testsparsevec

clean:
	@rm -f *.o *.x *~ runsparsevec.cpp

runsparsevec.cpp : sparsevtest.hpp
	$(TGEN) $(TFLAGS) -o $@ $^

runsparsevec.x : runsparsevec.cpp sparsevec.cpp
	$(COMP) $(FLAGS) -I$(CXXTEST) -o $@ $^

testsparsevec : runsparsevec.x
	./$<
