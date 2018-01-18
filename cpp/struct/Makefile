COMP = gcc
FLAGS = -std=c++17 -w -Wfatal-errors

.PHONY : all clean

all : sparsevec.o

clean:
	@rm -f *.o *.x *~

%.o : %.cpp
	$(COMP) $(FLAGS) -c $<

set.o : set.cpp sparsevec.hpp set.hpp
sparsevec.o : sparsevec.cpp sparsevec.hpp
