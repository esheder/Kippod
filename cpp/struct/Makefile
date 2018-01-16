COMP = gcc
FLAGS = -std=c++17 -w -Wfatal-errors

.PHONY : all clean

all : set.o

clean:
	@rm -f *.o *.x

%.o : %.cpp
	$(COMP) $(FLAGS) $^

set.o : set.cpp
