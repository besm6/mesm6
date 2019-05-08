CXX             = g++
#CXX             = clang++ -Wno-char-subscripts -Wno-dangling-field
CXXFLAGS        = -g -O -Wall -Werror
LDFLAGS         = -g

all:            pascompl

clean:
		rm -f pascompl *.o

pascompl:       pascompl.o
		$(CXX) $(LDFLAGS) $< $(LIBS) -o $@
