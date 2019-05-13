#
# Dis-assembler for micro-BESM
#
DESTDIR = /usr/local
CXXFLAGS = -std=c++11 -O -Werror -Wall -I..
LDFLAGS =

dtran   = besm6-dtran

all:    dtran

dtran:  dtran.o
	$(CXX) $(LDFLAGS) dtran.o -o $@

install: $(DESTDIR)/bin/$(dtran)

clean:
	rm -f *.o a.out core dtran

$(DESTDIR)/bin/$(dtran): dtran
	install -s dtran $(DESTDIR)/bin/$(dtran)

###
