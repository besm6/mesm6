#
# Dis-assembler for micro-BESM
#
DESTDIR = /usr/local
CXXFLAGS = -I.. -O -Werror -Wall
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
