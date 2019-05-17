#
# objdump: show contents of object files for BESM-6
#
DESTDIR = /usr/local
CFLAGS  = -I.. -O0 -Werror -Wall
LDFLAGS =
VPATH   = ../ld

objdump = besm6-objdump

all:    objdump

objdump: objdump.o
	$(MAKE) -C ../ld fgethdr.o fgeth.o
	$(CC) $(LDFLAGS) objdump.o -o objdump ../ld/fgethdr.o ../ld/fgeth.o

install: $(DESTDIR)/bin/$(objdump)

clean:
	rm -f *.o *.b a.out core objdump

$(DESTDIR)/bin/$(objdump): objdump
	install -s objdump $(DESTDIR)/bin/$(objdump)

###
objdump.o: objdump.c ../mesm6/a.out.h
