#
# Linker and other object file utilities
#
# To install libarchive, use:
#       sudo apt install libarchive-dev
#
DESTDIR = /usr/local/bin
CFLAGS  = -O -g -Werror -Wall $(shell pkg-config --cflags libarchive)
LDFLAGS =
LIBS    = $(shell pkg-config --libs libarchive)

ld      = besm6-ld
strip   = besm6-strip
nm      = besm6-nm
objdump = besm6-objdump
size    = besm6-size

all:    ld size strip nm objdump

ld:     ld.o stdobj.o
	$(CC) $(LDFLAGS) ld.o stdobj.o $(LIBS) -o $@

strip:  strip.o stdobj.o
	$(CC) $(LDFLAGS) strip.o stdobj.o -o $@

nm:     nm.o stdobj.o
	$(CC) $(LDFLAGS) nm.o stdobj.o $(LIBS) -o $@

size:   size.o stdobj.o
	$(CC) $(LDFLAGS) size.o stdobj.o -o $@

objdump: objdump.o stdobj.o
	$(CC) $(LDFLAGS) objdump.o stdobj.o -o objdump

install: $(DESTDIR)/$(ld) $(DESTDIR)/$(size) $(DESTDIR)/$(strip) \
         $(DESTDIR)/$(nm) $(DESTDIR)/$(objdump)

clean:
	rm -f *.o *.b a.out core ld strip nm size objdump

$(DESTDIR)/$(ld): ld
	install -s ld $(DESTDIR)/$(ld)

$(DESTDIR)/$(nm): nm
	install -s nm $(DESTDIR)/$(nm)

$(DESTDIR)/$(size): size
	install -s size $(DESTDIR)/$(size)

$(DESTDIR)/$(strip): strip
	install -s strip $(DESTDIR)/$(strip)

$(DESTDIR)/$(objdump): objdump
	install -s objdump $(DESTDIR)/$(objdump)

###
ld.o: ld.c stdobj.h
nm.o: nm.c stdobj.h
objdump.o: objdump.c stdobj.h
size.o: size.c stdobj.h
stdobj.o: stdobj.c stdobj.h
strip.o: strip.c stdobj.h
