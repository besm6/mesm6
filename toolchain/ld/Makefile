#
# Linker and other object file utilities
#
# To install libarchive, use:
#       sudo apt install libarchive-dev
#
DESTDIR = /usr/local/bin
CFLAGS  = -O -g -Werror -Wall -I../objdump
LDFLAGS =
LIBS    = -larchive

ld      = besm6-ld
strip   = besm6-strip
nm      = besm6-nm
size    = besm6-size

all:    ld size strip nm

ld:     ld.o stdobj.o
	$(CC) $(LDFLAGS) ld.o stdobj.o $(LIBS) -o $@

strip:  strip.o stdobj.o
	$(CC) $(LDFLAGS) strip.o stdobj.o -o $@

nm:     nm.o stdobj.o
	$(CC) $(LDFLAGS) nm.o stdobj.o $(LIBS) -o $@

size:   size.o stdobj.o
	$(CC) $(LDFLAGS) size.o stdobj.o -o $@

install: $(DESTDIR)/$(ld) $(DESTDIR)/$(size) $(DESTDIR)/$(strip) $(DESTDIR)/$(nm)

clean:
	rm -f *.o *.b a.out core ld strip nm size

stdobj.o: ../objdump/stdobj.c ../objdump/stdobj.h
	$(CC) $(CFLAGS) -c ../objdump/stdobj.c -o $@

$(DESTDIR)/$(ld): ld
	install -s ld $(DESTDIR)/$(ld)

$(DESTDIR)/$(nm): nm
	install -s nm $(DESTDIR)/$(nm)

$(DESTDIR)/$(size): size
	install -s size $(DESTDIR)/$(size)

$(DESTDIR)/$(strip): strip
	install -s strip $(DESTDIR)/$(strip)

###
ld.o: ld.c ../objdump/stdobj.h
nm.o: nm.c ../objdump/stdobj.h
size.o: size.c ../objdump/stdobj.h
strip.o: strip.c ../objdump/stdobj.h
