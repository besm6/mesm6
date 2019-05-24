#
# Linker, archiver and other object file utilities
#
# To install libarchive, use:
#       sudo apt install libarchive-dev
#
DESTDIR = /usr/SVSB
CFLAGS  = -I../objdump -Werror -Wall
LDFLAGS =
LIBS    = -larchive

ld      = mkb-ld
strip   = mkb-strip
nm      = mkb-nm
size    = mkb-size

all:    ld #strip nm size

ld:     ld.o stdobj.o
	$(CC) $(LDFLAGS) ld.o -o ld stdobj.o $(LIBS)

strip:  strip.o
	$(CC) $(LDFLAGS) strip.o -o strip

nm:     nm.o
	$(CC) $(LDFLAGS) nm.o -o nm

size:   size.o
	$(CC) $(LDFLAGS) size.o -o size

install: $(DESTDIR)/bin/$(ld) $(DESTDIR)/bin/$(nm)\
		$(DESTDIR)/bin/$(size) $(DESTDIR)/bin/$(strip)\
		$(DESTDIR)/bin/$(lorder)
clean:
	rm -f *.o *.b a.out core ot ld strip nm size

stdobj.o: ../objdump/stdobj.c ../objdump/stdobj.h
	$(CC) $(CFLAGS) -c ../objdump/stdobj.c -o $@

$(DESTDIR)/bin/$(ld): ld
	install -s ld $(DESTDIR)/bin/$(ld)

$(DESTDIR)/bin/$(nm): nm
	install -s nm $(DESTDIR)/bin/$(nm)

$(DESTDIR)/bin/$(size): size
	install -s size $(DESTDIR)/bin/$(size)

$(DESTDIR)/bin/$(strip): strip
	install -s strip $(DESTDIR)/bin/$(strip)

###
ld.o: ld.c ../mesm6/a.out.h
