#
# Linker, archiver and other object file utilities
#
DESTDIR = /usr/SVSB
CFLAGS  = -I.. -I../objdump -Werror -Wall
LDFLAGS =
LIBS    = -larchive

ld      = mkb-ld
strip   = mkb-strip
nm      = mkb-nm
size    = mkb-size

all:    ld #strip nm size

ld:     ld.o ../objdump/stdobj.o fgetint.o fgetsym.o fputsym.o \
		fgeth.o fputh.o
	$(CC) $(LDFLAGS) ld.o -o ld ../objdump/stdobj.o fgetint.o \
		fgetsym.o fputsym.o fgeth.o fputh.o $(LIBS)

strip:  strip.o fgeth.o fputh.o
	$(CC) $(LDFLAGS) strip.o -o strip fgeth.o fputh.o

nm:     nm.o fgetsym.o fgetint.o fgeth.o
	$(CC) $(LDFLAGS) nm.o -o nm fgetsym.o fgetint.o fgeth.o

size:   size.o fgeth.o
	$(CC) $(LDFLAGS) size.o -o size fgeth.o

install: $(DESTDIR)/bin/$(ld) $(DESTDIR)/bin/$(nm)\
		$(DESTDIR)/bin/$(size) $(DESTDIR)/bin/$(strip)\
		$(DESTDIR)/bin/$(lorder)
clean:
	rm -f *.o *.b a.out core ot ld strip nm size

../objdump/stdobj.o:
	$(MAKE) -C ../objdump stdobj.o

$(DESTDIR)/bin/$(ld): ld
	install -s ld $(DESTDIR)/bin/$(ld)

$(DESTDIR)/bin/$(nm): nm
	install -s nm $(DESTDIR)/bin/$(nm)

$(DESTDIR)/bin/$(size): size
	install -s size $(DESTDIR)/bin/$(size)

$(DESTDIR)/bin/$(strip): strip
	install -s strip $(DESTDIR)/bin/$(strip)

###
fgeth.o: fgeth.c ../mesm6/a.out.h
fgetint.o: fgetint.c ../mesm6/a.out.h
fgetsym.o: fgetsym.c ../mesm6/a.out.h
fputh.o: fputh.c ../mesm6/a.out.h
fputsym.o: fputsym.c ../mesm6/a.out.h
ld.o: ld.c ../mesm6/a.out.h
