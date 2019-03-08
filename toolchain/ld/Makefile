#
# Linker, archiver and other object file utilities
#
DESTDIR = /usr/SVSB
CFLAGS  = -I.. -Werror
LDFLAGS =

ld      = mkb-ld
ar      = mkb-ar
strip   = mkb-strip
nm      = mkb-nm
size    = mkb-size
lorder  = mkb-lorder
ranlib  = mkb-ranlib

all:    ld #ar strip nm size ranlib

ld:     ld.o fgetarhdr.o fgetint.o fgetsym.o fputsym.o fgethdr.o fputhdr.o\
		fgeth.o fputh.o fgetran.o
	$(CC) $(LDFLAGS) ld.o -o ld fgetarhdr.o fgetint.o fgetsym.o fputsym.o\
		fgethdr.o fputhdr.o fgeth.o fputh.o fgetran.o

ar:     ar.o getarhdr.o putarhdr.o getint.o putint.o
	$(CC) $(LDFLAGS) ar.o -o ar getarhdr.o putarhdr.o getint.o putint.o

ranlib: ranlib.o getarhdr.o putarhdr.o fgetarhdr.o fgeth.o fgetsym.o\
		fputran.o fputh.o fgethdr.o
	$(CC) $(LDFLAGS) ranlib.o -o ranlib getarhdr.o putarhdr.o fgetarhdr.o\
		fgeth.o fgetsym.o fputran.o fputh.o fgethdr.o

strip:  strip.o fgethdr.o fputhdr.o fgeth.o fputh.o
	$(CC) $(LDFLAGS) strip.o -o strip fgethdr.o fputhdr.o fgeth.o fputh.o

nm:     nm.o fgetsym.o fgethdr.o fgetarhdr.o fgetint.o fgeth.o
	$(CC) $(LDFLAGS) nm.o -o nm fgetsym.o fgethdr.o fgetarhdr.o fgetint.o\
		fgeth.o

size:   size.o fgethdr.o fgeth.o
	$(CC) $(LDFLAGS) size.o -o size fgethdr.o fgeth.o

install: $(DESTDIR)/bin/$(ld) $(DESTDIR)/bin/$(ar) $(DESTDIR)/bin/$(nm)\
		$(DESTDIR)/bin/$(size) $(DESTDIR)/bin/$(strip)\
		$(DESTDIR)/bin/$(lorder) $(DESTDIR)/bin/$(ranlib)
clean:
	rm -f *.o *.b a.out core ot ld ar strip nm size ranlib

$(DESTDIR)/bin/$(ld): ld
	install -s ld $(DESTDIR)/bin/$(ld)

$(DESTDIR)/bin/$(ranlib): ranlib
	install -s ranlib $(DESTDIR)/bin/$(ranlib)

$(DESTDIR)/bin/$(ar): ar
	install -s ar $(DESTDIR)/bin/$(ar)

$(DESTDIR)/bin/$(nm): nm
	install -s nm $(DESTDIR)/bin/$(nm)

$(DESTDIR)/bin/$(size): size
	install -s size $(DESTDIR)/bin/$(size)

$(DESTDIR)/bin/$(strip): strip
	install -s strip $(DESTDIR)/bin/$(strip)

$(DESTDIR)/bin/$(lorder): lorder.sh
	install lorder.sh $(DESTDIR)/bin/$(lorder)

###
