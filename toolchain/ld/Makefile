#
# Linker, archiver and other object file utilities
#
DESTDIR = /usr/SVSB
CFLAGS  = -I.. -I../objdump -Werror -Wall
LDFLAGS =

ld      = mkb-ld
ar      = mkb-ar
strip   = mkb-strip
nm      = mkb-nm
size    = mkb-size
lorder  = mkb-lorder
ranlib  = mkb-ranlib

all:    ld #ar strip nm size ranlib

ld:     ld.o ../objdump/stdobj.o fgetarhdr.o fgetint.o fgetsym.o fputsym.o \
		fgeth.o fputh.o
	$(CC) $(LDFLAGS) ld.o -o ld ../objdump/stdobj.o fgetarhdr.o fgetint.o \
		fgetsym.o fputsym.o fgeth.o fputh.o

ar:     ar.o getarhdr.o putarhdr.o getint.o putint.o
	$(CC) $(LDFLAGS) ar.o -o ar getarhdr.o putarhdr.o getint.o putint.o

strip:  strip.o fgeth.o fputh.o
	$(CC) $(LDFLAGS) strip.o -o strip fgeth.o fputh.o

nm:     nm.o fgetsym.o fgetarhdr.o fgetint.o fgeth.o
	$(CC) $(LDFLAGS) nm.o -o nm fgetsym.o fgetarhdr.o fgetint.o fgeth.o

size:   size.o fgeth.o
	$(CC) $(LDFLAGS) size.o -o size fgeth.o

install: $(DESTDIR)/bin/$(ld) $(DESTDIR)/bin/$(ar) $(DESTDIR)/bin/$(nm)\
		$(DESTDIR)/bin/$(size) $(DESTDIR)/bin/$(strip)\
		$(DESTDIR)/bin/$(lorder) $(DESTDIR)/bin/$(ranlib)
clean:
	rm -f *.o *.b a.out core ot ld ar strip nm size ranlib

../objdump/stdobj.o:
	$(MAKE) -C ../objdump stdobj.o

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
fgetarhdr.o: fgetarhdr.c ../mesm6/a.out.h ../mesm6/ar.h
fgeth.o: fgeth.c ../mesm6/a.out.h
fgetint.o: fgetint.c ../mesm6/a.out.h
fgetran.o: fgetran.c ../mesm6/a.out.h ../mesm6/ranlib.h
fgetsym.o: fgetsym.c ../mesm6/a.out.h
fputh.o: fputh.c ../mesm6/a.out.h
fputsym.o: fputsym.c ../mesm6/a.out.h
ld.o: ld.c ../mesm6/a.out.h ../mesm6/ar.h ../mesm6/ranlib.h
