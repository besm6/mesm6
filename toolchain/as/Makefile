#
# Assembler for micro-BESM
#
DESTDIR = /usr/local
CFLAGS  = -I.. -O0 -Werror #-Wall
LDFLAGS =
VPATH   = ../ld

as      = mkb-as
dis     = mkb-dis

all:    as dis

as:     as.o fputsym.o fputhdr.o fgeth.o fputh.o
	$(CC) $(LDFLAGS) as.o -o as fputsym.o fputhdr.o fgeth.o fputh.o

dis:    dis.o fgethdr.o fgeth.o
	$(CC) $(LDFLAGS) dis.o -o dis fgethdr.o fgeth.o

install: $(DESTDIR)/bin/$(as) $(DESTDIR)/bin/$(dis)

clean:
	rm -f *.o *.b a.out core as dis

$(DESTDIR)/bin/$(as): as
	install -s as $(DESTDIR)/bin/$(as)

$(DESTDIR)/bin/$(dis): dis
	install -s dis $(DESTDIR)/bin/$(dis)

###
as.o: as.c ../microbesm/a.out.h
fputsym.o: ../ld/fputsym.c ../microbesm/a.out.h
fputhdr.o: ../ld/fputhdr.c ../microbesm/a.out.h
fgeth.o: ../ld/fgeth.c ../microbesm/a.out.h
fputh.o: ../ld/fputh.c ../microbesm/a.out.h
dis.o: dis.c ../microbesm/a.out.h
fgethdr.o: ../ld/fgethdr.c ../microbesm/a.out.h
