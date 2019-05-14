#
# Assembler for micro-BESM
#
DESTDIR = /usr/local
CFLAGS  = -I.. -O0 -Werror #-Wall
LDFLAGS =
VPATH   = ../ld

as      = besm6-as
dis     = besm6-dis

all:    as dis

as:     as.o
	$(MAKE) -C ../ld fputsym.o fputhdr.o fgeth.o fputh.o
	$(CC) $(LDFLAGS) as.o -o as ../ld/fputsym.o ../ld/fputhdr.o ../ld/fgeth.o ../ld/fputh.o

dis:    dis.o
	$(MAKE) -C ../ld fgethdr.o fgeth.o
	$(CC) $(LDFLAGS) dis.o -o dis ../ld/fgethdr.o ../ld/fgeth.o

install: $(DESTDIR)/bin/$(as) $(DESTDIR)/bin/$(dis)

clean:
	rm -f *.o *.b a.out core as dis

$(DESTDIR)/bin/$(as): as
	install -s as $(DESTDIR)/bin/$(as)

$(DESTDIR)/bin/$(dis): dis
	install -s dis $(DESTDIR)/bin/$(dis)

###
as.o: as.c ../mesm6/a.out.h
dis.o: dis.c ../mesm6/a.out.h
#fgeth.o: ../ld/fgeth.c ../mesm6/a.out.h
#fgethdr.o: ../ld/fgethdr.c ../mesm6/a.out.h
#fputh.o: ../ld/fputh.c ../mesm6/a.out.h
#fputhdr.o: ../ld/fputhdr.c ../mesm6/a.out.h
#fputsym.o: ../ld/fputsym.c ../mesm6/a.out.h
