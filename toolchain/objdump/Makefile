#
# objdump: show contents of object files for BESM-6
#
DESTDIR = /usr/local
CFLAGS  = -I.. -O0 -Werror -Wall
LDFLAGS =
OBJ     = objdump.o stdobj.o

objdump = besm6-objdump

all:    objdump

objdump: $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o objdump

install: $(DESTDIR)/bin/$(objdump)

clean:
	rm -f *.o *.b a.out core objdump

$(DESTDIR)/bin/$(objdump): objdump
	install -s objdump $(DESTDIR)/bin/$(objdump)

###
objdump.o: objdump.c ../mesm6/a.out.h
