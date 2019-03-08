#
# Cross-assembler for MESM-6
#
SUBDIR		= as ld
DESTDIR		= /usr/local

all:
		for i in ${SUBDIR}; do $(MAKE) DESTDIR=$(DESTDIR) -C $$i; done

install:
		for i in ${SUBDIR}; do $(MAKE) DESTDIR=$(DESTDIR) -C $$i install; done

clean:
		rm -f *~
		for i in ${SUBDIR}; do $(MAKE) -C $$i clean; done
