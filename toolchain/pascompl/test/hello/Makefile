
all:            hello.pas
		../pas-run.py hello.pas

clean:
		rm -f *.obj

../../pascompl:
		$(MAKE) -C ../..

.pas.obj:
		../../pascompl $< $@

.obj.madlen:
		mesm6-dtran -e $< > $@

.SUFFIXES:      .pas .obj .madlen
