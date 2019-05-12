LST             = besm6pat1.lst besm6pat2.lst besm6pat3.lst

all:            ../../pascompl ../../../dtran/dtran ../madlen-run.py $(LST)

clean:
		rm -f *.obj *.madlen *.lst *.b6

../../pascompl:
		$(MAKE) -C ../..

../../../dtran/dtran:
		$(MAKE) -C ../../../dtran

.pas.obj:
		../../pascompl -t- $< $@

.obj.madlen:
		../../../dtran/dtran -e $< > $@

.madlen.lst:
		../madlen-run.py $< > $@

.SUFFIXES:      .pas .obj .madlen .lst

.PRECIOUS:      %.madlen %.obj
