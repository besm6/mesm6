OBJS            = algol.obj empty.obj bemsh.obj objfmt.obj ftn.obj \
                  forex.obj fortran.obj pascal.obj pascompl.obj

all:            $(OBJS)
		../dis algol.obj    > algol.dis
		../dis empty.obj    > empty.dis
		../dis bemsh.obj    > bemsh.dis
		../dis objfmt.obj   > disfmt.dis
		../dis fortran.obj  > fortran.dis
		../dis ftn.obj      > ftn.dis
		../dis forex.obj    > forex.dis
		../dis pascal.obj   > pascal.dis
		../dis pascompl.obj > pascompl.dis

algol.obj:      algol.alg algol-to-obj.py
		./algol-to-obj.py algol.alg

empty.obj:      empty.madlen madlen-to-obj.py
		./madlen-to-obj.py empty.madlen

bemsh.obj:      bemsh.bemsh bemsh-to-obj.py
		./bemsh-to-obj.py bemsh.bemsh

objfmt.obj:     objfmt.madlen madlen-to-obj.py
		./madlen-to-obj.py objfmt.madlen

ftn.obj:        fortran.f ftn-to-obj.py
		./ftn-to-obj.py fortran.f
		mv fortran.obj $@

forex.obj:      fortran.f forex-to-obj.py
		./forex-to-obj.py fortran.f
		mv fortran.obj $@

fortran.obj:    fortran.f fortran-to-obj.py
		./fortran-to-obj.py fortran.f

pascal.obj:     pascal.pas pascal-to-obj.py
		./pascal-to-obj.py pascal.pas

pascompl.obj:   ../../pascompl/test/pascompl/pascompl.pas pascal-to-obj.py
		./pascal-to-obj.py ../../pascompl/test/pascompl/pascompl.pas

clean:
		rm -f *.b6 *.lst *.punch *.obj *.dis
