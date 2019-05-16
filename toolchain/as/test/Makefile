OBJS            = algol.obj empty.obj bemsh.obj madlen.obj ftn.obj \
                  forex.obj fortran.obj pascal.obj pascompl.obj

all:            $(OBJS)
		../dis -d algol.obj    > algol.dis
		../dis -d empty.obj    > empty.dis
		../dis -d bemsh.obj    > bemsh.dis
		../dis -d madlen.obj   > madlen.dis
		../dis -d fortran.obj  > fortran.dis
		../dis -d ftn.obj      > ftn.dis
		../dis -d forex.obj    > forex.dis
		../dis -d pascal.obj   > pascal.dis
		../dis -d pascompl.obj > pascompl.dis

algol.obj:      algol.alg algol-to-obj.py
		./algol-to-obj.py algol.alg

empty.obj:      empty.madlen madlen-to-obj.py
		./madlen-to-obj.py empty.madlen

bemsh.obj:      bemsh.bemsh bemsh-to-obj.py
		./bemsh-to-obj.py bemsh.bemsh

madlen.obj:     madlen.madlen madlen-to-obj.py
		./madlen-to-obj.py madlen.madlen

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
		rm -f *.b6 *.lst *.punch *.obj pascompl.dis
