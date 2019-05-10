OBJ             = besm6pat1.obj besm6pat2.obj besm6pat3.obj

all:            ../../pascompl $(OBJ)

clean:
		rm -f *.obj

../../pascompl:
		$(MAKE) -C ../..

.pas.obj:
		../../pascompl < $< && mv temp.obj $@

.SUFFIXES:      .pas .obj
