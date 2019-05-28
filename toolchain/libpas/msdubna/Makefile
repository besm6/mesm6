OBJ             = print8.obj \
                  pult.obj \
                  read@.obj \
                  stop@.obj

all:            $(OBJ)

clean:
		rm -f a.out *.b6 *.punch *.obj *.lst

.SUFFIXES:      .obj .madlen

.madlen.obj:
		../../ld/madlen-to-obj.py $<
