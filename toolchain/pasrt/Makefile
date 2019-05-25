OBJ             = p_1.obj

all:            $(OBJ)

clean:
		rm -f *.b6 *.punch *.obj

.SUFFIXES:      .obj .madlen

.madlen.obj:
		../objdump/test/madlen-to-obj.py $<
