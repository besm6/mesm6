OBJ             = p_1.obj \
                  p_7a.obj \
                  p_ef.obj \
                  p_sc.obj \
                  p_sys.obj \
                  paspmd.obj

all:            $(OBJ)

clean:
		rm -f *.b6 *.punch *.obj *.lst

.SUFFIXES:      .obj .madlen

.madlen.obj:
		../objdump/test/madlen-to-obj.py $<
