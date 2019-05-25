OBJ             = p_1.obj \
                  p_7a.obj \
                  p_ef.obj \
                  p_sc.obj \
                  p_sys.obj \
                  paspmd.obj

all:            $(OBJ)

clean:
		rm -f *.b6 *.punch *.obj *.lst *.dis

%.obj:
		./perso-to-obj.py $* 410440 $(subst _,/,$*)
		besm6-objdump $@ > $*.dis
