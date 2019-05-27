OBJ1            = p_1.obj \
                  p_7a.obj \
                  p_ef.obj \
                  p_sc.obj \
                  p_sys.obj \
                  paspmd.obj

OBJ2            = p_bexf.obj \
                  p_bx.obj \
                  p_cw.obj \
                  p_da.obj \
                  p_di.obj \
                  p_er.obj \
                  p_hide.obj \
                  p_ht.obj \
                  p_md.obj \
                  p_trpage.obj \
                  p_wi.obj \
                  p_wo.obj \
                  p_wr.obj \
                  p_wx.obj \
                  paseof.obj \
                  pasout.obj

OBJ3            = p_a7.obj \
                  p_pages.obj \
                  p_rc.obj \
                  p_sp.obj \
                  p_tr.obj \
                  pasevn.obj \
                  pasisoxt.obj \
                  pasnb8.obj

OBJ4            = pashid.obj

OBJ_DUBNA       = print8.obj \
                  pult.obj \
                  read@.obj \
                  stop@.obj

all:            $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ_DUBNA)

hello.out:      $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ_DUBNA)
		../ld/ld ../pascompl/test/hello/hello.obj $? -o $@

clean:
		rm -f a.out *.b6 *.punch *.obj *.lst

.SUFFIXES:      .obj .madlen

.madlen.obj:
		../objdump/test/madlen-to-obj.py $<
		@rm $*.punch $*.b6
