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

OBJ5            = obprog.obj \
                  p_00.obj \
                  p_01.obj \
                  p_03.obj \
                  p_04.obj \
                  p_06.obj \
                  p_08.obj \
                  p_2.obj \
                  p_3.obj \
                  p_31.obj \
                  p_4.obj \
                  p_42.obj \
                  p_5.obj \
                  p_51.obj \
                  p_52.obj \
                  p_53.obj \
                  p_6.obj \
                  p_62.obj \
                  p_63.obj \
                  p_6a.obj \
                  p_a6.obj \
                  p_ca.obj \
                  p_ck.obj \
                  p_cp.obj \
                  p_ds.obj \
                  p_el.obj \
                  p_eo.obj \
                  p_eq.obj \
                  p_in.obj \
                  p_is.obj \
                  p_kc.obj \
                  p_ldar.obj \
                  p_ldarin.obj \
                  p_lngpar.obj \
                  p_mi.obj \
                  p_nw.obj \
                  p_pa.obj \
                  p_pb.obj \
                  p_pi.obj \
                  p_riln.obj \
                  p_rdi.obj \
                  p_rdr.obj \
                  p_rsr.obj \
                  p_ss.obj \
                  p_star.obj \
                  p_un.obj \
                  p_wc.obj \
                  pasmitxt.obj \
                  pastpr.obj

OBJ_DUBNA       = print8.obj \
                  pult.obj \
                  read@.obj \
                  stop@.obj

all:            libpas.a

libpas.a:       $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ_DUBNA) $(OBJ5)
		ar cr $@ $?

tests:          hello.out besm6pat1.out besm6pat2.out besm6pat3.out pascompl.out

hello.out:      libpas.a
		$(MAKE) -C../pascompl/test/hello hello.obj
		../ld/ld ../pascompl/test/hello/hello.obj $? -o $@
		../ld/size $@

pascompl.out:   libpas.a
		$(MAKE) -C../pascompl/test/pascompl pascompl.obj
		../ld/ld ../pascompl/test/pascompl/pascompl.obj $? -o $@
		../ld/size $@

besm6pat1.out:  libpas.a
		../ld/ld ../pascompl/test/accept/besm6pat1.obj $? -o $@
		../ld/size $@

besm6pat2.out:  libpas.a
		../ld/ld ../pascompl/test/accept/besm6pat2.obj $? -o $@
		../ld/size $@

besm6pat3.out:  libpas.a
		../ld/ld ../pascompl/test/accept/besm6pat3.obj $? -o $@
		../ld/size $@

clean:
		rm -f *.out *.b6 *.punch *.obj *.lst *.a

.SUFFIXES:      .obj .madlen

.madlen.obj:
		../ld/madlen-to-obj.py $<
		@rm $*.punch $*.b6
