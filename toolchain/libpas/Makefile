DESTDIR         = /usr/local/lib/besm6

OBJ             = obprog.obj p_00.obj p_01.obj p_02.obj p_03.obj p_04.obj \
                  p_06.obj p_07.obj p_08.obj p_0п.obj p_1.obj p_2.obj p_31.obj \
                  p_3.obj p_41.obj p_42.obj p_4.obj p_51.obj p_52.obj p_53.obj \
                  p_5.obj p_61.obj p_62.obj p_63.obj p_64.obj p_6a.obj p_6.obj \
                  p_7a.obj p_a6.obj p_a7.obj p_bexf.obj p_bx.obj p_ca.obj \
                  p_ci.obj p_ck.obj p_cp.obj p_cw.obj p_da.obj p_dd.obj p_di.obj \
                  p_ds.obj p_ea.obj p_ef.obj p_el.obj p_eo.obj p_eq.obj p_er.obj \
                  p_hide.obj p_ht.obj p_in.obj p_is.obj p_kc.obj p_ldarin.obj \
                  p_ldar.obj p_lngpar.obj p_lv.obj p_md.obj p_mf.obj p_mi.obj \
                  p_nn.obj p_nw.obj p_pages.obj p_pa.obj p_pb.obj p_pi.obj \
                  p_ra7.obj p_ra.obj p_rc.obj p_rdi.obj p_rdr.obj p_re.obj \
                  p_riln.obj p_ri.obj p_rr.obj p_rsr.obj p_sc.obj p_sp.obj \
                  p_ss.obj p_star.obj p_sys.obj p_tr.obj p_trpage.obj p_un.obj \
                  p_wa.obj p_wc.obj p_wi.obj p_wo.obj p_wr.obj p_wx.obj \
                  paseof.obj pasevn.obj pashid.obj pasisoxt.obj pasmitxt.obj \
                  pasnb8.obj pasout.obj paspmd.obj pastpr.obj

OBJ_DUBNA       = print8.obj pult.obj read@.obj stop@.obj

all:            libpas.a

install:        $(DESTDIR)/libpas.a

$(DESTDIR)/libpas.a: libpas.a
		@install -d $(DESTDIR)
		install -m 444 libpas.a $(DESTDIR)/libpas.a

libpas.a:       $(OBJ) $(OBJ_DUBNA)
		@rm -f $@
		ar cr $@ $(OBJ) $(OBJ_DUBNA)

tests:          hello.out besm6pat1.out besm6pat2.out besm6pat3.out pascompl.out
		../ld/size hello.obj hello.out
		../ld/size pascompl.obj pascompl.out
		../ld/size besm6pat1.obj besm6pat1.out
		../ld/size besm6pat2.obj besm6pat2.out
		../ld/size besm6pat3.obj besm6pat3.out

hello.out:      hello.obj libpas.a
		../ld/ld hello.obj libpas.a -o $@

pascompl.out:   pascompl.obj libpas.a
		../ld/ld pascompl.obj libpas.a -o $@

besm6pat1.out:  besm6pat1.obj libpas.a
		../ld/ld besm6pat1.obj libpas.a -o $@

besm6pat2.out:  besm6pat2.obj libpas.a
		../ld/ld besm6pat2.obj libpas.a -o $@

besm6pat3.out:  besm6pat3.obj libpas.a
		../ld/ld besm6pat3.obj libpas.a -o $@

hello.obj:      ../pascompl/test/hello/hello.pas
		$(MAKE) -C../pascompl/test/hello hello.obj
		cp -a ../pascompl/test/hello/hello.obj $@

pascompl.obj:   ../pascompl/test/pascompl/pascompl.pas
		$(MAKE) -C../pascompl/test/pascompl pascompl.obj
		cp -a ../pascompl/test/pascompl/pascompl.obj $@

besm6pat1.obj:  ../pascompl/test/accept/besm6pat1.pas
		$(MAKE) -C../pascompl/test/accept besm6pat1.obj
		cp -a ../pascompl/test/accept/besm6pat1.obj $@

besm6pat2.obj:  ../pascompl/test/accept/besm6pat2.pas
		$(MAKE) -C../pascompl/test/accept besm6pat2.obj
		cp -a ../pascompl/test/accept/besm6pat2.obj $@

besm6pat3.obj:  ../pascompl/test/accept/besm6pat3.pas
		$(MAKE) -C../pascompl/test/accept besm6pat3.obj
		cp -a ../pascompl/test/accept/besm6pat3.obj $@

clean:
		rm -f *.out *.b6 *.punch *.obj *.lst *.a

.SUFFIXES:      .obj .madlen

.madlen.obj:
		../ld/madlen-to-obj.py $<
		@rm $*.punch $*.b6
