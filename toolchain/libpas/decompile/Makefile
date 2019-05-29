LST             = obprog \
                  p/00 \
                  p/01 \
                  p/02 \
                  p/03 \
                  p/04 \
                  p/06 \
                  p/07 \
                  p/08 \
                  p/0п \
                  p/1 \
                  p/2 \
                  p/3 \
                  p/31 \
                  p/4 \
                  p/41 \
                  p/42 \
                  p/5 \
                  p/51 \
                  p/52 \
                  p/53 \
                  p/6 \
                  p/61 \
                  p/62 \
                  p/63 \
                  p/64 \
                  p/6a \
                  p/7a \
                  p/a6 \
                  p/a7 \
                  p/bexf \
                  p/bx \
                  p/ca \
                  p/ci \
                  p/ck \
                  p/cp \
                  p/cw \
                  p/da \
                  p/dd \
                  p/di \
                  p/ds \
                  p/ea \
                  p/ef \
                  p/el \
                  p/eo \
                  p/eq \
                  p/er \
                  p/hide \
                  p/ht \
                  p/in \
                  p/is \
                  p/kc \
                  p/ldar \
                  p/ldarin \
                  p/lngpar \
                  p/lv \
                  p/md \
                  p/mf \
                  p/mi \
                  p/nn \
                  p/nw \
                  p/pa \
                  p/pages \
                  p/pb \
                  p/pi \
                  p/ra \
                  p/ra7 \
                  p/rc \
                  p/rdi \
                  p/rdr \
                  p/re \
                  p/ri \
                  p/riln \
                  p/rr \
                  p/rsr \
                  p/sc \
                  p/sp \
                  p/ss \
                  p/star \
                  p/sys \
                  p/tr \
                  p/trpage \
                  p/un \
                  p/wa \
                  p/wc \
                  p/wi \
                  p/wo \
                  p/wr \
                  p/wx \
                  paseof \
                  pasevn \
                  pashid \
                  pasisoxt \
                  pasmitxt \
                  pasnb8 \
                  pasout \
                  paspmd \
                  pastpr \
                  pult \
                  stop\*
#OBJ             = p_1.obj

all:            $(addsuffix .lst, $(subst /,_,$(LST))) $(OBJ)

%.lst:
		./dtran.py $(subst _,/,$*)

clean:
		rm -f *.b6

.SUFFIXES:      .obj .madlen

.madlen.obj:
		../ld/madlen-to-obj.py $<
