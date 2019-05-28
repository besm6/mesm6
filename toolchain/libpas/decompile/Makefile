LST             = obprog \
                  p/08 \
                  p/1 \
                  p/2 \
                  p/3 \
                  p/31 \
                  p/4 \
                  p/42 \
                  p/5 \
                  p/51 \
                  p/52 \
                  p/53 \
                  p/6 \
                  p/62 \
                  p/63 \
                  p/6a \
                  p/7a \
                  p/a6 \
                  p/a7 \
                  p/bexf \
                  p/bx \
                  p/cw \
                  p/da \
                  p/di \
                  p/ef \
                  p/el \
                  p/eo \
                  p/er \
                  p/hide \
                  p/ht \
                  p/in \
                  p/is \
                  p/kc \
                  p/ldar \
                  p/md \
                  p/mi \
                  p/nw \
                  p/pa \
                  p/pages \
                  p/pi \
                  p/rc \
                  p/sc \
                  p/sp \
                  p/ss \
                  p/sys \
                  p/tr \
                  p/trpage \
                  p/un \
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
		../objdump/test/madlen-to-obj.py $<
