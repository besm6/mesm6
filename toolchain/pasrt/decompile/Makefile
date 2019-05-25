LST             = p/1 \
                  p/7a \
                  p/a7 \
                  p/bexf \
                  p/bx \
                  p/cw \
                  p/da \
                  p/di \
                  p/ef \
                  p/er \
                  p/hide \
                  p/ht \
                  p/md \
                  p/pages \
                  p/rc \
                  p/sc \
                  p/sp \
                  p/sys \
                  p/tr \
                  p/trpage \
                  p/wi \
                  p/wo \
                  p/wr \
                  p/wx \
                  paseof \
                  pasevn \
                  pashid \
                  pasisoxt \
                  pasnb8 \
                  pasout \
                  paspmd \
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
