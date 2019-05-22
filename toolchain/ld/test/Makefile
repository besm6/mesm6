TESTS           = test1
OBJS            = main.obj abra.obj cadabra.obj

all:            $(TESTS)

test1:          $(OBJS) ../ld
		../ld -t -t $(OBJS) -o $@

clean:
		rm -f *.b6 *.lst *.punch *.obj $(TESTS)

.SUFFIXES:      .obj .madlen

.madlen.obj:
		../../objdump/test/madlen-to-obj.py $<
