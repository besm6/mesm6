TESTS           = test1
OBJS            = main.obj abra.obj cadabra.obj

all:            $(TESTS)

test1:          $(OBJS) ../ld always
		../ld -t -t $(OBJS) -o $@

clean:
		rm -f *.b6 *.lst *.punch *.obj $(TESTS)

.SUFFIXES:      .obj .madlen

.madlen.obj:
		../madlen-to-obj.py $<

always:
