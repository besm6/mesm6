TESTS           = test1 test2
OBJS            = main.obj abra.obj cadabra.obj

all:            $(TESTS)

# Link three files
test1:          $(OBJS) ../ld always
		../ld -t -t $(OBJS) -o $@

# No object files
test2:          ../ld always
		../ld -t -t -o $@

../ld:
		$(MAKE) -C.. ld

clean:
		rm -f *.b6 *.lst *.punch *.obj $(TESTS)

.SUFFIXES:      .obj .madlen

.madlen.obj:
		../madlen-to-obj.py $<

always:
