TEST    = $(notdir $(CURDIR))
TTY     = +tty
OPTS    = +load=$(TEST).oct +nowarn3116 +limit=2000
OPTS   += +trace=output.trace
#OPTS   += +utrace=output-full.trace
#OPTS   += +dump

all:    $(TEST).oct

$(TEST).oct: $(TEST).obj
	../../toolchain/ld/ld -D1 $(TEST).obj -lpas -o $(TEST).exe
	../../toolchain/ld/objdump -O $(TEST).exe > $@
	../../toolchain/ld/objdump $(TEST).exe > $(TEST).dis

clean:
	rm -f *.b6 *.lst *.dump *.punch *.obj *.exe *.dis transcript

run:    $(TEST).oct
	$(MAKE) -C../../rtl work
	vsim -c -lib ../../rtl/work -do 'run -all; quit' testbench $(OPTS)

# Disable +tty option when running from test script.
test:   TTY=
test:   run

view:
	gtkwave output.vcd cpu.gtkw &

$(TEST).obj:    $(TEST).pas ../../toolchain/ld/pascal-to-obj.py
		../../toolchain/pascompl/pascompl $(TEST).pas $(TEST).obj
