TEST            = $(notdir $(CURDIR))
OPTS            = +load=$(TEST).oct +nowarn3116 +limit=2000
OPTS           += +trace=output.trace
#OPTS           += +utrace=output-full.trace
#OPTS           += +dump

all:            $(TEST).oct

$(TEST).oct: $(TEST).obj peek.obj poke.obj
		../../toolchain/ld/objdump $(TEST).obj > $(TEST).obj.dis
		../../toolchain/ld/ld -D1 $(TEST).obj peek.obj poke.obj -lpas -o $(TEST).exe
		../../toolchain/ld/objdump -O $(TEST).exe > $@
		../../toolchain/ld/objdump $(TEST).exe > $(TEST).exe.dis

clean:
		rm -f *.b6 *.lst *.dump *.punch *.obj *.exe *.dis transcript

run:    $(TEST).oct
		$(MAKE) -C../../rtl work
		vsim -c -lib ../../rtl/work -do 'run -all; quit' testbench $(OPTS)

view:
		gtkwave output.vcd cpu.gtkw &

$(TEST).obj:    $(TEST).pas ../../toolchain/ld/pascal-to-obj.py
		../../toolchain/pascompl/pascompl $(TEST).pas $(TEST).obj

peek.obj:       peek.madlen ../../toolchain/ld/madlen-to-obj.py
		../../toolchain/ld/madlen-to-obj.py peek.madlen

poke.obj:       poke.madlen ../../toolchain/ld/madlen-to-obj.py
		../../toolchain/ld/madlen-to-obj.py poke.madlen
