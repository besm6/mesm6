OPTS    = +nowarn3116

all:

clean:
	rm -rf *.vcd

run:
	$(MAKE) -C../../rtl work
	vsim -c -lib ../../rtl/work -l run.log -do 'run -all; quit' testbench_pic $(OPTS)

test:   run

view:
	gtkwave output.vcd cpu.gtkw &
