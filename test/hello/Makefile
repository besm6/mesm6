TEST    = $(notdir $(CURDIR))
PASCAL  = ../pas-to-oct.py
TTY     = +tty
OPTS    = +load=$(TEST).oct +nowarn3116 +limit=10000
#OPTS   += +trace=output.trace
OPTS   += +utrace=output-full.trace
#OPTS   += +dump

all:    $(TEST).oct

$(TEST).oct: $(TEST).pas $(PASCAL)
	$(PASCAL) $(TEST).pas

clean:
	rm -f $(TEST).b6 $(TEST).lst $(TEST).dump

run:    $(TEST).oct
	$(MAKE) -C../../rtl work
	vsim -c -lib ../../rtl/work -do 'run -all; quit' testbench $(OPTS)

# Disable +tty option when running from test script.
test:   TTY=
test:   run

view:
	gtkwave output.vcd cpu.gtkw &
