TEST    = $(notdir $(CURDIR))
BEMSH   = ../bemsh-to-oct.py

all:    $(TEST).oct

$(TEST).oct: $(TEST).bemsh $(BEMSH)
	$(BEMSH) $(TEST).bemsh

clean:
	rm -f $(TEST).b6 $(TEST).lst $(TEST).dump

run:    $(TEST).oct
	$(MAKE) -C../../rtl work
	vsim -c -lib ../../rtl/work -do 'run -all; quit' testbench +load=$(TEST).oct +nowarn3116 #+dump

test:   run

view:
	gtkwave output.vcd cpu.gtkw &
