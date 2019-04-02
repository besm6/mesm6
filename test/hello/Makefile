TEST    = $(notdir $(CURDIR))
PASCAL  = ../pas-to-oct.py

all:    $(TEST).oct

$(TEST).oct: $(TEST).pas $(PASCAL)
	$(PASCAL) $(TEST).pas

clean:
	rm -f $(TEST).b6 $(TEST).lst $(TEST).dump

run:    $(TEST).oct
	$(MAKE) -C../../rtl work
	vsim -c -lib ../../rtl/work -do 'run -all; quit' testbench +load=$(TEST).oct +nowarn3116 +limit=3000 #+dump

view:
	gtkwave output.vcd cpu.gtkw &
