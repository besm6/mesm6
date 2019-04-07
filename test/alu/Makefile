TEST    = $(notdir $(CURDIR))
BEMSH   = ../bemsh-to-oct.py
OPTS    = +load=$(TEST).oct +nowarn3116 +limit=12000000
#OPTS   += +trace=output.trace
OPTS   += +utrace=output-full.trace
#OPTS   += +dump

all:    $(TEST).oct

$(TEST).oct: $(TEST).bemsh $(BEMSH)
	$(BEMSH) $(TEST).bemsh "1,1, 032000,032007, 032012,034407, 035052,035156, 035207,035760, 036022,036121, 036125,036427"

$(TEST).bemsh: test_alu.b6 simh-to-bemsh.py
	./simh-to-bemsh.py test_alu.b6 > $(TEST).bemsh
clean:
	rm -f $(TEST).b6 $(TEST).lst $(TEST).dump

run:    $(TEST).oct
	$(MAKE) -C../../rtl work
	vsim -c -lib ../../rtl/work -do 'run -all; quit' testbench $(OPTS)

test:   run

view:
	gtkwave output.vcd cpu.gtkw &
