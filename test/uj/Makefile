TEST    = $(notdir $(CURDIR))
BEMSH   = ../bemsh-to-oct.py
OPTS    = +load=$(TEST).oct +nowarn3116 +limit=200
#OPTS   += +trace=output.trace
OPTS   += +utrace=output-full.trace
#OPTS   += +dump

all:    $(TEST).oct

$(TEST).oct: $(TEST).bemsh $(BEMSH)
	$(BEMSH) $(TEST).bemsh

clean:
	rm -f $(TEST).b6 $(TEST).lst $(TEST).dump

run:    $(TEST).oct
	$(MAKE) -C../../rtl work
	vsim -c -lib ../../rtl/work -do 'run -all; quit' testbench $(OPTS)

test:   run
