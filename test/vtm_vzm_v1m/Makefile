TEST    = $(notdir $(CURDIR))
BEMSH   = ../bemsh-to-oct.py

all:    $(TEST).oct

$(TEST).oct: $(TEST).bemsh $(BEMSH)
	$(BEMSH) $(TEST).bemsh

clean:
	rm -f $(TEST).b6 $(TEST).lst $(TEST).dump
