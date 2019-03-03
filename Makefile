#
# Use Altera ModelSim:
#   module load modelsim/18.1
#
SOURCES         = mesm6_alu.sv \
                  mesm6_core.sv \
                  mesm6_rom.sv \
                  testbench.sv \
                  imemory.sv \
                  dmemory.sv

all:            work

clean:
		rm -rf *.o *.vcd work

work:           $(SOURCES) mesm6_defines.sv
		vlib work
		vlog -sv $(SOURCES)

run:            work
		vsim -c -l run.log -do 'run 0.1us; quit' testbench +dump +nowarn3116 | tee run.log

view:
		gtkwave output.vcd databus.gtkw
