#
# Use Altera ModelSim:
#   module load modelsim/18.1
#
SOURCES         = mesm6_alu.sv \
                  mesm6_cpu.sv \
                  testbench.sv \
                  imemory.sv \
                  dmemory.sv

INCLUDES        = mesm6_defines.sv

GENDATA         = microcode.v \
                  jumptab16.v \
                  jumptab64.v

all:            work

clean:
		rm -rf *.o *.vcd work

work:           $(SOURCES) $(INCLUDES) $(GENDATA)
		vlib work
		vlog -sv $(SOURCES)

microcode.v:    mesm6_microcode.sv $(INCLUDES)
		vlib work
		vlog -sv mesm6_microcode.sv
		vsim -c -l gen.log -do 'run; quit' gendata +nowarn3116

run:            work
		vsim -c -l run.log -do 'run 0.1us; quit' testbench +dump +nowarn3116 | tee run.log

view:
		gtkwave output.vcd cpu.gtkw &
