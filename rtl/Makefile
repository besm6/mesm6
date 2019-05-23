#
# Use Altera ModelSim:
#   module load modelsim/18.1
#
SOURCES         = mesm6_alu.sv \
				  mesm6_pic.sv \
				  mesm6_gpio.sv \
				  mesm6_timer.sv \
				  mesm6_mmu.sv \
				  mesm6_cpu.sv \
				  mesm6_uart2.sv \
				  testbench.sv \
				  testbench_pic.sv \
				  testbench_uart.sv \
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
		vsim -c -l run.log -do 'run -all; quit' testbench +load=../test/pult-prog-1/pprog01.oct +dump +nowarn3116 | tee run.log
test_pic: work
		vsim -c -l run.log -do 'run -all; quit' testbench_pic +nowarn3116 | tee run.log
test_uart: work
		vsim -c -l run.log -do 'run -all; quit' testbench_uart +nowarn3116 | tee run.log
view:
		gtkwave output.vcd cpu.gtkw &
