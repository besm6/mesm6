//
// Run the processor with memory attached.
//
// Copyright (c) 2019 Serge Vakulenko
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
`default_nettype none
`include "mesm6_defines.sv"

module testbench();

// Global time parameters.
timeunit 1ns / 1ps;

// Inputs.
// Clock, reset, interrupt rquest
logic        clk, reset, irq;

// Instruction memory signals.
logic        ibus_rd;       // fetch request
logic [14:0] ibus_addr;     // address
logic [47:0] ibus_input;    // instruction word from memory
logic        ibus_done;     // operation completed

// Data memory signals.
logic        dbus_rd;       // read request
logic        dbus_wr;       // write request
logic [14:0] dbus_addr;     // address
logic [47:0] dbus_output;   // data to memory
logic [47:0] dbus_input;    // data from memory
logic        dbus_done;     // operation completed

// Instantiate CPU.
mesm6_core cpu(
    clk,                    // clock on rising edge
    reset,                  // reset on rising edge
    irq,                    // interrupt request

    // Instruction memory bus.
    ibus_rd,                // request instruction fetch
    ibus_addr,              // memory address
    ibus_input,             // instruction word read
    ibus_done,              // memory operation completed

    // Data memory bus.
    dbus_rd,                // request data read
    dbus_wr,                // request data write
    dbus_addr,              // memory address
    dbus_output,            // data written
    dbus_input,             // data read
    dbus_done               // memory operation completed
);

// Instruction memory.
imemory prom(
    clk,                    // clock on rising edge
    ibus_addr,              // memory address
    ibus_rd,                // read request
    ibus_input,             // data from memory
    ibus_done               // operation completed
);

// Data memory.
dmemory ram(
    clk,                    // clock on rising edge
    dbus_addr,              // memory address
    dbus_rd,                // read request
    dbus_wr,                // write request
    dbus_output,            // data to memory
    dbus_input,             // data from memory
    dbus_done               // operation completed
);

string tracefile = "output.trace";
int limit;
int tracelevel;             // Trace level
int tracefd;                // Trace file descriptor
time ctime;                 // Current time
longint uinstr_count;
longint instr_count;                    // Instruction and micro-instruction counters
bit old_reset = 0;                      // Previous state of reset

//
// Last fetch address
//
logic [`UPC_BITS-1:0] upc;  // last PC
logic [`UOP_BITS-1:0] uop;  // last micro-instruction
logic                 busy; // last busy signal

//
// Import standard C function gettimeofday().
//
`ifdef XILINX_SIMULATOR
typedef struct { longint sec, usec; } timeval_t;
`else
typedef struct { int sec, usec; } timeval_t;
`endif
import "DPI-C" function void gettimeofday(inout timeval_t tv, input chandle tz);

timeval_t t0;               // Start time of simulation

//
// Generate clock 500MHz.
//
always #0.5 clk = ~clk;

//
// Main loop.
//
initial begin
    $display("");
    $display("--------------------------------");

    // Dump waveforms.
    if ($test$plusargs("dump")) begin
        $dumpfile("output.vcd");
        $dumpvars();
    end

    // Enable detailed instruction trace to file.
    tracelevel = 2;
    $display("Generate trace file %0S", tracefile);
    tracefd = $fopen(tracefile, "w");

    // Limit the simulation by specified number of cycles.
    if (! $value$plusargs("limit=%d", limit)) begin
        // Default limit value.
        limit = 100000;
        $display("Limit: %0d", limit);
        $fdisplay(tracefd, "Limit: %0d", limit);
    end

    // Start with reset active
    clk = 1;
    reset = 1;
    irq = 0;

    // Hold reset for a while.
    #1 reset = 0;

    // Run until limit.
    gettimeofday(t0, null);
    #limit begin
        message("Time Limit Exceeded");
        $finish;
    end
end

//
// Print a message to stdout and trace file
//
task message(input string msg);
    $display("*** %s", msg);
    $fdisplay(tracefd, "(%0d) *** %s", ctime, msg);
endtask

// Get time at the rising edge of the clock.
always @(posedge clk) begin
    ctime = $time;
    upc = cpu.upc;
    uop = cpu.uop;
    busy = cpu.busy;
    if (~reset & ~cpu.busy)
        uinstr_count++;
end

//
// Tracer
//

// At negative clock edge, when all the signals are quiet,
// print the state of the processor.
always @(negedge clk) begin
    if (tracefd) begin
        if (reset) begin
            if (!old_reset) begin               // Reset
                $fdisplay(tracefd, "(%0d) *** Reset", ctime);
                old_reset = 1;
            end
        end else if (old_reset) begin                // Clear reset
            $fdisplay(tracefd, "(%0d) *** Clear reset", ctime);
            old_reset = 0;
        end else begin
            if (tracelevel > 1) begin
                // Print last executed micro-instruction
                print_uop();
            end

            // Print changed architectural state
            print_changed_regs();

`ifdef notdef
            // Print transactions on external bus
            print_ext_bus();

            // Print BESM instruction
            if (!reset)
                print_insn();

            if (int_flag_x)
                $fdisplay(tracefd, "(%0d) *** Interrupt #%0d", ctime, cpu.int_vect);
`endif

            // Get data from fetch stage
            //int_flag_x = cpu.int_flag;
            //tkk_x = cpu.tkk;
            //cb_x = cpu.cb;

            //->instruction_retired;
        end
    end

`ifdef notdef
    if (!reset && $isunknown(cpu.opcode)) begin
        $display("(%0d) Unknown instruction: cpu.opcode=%h", ctime, cpu.opcode);
        if (tracefd)
            $fdisplay(tracefd, "(%0d) *** Unknown instruction: cpu.opcode=%h", ctime, cpu.opcode);
        terminate("Fatal Error!");
    end
`endif

    if ((cpu.dbus_read | cpu.dbus_write) && $isunknown(cpu.dbus_addr)) begin
        $display("(%0d) Unknown address: dbus_addr=%h", ctime, cpu.dbus_addr);
        if (tracefd)
            $fdisplay(tracefd, "(%0d) *** Unknown address: dbus_addr=%h", ctime, cpu.dbus_addr);
        terminate("Fatal Error!");
    end

    if (cpu.ibus_fetch && $isunknown(cpu.ibus_addr)) begin
        $display("(%0d) Unknown address: ibus_addr=%h", ctime, cpu.ibus_addr);
        if (tracefd)
            $fdisplay(tracefd, "(%0d) *** Unknown address: ibus_addr=%h", ctime, cpu.ibus_addr);
        terminate("Fatal Error!");
    end

    //TODO
    //if (!cpu.run) begin
    //    cpu_halted();
    //end
end

//
// Print statistics and finish the simulation.
//
task terminate(input string message);
    timeval_t t1;
    longint usec;

    gettimeofday(t1, null);

    if (message != "")
        $display("\n----- %s -----", message);
    if (tracefd)
        $fdisplay(tracefd, "\n----- %s -----", message);

    usec = (t1.usec - t0.usec) + (t1.sec - t0.sec) * 1000000;
    $display("   Elapsed time: %0d seconds", usec / 1000000);
    $display("      Simulated: %0d instructions, %0d micro-instructions",
        instr_count, uinstr_count);
    if (usec > 0)
        $display("Simulation rate: %.1f instructions/sec, %.0f micro-instructions/sec",
            1000000.0 * instr_count / usec,
            1000000.0 * uinstr_count / usec);

    if (tracefd) begin
        $fdisplay(tracefd, "   Elapsed time: %0d seconds", usec / 1000000);
        $fdisplay(tracefd, "      Simulated: %0d instructions, %0d micro-instructions",
            instr_count, uinstr_count);
        if (usec > 0)
            $fdisplay(tracefd, "Simulation rate: %.1f instructions/sec, %.0f micro-instructions/sec",
                1000000.0 * instr_count / usec,
                1000000.0 * uinstr_count / usec);
    end

    $finish;
endtask

//
// Print micro-instruction.
//
task print_uop();
    static string op_name[16] = '{
        0: "A",    1: "B",     2: "A+B",  3: "A+Boff",
        4: "A&B",  5: "A|B",   6: "~A",   7: "?7",
        8: "?8",   9: "?9",    10:"?10",  11:"?11",
        12:"?12",  13:"?13",   14:"?14",  15:"?15",
        default: "???"
    };
    static string acc_name[8] = '{
        0: "ALU",  1: "MEM",   2: "REG",  3: "RR",
        4: "Y",    5: "?5",    6: "?6",   7: "?7"
    };
    static string md_name[8] = '{
        0: "PC",     1: "A",       2: "ALU",  3: "REG",
        4: "RPLUS1", 5: "RMINUS1", 6: "VA",   7: "UA"
    };
    static string mw_name[4] = '{
        0: "I",      1: "IMM",     2: "VA",  3: "UA"
    };
    static string mr_name[4] = '{
        0: "I",      1: "IMM",     2: "VA",  3: "UA"
    };
    static string pc_name[8] = '{
        0: "REG",   1: "IMM", 2: "VA", 3: "UA",
        4: "PLUS1", 5: "?5",  6: "?6", 7: "?7"
    };

    logic [`UPC_BITS-1:0] imm;
    logic [5:0] alu_op;
    logic [2:0] sel_acc;
    logic [2:0] sel_md;
    logic [1:0] sel_mw;
    logic [1:0] sel_mr;
    logic [2:0] sel_pc;
    logic       w_m;
    logic       m_use;
    logic       sel_addr;
    logic       sel_j_add;
    logic       sel_c_mem;
    logic       cond_op_not_cached;
    logic       fetch;
    logic       w_opcode;
    logic       decode;
    logic       mem_r;
    logic       mem_w;
    logic       w_a;
    logic       w_c;
    logic       w_y;
    logic       cond_a_zero;
    logic       cond_a_neg;
    logic       branch;
    logic       clear_c;
    logic       enter_interrupt;
    logic       exit_interrupt;
    logic       w_pc;

    assign imm                = uop[`P_IMM+`UPC_BITS-1:`P_IMM];
    assign alu_op             = uop[`P_ALU+5:`P_ALU];
    assign sel_acc            = uop[`P_SEL_ACC+2:`P_SEL_ACC];
    assign sel_md             = uop[`P_SEL_MD+2:`P_SEL_MD];
    assign sel_mw             = uop[`P_SEL_MW+1:`P_SEL_MW];
    assign sel_mr             = uop[`P_SEL_MR+1:`P_SEL_MR];
    assign sel_pc             = uop[`P_SEL_PC+2:`P_SEL_PC];
    assign w_m                = uop[`P_W_M];
    assign m_use              = uop[`P_M_USE];
    assign sel_addr           = uop[`P_SEL_ADDR];
    assign sel_j_add          = uop[`P_SEL_J_ADD];
    assign sel_c_mem          = uop[`P_SEL_C_MEM];
    assign cond_op_not_cached = uop[`P_OP_NOT_CACHED];
    assign fetch              = uop[`P_FETCH];
    assign w_opcode           = uop[`P_W_OPCODE];
    assign decode             = uop[`P_DECODE];
    assign mem_r              = uop[`P_MEM_R];
    assign mem_w              = uop[`P_MEM_W];
    assign w_a                = uop[`P_W_A];
    assign w_c                = uop[`P_W_C];
    assign w_y                = uop[`P_W_Y];
    assign cond_a_zero        = uop[`P_A_NEG];
    assign cond_a_neg         = uop[`P_A_ZERO];
    assign branch             = uop[`P_BRANCH];
    assign clear_c            = uop[`P_CLEAR_C];
    assign enter_interrupt    = uop[`P_ENTER_INT];
    assign exit_interrupt     = uop[`P_EXIT_INT];
    assign w_pc               = uop[`P_W_PC];

    $fwrite(tracefd, "(%0d) %3d: %o", ctime, upc, uop);

    if (sel_pc == `SEL_PC_IMM || sel_mr == `SEL_MR_IMM ||
        sel_mw == `SEL_MW_IMM || cond_op_not_cached ||
        cond_a_zero || cond_a_neg || branch)
        $fwrite(tracefd, " imm=%0d", imm);
    if (alu_op) $fwrite(tracefd, " alu=%0s",  op_name[alu_op]);
    if (w_a)    $fwrite(tracefd, " acc=%0s", acc_name[sel_acc]);
    if (w_m)    $fwrite(tracefd, " md=%0s",  md_name[sel_md]);
    if (w_m)    $fwrite(tracefd, " mw=%0s",  mw_name[sel_mw]);
    if (sel_addr || m_use || sel_pc == `SEL_PC_REG ||
        sel_acc == `SEL_ACC_REG || sel_md == `SEL_MD_REG ||
        sel_md == `SEL_MD_REG_PLUS1 || sel_md == `SEL_MD_REG_MINUS1)
        $fwrite(tracefd, " mr=%0s",  mr_name[sel_mr]);
    if (w_pc) $fwrite(tracefd, " pc=%0s",  pc_name[sel_pc]);

    if (w_m)                $fwrite(tracefd, " w_m");
    if (m_use)              $fwrite(tracefd, " m_use");
    if (sel_addr)           $fwrite(tracefd, " sel_addr");
    if (sel_j_add)          $fwrite(tracefd, " sel_j_add");
    if (sel_c_mem)          $fwrite(tracefd, " sel_c_mem");
    if (cond_op_not_cached) $fwrite(tracefd, " cond_op_not_cached");
    if (fetch)              $fwrite(tracefd, " fetch");
    if (w_opcode)           $fwrite(tracefd, " w_opcode");
    if (decode)             $fwrite(tracefd, " decode");
    if (mem_r)              $fwrite(tracefd, " mem_r");
    if (mem_w)              $fwrite(tracefd, " mem_w");
    if (w_a)                $fwrite(tracefd, " w_a");
    if (w_c)                $fwrite(tracefd, " w_c");
    if (w_y)                $fwrite(tracefd, " w_y");
    if (cond_a_zero)        $fwrite(tracefd, " cond_a_zero");
    if (cond_a_neg)         $fwrite(tracefd, " cond_a_neg");
    if (branch)             $fwrite(tracefd, " branch");
    if (clear_c)            $fwrite(tracefd, " clear_c");
    if (enter_interrupt)    $fwrite(tracefd, " enter_interrupt");
    if (exit_interrupt)     $fwrite(tracefd, " exit_interrupt");
    if (w_pc)               $fwrite(tracefd, " w_pc");

    if (uop == 0) $fwrite(tracefd, " nop");

    if (busy)
        $fwrite(tracefd, " --- busy");
    $fdisplay(tracefd, "");

endtask

//
// Print changed state at architectural level
//
task print_changed_regs();
    static logic [47:0] old_acc, old_Y;
    static logic [5:0] old_R;
    static logic [14:0] old_M[16], old_C;
    static string ir_name[16] = '{
        0:"M0",     1:"M1",     2:"M2",     3:"M3",
        4:"M4",     5:"M5",     6:"M6",     7:"M7",
        8:"M10",    9:"M11",    10:"M12",   11:"M13",
        12:"M14",   13:"M15",   14:"M16",   15:"SP"
    };

    // Accumulator
    if (cpu.acc !== old_acc) begin
        $fdisplay(tracefd, "(%0d)      Write A = %o", ctime, cpu.acc);
        old_acc = cpu.acc;
    end

    // Y register
    if (cpu.Y !== old_Y) begin
        $fdisplay(tracefd, "(%0d)      Write Y = %o", ctime, cpu.Y);
        old_Y = cpu.Y;
    end

    // R register
    if (cpu.R !== old_R) begin
        $fdisplay(tracefd, "(%0d)      Write R = %o", ctime, cpu.R);
        old_R = cpu.R;
    end

    // C register
    if (cpu.C !== old_C) begin
        $fdisplay(tracefd, "(%0d)      Write C = %o", ctime, cpu.C);
        old_C = cpu.C;
    end

    //
    // Index-registers
    //
    for (int i=0; i<16; i+=1) begin
        if (cpu.M[i] !== old_M[i]) begin
            $fdisplay(tracefd, "(%0d)      Write %0s = %o",
                ctime, ir_name[i], cpu.M[i]);
            old_M[i] = cpu.M[i];
        end
    end

endtask

endmodule
