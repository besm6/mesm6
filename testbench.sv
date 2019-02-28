//
// Run the processor with memory attached.
//
// Copyright (c) 2018 Serge Vakulenko
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
logic        clk, reset;
logic        i_interrupt;   // interrupt request
logic        i_done;        // memory operation completed
logic [47:0] i_data_read;   // data from memory

// Outputs.
logic        o_read;        // read op
logic        o_write;       // write op
logic [14:0] o_addr;        // address output
logic [47:0] o_data_write;  // data to memory

// Instantiate CPU.
mesm6_core cpu(
    clk,                    // clock on rising edge
    reset,                  // reset on rising edge
    i_interrupt,            // interrupt request
    o_read,                 // request memory read
    o_write,
    i_done,                 // memory operation completed
    o_addr,                 // memory address
    i_data_read,            // data read
    o_data_write            // data written
);

// 1Mword x 32bit of RAM.
memory ram(
    clk,                    // clock on rising edge
    o_addr,                 // input address
    o_read,                 // input read request
    o_write,                // input write request
    o_data_write,           // input data to memory
    i_data_read,            // output data from memory
    i_done                  // output r/w operation completed
);

string tracefile = "output.trace";
int limit;
int tracelevel;             // Trace level
int tracefd;                // Trace file descriptor
time ctime;                 // Current time

//
// Last fetch address
//
logic [`UPC_BITS-1:0] upc_f;    // PC at fetch stage

// Time routines imported from C library.
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
always #1 clk = ~clk;

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
    i_interrupt = 0;

    // Hold reset for a while.
    #2 reset = 0;

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
    upc_f = cpu.upc;
end

//
// Tracer
//
//
// Import standard C function gettimeofday().
//
longint instr_count;                    // Instruction and micro-instruction counters
longint uinstr_count;
bit old_reset = 0;                      // Previous state of reset

// At negative clock edge, when all the signals are quiet,
// print the state of the processor.
always @(negedge clk) begin
    if (tracefd) begin
        if (reset) begin
            if (!old_reset) begin               // Reset
                $fdisplay(tracefd, "(%0d) *** Reset", ctime);
                old_reset = 1;
            end
        end else begin
            if (old_reset) begin                // Clear reset
                $fdisplay(tracefd, "(%0d) *** Clear reset", ctime);
                old_reset = 0;
            end
        end

        if (tracelevel > 1) begin
            // Print last executed micro-instruction
            if (!reset)
                print_uop(upc_f, cpu.uop);

            // Print changed micro state
            //print_changed_cpu(opcode_x);
        end else begin
            // Print changed architectural state
            //print_changed_regs(opcode_x);
        end

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

`ifdef notdef
    if (!reset && $isunknown(cpu.opcode)) begin
        $display("(%0d) Unknown instruction: cpu.opcode=%h", ctime, cpu.opcode);
        if (tracefd)
            $fdisplay(tracefd, "(%0d) *** Unknown instruction: cpu.opcode=%h", ctime, cpu.opcode);
        terminate("Fatal Error!");
    end
`endif

    if ((cpu.mem_read | cpu.mem_write) && $isunknown(cpu.mem_addr)) begin
        $display("(%0d) Unknown address: cpu.mem_addr=%h", ctime, cpu.mem_addr);
        if (tracefd)
            $fdisplay(tracefd, "(%0d) *** Unknown address: cpu.mem_addr=%h", ctime, cpu.mem_addr);
        terminate("Fatal Error!");
    end

    //TODO
    //if (!cpu.run) begin
    //    cpu_halted();
    //end
end

// ---- register operation dump ----
always @(negedge clk) begin
    if (~reset) begin
        uinstr_count++;

        if (cpu.w_pc) $fdisplay(tracefd, "--- set PC=0x%h", cpu.alu.alu_r);
        if (cpu.w_sp) $fdisplay(tracefd, "--- set SP=0x%h", cpu.alu.alu_r);
        if (cpu.w_a) $fdisplay(tracefd, "--- set A=0x%h", cpu.alu.alu_r);
        if (cpu.w_a_mem) $fdisplay(tracefd, "--- set A=0x%h (from MEM)", cpu.mem_data_read);
        if (cpu.w_b) $fdisplay(tracefd, "--- set B=0x%h", cpu.alu.alu_r);
        if (cpu.w_op & ~cpu.is_op_cached) $fdisplay(tracefd, "--- set opcode_cache=0x%h, pc_cached=0x%h", cpu.alu.alu_r, {cpu.pc[31:2], 2'b0});

        if (~cpu.busy & cpu.upc == `UADDR_INTERRUPT) $fdisplay(tracefd, "--- ***** ENTERING INTERRUPT MICROCODE ******");
        if (~cpu.busy & cpu.exit_interrupt) $fdisplay(tracefd, "--- ***** INTERRUPT FLAG CLEARED *****");
        if (~cpu.busy & cpu.enter_interrupt) $fdisplay(tracefd, "--- ***** INTERRUPT FLAG SET *****");

// ---- microcode trace ----
        if (~cpu.busy) begin
            $fdisplay(tracefd, "--- uop[%d]=0b%b", cpu.upc, cpu.uop);
            if (cpu.branch)      $fdisplay(tracefd, "--- microcode: branch=%d", cpu.u_goto);
            if (cpu.cond_branch) $fdisplay(tracefd, "--- microcode: CONDITION branch=%d", cpu.u_goto);
            if (cpu.decode)      $fdisplay(tracefd, "--- decoding opcode=0x%h (0b%b) : branch to=%d ", cpu.opcode, cpu.opcode, cpu.u_entry);
        end else
            $fdisplay(tracefd, "--- busy");
    end
end

// ----- opcode dissasembler ------
always @(negedge clk) begin
    if (~cpu.busy)
        case (cpu.upc)
        0 : begin
            $fdisplay(tracefd, "--- ------  breakpoint ------");
            $finish;
        end
        4 : $fdisplay(tracefd, "--- ------  shiftleft ------");
        8 : $fdisplay(tracefd, "--- ------  pushsp ------");
        12 : $fdisplay(tracefd, "--- ------  popint ------");
        16 : $fdisplay(tracefd, "--- ------  poppc ------");
        20 : $fdisplay(tracefd, "--- ------  add ------");
        24 : $fdisplay(tracefd, "--- ------  and ------");
        28 : $fdisplay(tracefd, "--- ------  or ------");
        32 : $fdisplay(tracefd, "--- ------  load ------");
        36 : $fdisplay(tracefd, "--- ------  not ------");
        40 : $fdisplay(tracefd, "--- ------  flip ------");
        44 : $fdisplay(tracefd, "--- ------  nop ------");
        48 : $fdisplay(tracefd, "--- ------  store ------");
        52 : $fdisplay(tracefd, "--- ------  popsp ------");
        56 : $fdisplay(tracefd, "--- ------  ipsum ------");
        60 : $fdisplay(tracefd, "--- ------  sncpy ------");

        `UADDR_STORESP   : $fdisplay(tracefd, "--- ------  storesp 0x%h ------", { ~cpu.opcode[4], cpu.opcode[3:0], 2'b0 } );
        `UADDR_LOADSP    : $fdisplay(tracefd, "--- ------  loadsp 0x%h ------", { ~cpu.opcode[4], cpu.opcode[3:0], 2'b0 } );
        `UADDR_ADDSP     : $fdisplay(tracefd, "--- ------  addsp 0x%h ------", { ~cpu.opcode[4], cpu.opcode[3:0], 2'b0 } );
        `UADDR_EMULATE   : $fdisplay(tracefd, "--- ------  emulate 0x%h ------", cpu.b[2:0]); // opcode[5:0] );

        128 : $fdisplay(tracefd, "--- ------  mcpy ------");
        132 : $fdisplay(tracefd, "--- ------  mset ------");
        136 : $fdisplay(tracefd, "--- ------  loadh ------");
        140 : $fdisplay(tracefd, "--- ------  storeh ------");
        144 : $fdisplay(tracefd, "--- ------  lessthan ------");
        148 : $fdisplay(tracefd, "--- ------  lessthanorequal ------");
        152 : $fdisplay(tracefd, "--- ------  ulessthan ------");
        156 : $fdisplay(tracefd, "--- ------  ulessthanorequal ------");
        160 : $fdisplay(tracefd, "--- ------  swap ------");
        164 : $fdisplay(tracefd, "--- ------  mult ------");
        168 : $fdisplay(tracefd, "--- ------  lshiftright ------");
        172 : $fdisplay(tracefd, "--- ------  ashiftleft ------");
        176 : $fdisplay(tracefd, "--- ------  ashiftright ------");
        180 : $fdisplay(tracefd, "--- ------  call ------");
        184 : $fdisplay(tracefd, "--- ------  eq ------");
        188 : $fdisplay(tracefd, "--- ------  neq ------");
        192 : $fdisplay(tracefd, "--- ------  neg ------");
        196 : $fdisplay(tracefd, "--- ------  sub ------");
        200 : $fdisplay(tracefd, "--- ------  xor ------");
        204 : $fdisplay(tracefd, "--- ------  loadb ------");
        208 : $fdisplay(tracefd, "--- ------  storeb ------");
        212 : $fdisplay(tracefd, "--- ------  div ------");
        216 : $fdisplay(tracefd, "--- ------  mod ------");
        220 : $fdisplay(tracefd, "--- ------  eqbranch ------");
        224 : $fdisplay(tracefd, "--- ------  neqbranch ------");
        228 : $fdisplay(tracefd, "--- ------  poppcrel ------");
        232 : $fdisplay(tracefd, "--- ------  config ------");
        236 : $fdisplay(tracefd, "--- ------  pushpc ------");
        240 : $fdisplay(tracefd, "--- ------  syscall_emulate ------");
        244 : $fdisplay(tracefd, "--- ------  pushspadd ------");
        248 : $fdisplay(tracefd, "--- ------  halfmult ------");
        252 : $fdisplay(tracefd, "--- ------  callpcrel ------");
      //default : $fdisplay(tracefd, "--- upc=0x%h", decode_mcpc);
        endcase
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
task print_uop(
    input logic [`UPC_BITS-1:0] upc,    // microcode PC
    input logic [`UOP_BITS-1:0] uop     // microcode operation
);
    static string alu_name[4] = '{
        0: "A",    1: "OPCODE",    2: "CONST",   3: "B"
    };
    static string addr_name[4] = '{
        0: "PC",    1: "SP",    2: "A",   3: "B"
    };
    static string op_name[16] = '{
        0: "A",    1: "B",     2: "A+B",  3: "A+Boff",
        4: "A&B",  5: "A|B",   6: "~A",   7: "?7",
        8: "?8",   9: "?9",    10:"?10",  11:"?11",
        12:"?12",  13:"?13",   14:"?14",  15:"?15"
    };

    logic       sel_read;
    logic [1:0] sel_alu;
    logic [1:0] sel_addr;
    logic [3:0] alu_op;
    logic       w_sp;
    logic       w_pc;
    logic       w_a;
    logic       w_a_mem;
    logic       w_b;
    logic       w_op;
    logic       mem_read;
    logic       mem_write;
    logic       w_pc_increment;
    logic       exit_interrupt;
    logic       enter_interrupt;
    logic       cond_op_not_cached;
    logic       cond_a_zero;
    logic       cond_a_neg;
    logic       decode;
    logic       branch;
    logic [`UPC_BITS-1:0] goto;

    assign sel_read           = uop[`P_SEL_READ];
    assign sel_alu            = uop[`P_SEL_ALU+1:`P_SEL_ALU];
    assign sel_addr           = uop[`P_SEL_ADDR+1:`P_SEL_ADDR];
    assign alu_op             = uop[`P_ALU+3:`P_ALU];
    assign w_sp               = uop[`P_W_SP];
    assign w_pc               = uop[`P_W_PC];
    assign w_a                = uop[`P_W_A];
    assign w_a_mem            = uop[`P_W_A_MEM];
    assign w_b                = uop[`P_W_B];
    assign w_op               = uop[`P_W_OPCODE];
    assign mem_read           = uop[`P_MEM_R];
    assign mem_write          = uop[`P_MEM_W];
    assign exit_interrupt     = uop[`P_EXIT_INT];
    assign enter_interrupt    = uop[`P_ENTER_INT];
    assign cond_op_not_cached = uop[`P_OP_NOT_CACHED];
    assign cond_a_zero        = uop[`P_A_ZERO];
    assign cond_a_neg         = uop[`P_A_NEG];
    assign decode             = uop[`P_DECODE];
    assign branch             = uop[`P_BRANCH];
    assign goto               = { uop[`P_ADDR+6:`P_ADDR], 2'b00 };

    $fwrite(tracefd, "(%0d) %0d:", ctime, upc);

    if (sel_read != 0) $fwrite(tracefd, " sel_read");
    if (sel_alu  != 0) $fwrite(tracefd, " sel_alu=%0s", alu_name[sel_alu]);
    if (sel_addr != 0) $fwrite(tracefd, " sel_addr=%0s", addr_name[sel_addr]);
    if (alu_op   != 0) $fwrite(tracefd, " alu_op=%0s", op_name[alu_op]);

    if (w_sp               != 0) $fwrite(tracefd, " w_sp");
    if (w_pc               != 0) $fwrite(tracefd, " w_pc");
    if (w_a                != 0) $fwrite(tracefd, " w_a");
    if (w_a_mem            != 0) $fwrite(tracefd, " w_a_mem");
    if (w_b                != 0) $fwrite(tracefd, " w_b");
    if (w_op               != 0) $fwrite(tracefd, " w_op");
    if (mem_read           != 0) $fwrite(tracefd, " mem_read");
    if (mem_write          != 0) $fwrite(tracefd, " mem_write");
    if (w_pc_increment     != 0) $fwrite(tracefd, " w_pc_increment");
    if (exit_interrupt     != 0) $fwrite(tracefd, " exit_interrupt");
    if (enter_interrupt    != 0) $fwrite(tracefd, " enter_interrupt");
    if (cond_op_not_cached != 0) $fwrite(tracefd, " cond_op_not_cached");
    if (cond_a_zero        != 0) $fwrite(tracefd, " cond_a_zero");
    if (cond_a_neg         != 0) $fwrite(tracefd, " cond_a_neg");
    if (decode             != 0) $fwrite(tracefd, " decode");
    if (branch             != 0) $fwrite(tracefd, " branch");

    if (goto  != 0)    $fwrite(tracefd, " goto=%0d", goto);
    $fdisplay(tracefd, "");
endtask

endmodule
