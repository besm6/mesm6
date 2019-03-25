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
logic [`UPC_BITS-1:0] upc;      // last micro-PC
logic [`UOP_BITS-1:0] uop;      // last micro-instruction
logic                 busy;     // last busy signal
logic [23:0]          opcode;   // last besm opcode
logic [15:0]          pc;       // last PC
logic                 fetch;    // new opcode obtained

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
    string octfile;

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
        limit = 99;
    end
    $display("Limit: %0d", limit);
    if (tracefd)
        $fdisplay(tracefd, "Limit: %0d", limit);

    // Load memory contents.
    if (! $value$plusargs("load=%s", octfile)) begin
        $display("----- Load option required -----");
        $display("Options:");
        $display("    +load=NAME        Load code into main memory");
        $display("    +limit=NUM        Limit execution to a number of cycles (default %0d)", limit);
        $display("    +dump             Dump waveforms as output.vcd");
        $display("");
        $finish(1);
    end
    load_oct(octfile);

    // Start with reset active
    clk = 1;
    reset = 1;
    irq = 0;

    // Hold reset for a while.
    #1 reset = 0;

    // Run until limit.
    gettimeofday(t0, null);
    #limit terminate("Time Limit Exceeded");
end

//
// Print a message to stdout and trace file
//
task message(input string msg);
    $display("*** %s", msg);
    if (tracefd)
        $fdisplay(tracefd, "(%0d) *** %s", ctime, msg);
endtask

//
// Load memory contents from *.oct file.
//
task load_oct(input string filename);
    int fd, i, count, lm, lop, laddr, rm, rop, raddr;
    string line, key;
    logic [47:0] word;

    // Open file with code.
    fd = $fopen(filename, "r");
    if (fd == 0) begin
        $error("%s: Cannot open", filename);
        $finish(1);
    end

    // Read hex code.
    count = 0;
    while ($fgets(line, fd)) begin
        if (line[0] == "#")
            continue;

        if (line[0] == "i") begin
            // i 00001 02 24 00000 01 010 0007
            if ($sscanf(line, "%c %o %o %o %o %o %o %o", key, i, lm, lop, laddr, rm, rop, raddr) != 8) begin
                $display("Bad line in OCT file: %s", line);
                if (tracefd)
                    $fdisplay(tracefd, "Bad line in OCT file: %s", line);
                $finish(1);
            end
            word[47:44] = lm;
            if (line[11] != "0") begin
                word[43:39] = lop;
                word[38:24] = laddr;
            end else begin
                word[43:36] = lop;
                word[35:24] = laddr;
            end
            word[23:20] = rm;
            if (line[23] != "0") begin
                word[19:15] = rop;
                word[14:0]  = raddr;
            end else begin
                word[19:12] = rop;
                word[11:0]  = raddr;
            end
            //$fdisplay(tracefd, "i %05o: %016o", i, word);
            prom.mem[i] = word;
        end else begin
            if ($sscanf(line, "%c %o %o %o %o %o", key, i, lm, laddr, rm, raddr) != 6) begin
                $display("Bad line in OCT file: %s", line);
                if (tracefd)
                    $fdisplay(tracefd, "Bad line in OCT file: %s", line);
                $finish(1);
            end
            word[47:36] = lm;
            word[35:24] = laddr;
            word[23:12] = rm;
            word[11:0]  = raddr;
            //$fdisplay(tracefd, "d %05o: %016o", i, word);
            ram.mem[i] = word;
        end
        count += 1;
    end
    $fclose(fd);
    $display("Load %0d words from %s", count, filename);
    if (tracefd)
        $fdisplay(tracefd, "Load %0d words from %s", count, filename);
endtask

// Get time at the rising edge of the clock.
always @(posedge clk) begin
    ctime = $time;
    upc = cpu.upc;
    uop = cpu.uop;
    busy = cpu.busy;
    fetch = 0;
    if (~reset & ~cpu.busy) begin
        uinstr_count++;
        if (cpu.decode & ~cpu.branch) begin
            fetch = 1;
            pc = cpu.pc;
            opcode = cpu.opcode;
            instr_count++;
        end
    end
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

            // Print transactions on external bus
            print_ext_bus();

            // Print BESM instruction
            if (fetch)
                print_insn();

            if (cpu.irq)
                $fdisplay(tracefd, "(%0d) *** Interrupt", ctime);
        end
    end

    // Check for magic opcodes.
    if (fetch && opcode == 'o33312345) begin
        // stop 12345(6) - success.
        terminate("Test PASS");
    end
    if (fetch && opcode == 'o13376543) begin
        // stop 76543(2) - failure.
        terminate("Test FAIL");
    end

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
    $display("   System clock: %0d ticks", ctime);
    $display("      Simulated: %0d instructions, %0d micro-instructions",
        instr_count, uinstr_count);
    if (usec > 0)
        $display("Simulation rate: %.1f kHz, %.0f instructions/sec, %.0f micro-instructions/sec",
            1000.0 * ctime / usec,
            1000000.0 * instr_count / usec,
            1000000.0 * uinstr_count / usec);

    if (tracefd) begin
        $fdisplay(tracefd, "   Elapsed time: %0d seconds", usec / 1000000);
        $fdisplay(tracefd, "   System clock: %0d ticks", ctime);
        $fdisplay(tracefd, "      Simulated: %0d instructions, %0d micro-instructions",
            instr_count, uinstr_count);
        if (usec > 0)
            $fdisplay(tracefd, "Simulation rate: %.1f kHz, %.0f instructions/sec, %.0f micro-instructions/sec",
                1000.0 * ctime / usec,
                1000000.0 * instr_count / usec,
                1000000.0 * uinstr_count / usec);
    end

    $finish;
endtask

//
// Print micro-instruction.
//
task print_uop();
    static string op_name[32] = '{
        0: "A",        1: "A&B",      2: "A|B",      3: "A^B",
        4: "A<<B",     5: "A++B",     6: "A.pack.B", 7: "A.unpack.B",
        8: ".count.A", 9: ".clz.A",   10:"A+B",      11:"A-B",
        12:"B-A",      13:"A.sign.B", 14:"?14",      15:"A+.exp.B",
        16:"A+.exp.B", 17:"A*B",      18:"A/B",      19:"?19",
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
        0: "UA",    1: "VA",  2: "REG", 3: "IMM",
        4: "PLUS1", 5: "?5",  6: "?6",  7: "?7"
    };

    logic [`UPC_BITS-1:0] imm;
    logic [5:0] alu_op;
    logic [2:0] sel_acc;
    logic [2:0] sel_md;
    logic [1:0] sel_mw;
    logic [1:0] sel_mr;
    logic [2:0] sel_pc;
    logic       w_m;
    logic       sel_addr;
    logic       r_add;
    logic       sel_c_mem;
    logic       sel_alu_mem;
    logic       cond_op_not_cached;
    logic       fetch;
    logic       w_opcode;
    logic       decode;
    logic       mem_r;
    logic       mem_w;
    logic       w_a;
    logic       w_c;
    logic       cond_a_zero;
    logic       cond_a_nonzero;
    logic       cond_a_neg;
    logic       cond_m_zero;
    logic       cond_m_nonzero;
    logic       branch;
    logic       c_active;
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
    assign sel_addr           = uop[`P_SEL_ADDR];
    assign r_add              = uop[`P_R_ADD];
    assign sel_c_mem          = uop[`P_SEL_C_MEM];
    assign sel_alu_mem        = uop[`P_SEL_ALU_MEM];
    assign cond_op_not_cached = uop[`P_OP_NOT_CACHED];
    assign fetch              = uop[`P_FETCH];
    assign w_opcode           = uop[`P_W_OPCODE];
    assign decode             = uop[`P_DECODE];
    assign mem_r              = uop[`P_MEM_R];
    assign mem_w              = uop[`P_MEM_W];
    assign w_a                = uop[`P_W_A];
    assign w_c                = uop[`P_W_C];
    assign cond_a_zero        = uop[`P_A_ZERO];
    assign cond_a_nonzero     = uop[`P_A_NONZERO];
    assign cond_a_neg         = uop[`P_A_NEG];
    assign cond_m_zero        = uop[`P_M_ZERO];
    assign cond_m_nonzero     = uop[`P_M_NONZERO];
    assign branch             = uop[`P_BRANCH];
    assign c_active           = uop[`P_C_ACTIVE];
    assign enter_interrupt    = uop[`P_ENTER_INT];
    assign exit_interrupt     = uop[`P_EXIT_INT];
    assign w_pc               = uop[`P_W_PC];

    $fwrite(tracefd, "(%0d) %5d: %o", ctime, upc, uop);

    if (sel_pc == `SEL_PC_IMM || sel_mr == `SEL_MR_IMM ||
        sel_mw == `SEL_MW_IMM || cond_op_not_cached ||
        cond_a_zero || cond_a_nonzero || cond_m_zero || cond_m_nonzero || cond_a_neg || branch)
        $fwrite(tracefd, " imm=%0d", imm);
    if (alu_op) $fwrite(tracefd, " alu=%0s",  op_name[alu_op]);
    if (w_a)    $fwrite(tracefd, " acc=%0s", acc_name[sel_acc]);
    if (w_m)    $fwrite(tracefd, " md=%0s",  md_name[sel_md]);
    if (w_m)    $fwrite(tracefd, " mw=%0s",  mw_name[sel_mw]);
    if (r_add || sel_acc == `SEL_ACC_REG || sel_md == `SEL_MD_REG ||
        sel_md == `SEL_MD_REG_PLUS1 || sel_md == `SEL_MD_REG_MINUS1)
        $fwrite(tracefd, " mr=%0s",  mr_name[sel_mr]);
    if (w_pc) $fwrite(tracefd, " pc=%0s",  pc_name[sel_pc]);

    if (w_m)                $fwrite(tracefd, " w_m");
    if (sel_addr)           $fwrite(tracefd, " sel_addr");
    if (r_add)              $fwrite(tracefd, " r_add");
    if (sel_c_mem)          $fwrite(tracefd, " c_mem");
    if (sel_alu_mem)        $fwrite(tracefd, " alu_mem");
    if (cond_op_not_cached) $fwrite(tracefd, " cond_op_not_cached");
    if (fetch)              $fwrite(tracefd, " fetch");
    if (w_opcode)           $fwrite(tracefd, " w_opcode");
    if (decode)             $fwrite(tracefd, " decode");
    if (mem_r)              $fwrite(tracefd, " mem_r");
    if (mem_w)              $fwrite(tracefd, " mem_w");
    if (w_a)                $fwrite(tracefd, " w_a");
    if (w_c)                $fwrite(tracefd, " w_c");
    if (cond_a_zero)        $fwrite(tracefd, " cond_a_zero");
    if (cond_a_nonzero)     $fwrite(tracefd, " cond_a_nonzero");
    if (cond_a_neg)         $fwrite(tracefd, " cond_a_neg");
    if (cond_m_zero)        $fwrite(tracefd, " cond_m_zero");
    if (cond_m_nonzero)     $fwrite(tracefd, " cond_m_nonzero");
    if (branch)             $fwrite(tracefd, " branch");
    if (c_active)           $fwrite(tracefd, " c_active");
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
    static string ir_name[16] = '{
        0:"M[0]",   1:"M[1]",   2:"M[2]",   3:"M[3]",
        4:"M[4]",   5:"M[5]",   6:"M[6]",   7:"M[7]",
        8:"M[8]",   9:"M[9]",  10:"M[10]", 11:"M[11]",
       12:"M[12]", 13:"M[13]", 14:"M[14]", 15:"SP"
    };
    static logic [15:0] old_pc;
    static logic [47:0] old_acc, old_Y;
    static logic  [5:0] old_R;
    static logic [14:0] old_M[16], old_C;
    static logic        old_gie;

    // Accumulator
    if (cpu.acc !== old_acc) begin
        $fdisplay(tracefd, "(%0d)        Write A = %o %o %o %o",
            ctime, cpu.acc[47:36], cpu.acc[35:24],
            cpu.acc[23:12], cpu.acc[11:0]);
        old_acc = cpu.acc;
    end

    // Y register
    if (cpu.Y !== old_Y) begin
        $fdisplay(tracefd, "(%0d)        Write Y = %o %o %o %o",
            ctime, cpu.Y[47:36], cpu.Y[35:24],
            cpu.Y[23:12], cpu.Y[11:0]);
        old_Y = cpu.Y;
    end

    // R register
    if (cpu.R !== old_R) begin
        $fdisplay(tracefd, "(%0d)        Write R = %o", ctime, cpu.R);
        old_R = cpu.R;
    end

    // C register
    if (cpu.C !== old_C || (uop[`P_W_C] & ~busy)) begin
        $fdisplay(tracefd, "(%0d)        Write C = %o", ctime, cpu.C);
        old_C = cpu.C;
    end

    //
    // Index-registers
    //
    for (int i=0; i<16; i+=1) begin
        if (cpu.M[i] !== old_M[i]) begin
            $fdisplay(tracefd, "(%0d)        Write %0s = %o",
                ctime, ir_name[i], cpu.M[i]);
            old_M[i] = cpu.M[i];
        end
    end

    // Global Interrupt Enable
    if (cpu.gie !== old_gie) begin
        $fdisplay(tracefd, "(%0d)        Write GIE = %o", ctime, cpu.gie);
        old_gie = cpu.gie;
    end

    // PC
    if (tracelevel >= 2 && cpu.pc !== old_pc) begin
        $fdisplay(tracefd, "(%0d)        Write PC = %o:%b", ctime, cpu.pc[15:1], cpu.pc[0]);
        old_pc = cpu.pc;
    end

endtask

//
// Print transactions on external bus: memory loads/stores/fetches etc.
//
task print_ext_bus();
    if (ibus_rd & ibus_done && tracelevel > 1)
        $fdisplay(tracefd, "(%0d)        Memory Fetch [%o] = %o %o %o %o",
            ctime, ibus_addr, ibus_input[47:36], ibus_input[35:24],
            ibus_input[23:12], ibus_input[11:0]);

    if (dbus_wr & dbus_done)
        $fdisplay(tracefd, "(%0d)        Memory Store [%o] = %o %o %o %o",
            ctime, dbus_addr, dbus_output[47:36], dbus_output[35:24],
            dbus_output[23:12], dbus_output[11:0]);

    else if (dbus_rd & dbus_done)
        $fdisplay(tracefd, "(%0d)        Memory Load [%o] = %o %o %o %o",
            ctime, dbus_addr, dbus_input[47:36], dbus_input[35:24],
            dbus_input[23:12], dbus_input[11:0]);
endtask

//
// Print BESM-6 instruction.
//
task print_insn();
    static string long_name[16] = '{
        0:"20", 1:"21",   2:"utc", 3:"wtc",   4:"vtm",  5:"utm",  6:"uza", 7:"u1a",
        8:"uj", 9:"vjm", 10:"ij",  11:"stop", 12:"vzm", 13:"vim", 14:"36", 15:"vlm"
    };
    static string short_name[64] = '{
         0:"atx",  1:"stx",  2:"*02",  3:"xts",  4:"a+x",  5:"a-x",  6:"x-a",  7:"amx",
         8:"xta",  9:"aax", 10:"aex", 11:"arx", 12:"avx", 13:"aox", 14:"a/x", 15:"a*x",
        16:"apx", 17:"aux", 18:"acx", 19:"anx", 20:"e+x", 21:"e-x", 22:"asx", 23:"xtr",
        24:"rte", 25:"yta", 26:"*32", 27:"*33", 28:"e+n", 29:"e-n", 30:"asn", 31:"ntr",
        32:"ati", 33:"sti", 34:"ita", 35:"its", 36:"mtj", 37:"j+m", 38:"*46", 39:"*47",
        40:"*50", 41:"*51", 42:"*52", 43:"*53", 44:"*54", 45:"*55", 46:"*56", 47:"*57",
        48:"*60", 49:"*61", 50:"*62", 51:"*63", 52:"*64", 53:"*65", 54:"*66", 55:"*67",
        56:"*70", 57:"*71", 58:"*72", 59:"*73", 60:"*74", 61:"*75", 62:"*76", 63:"*77"
    };
    automatic logic [3:0]  op_ir    = opcode[23:20];
    automatic logic        op_lflag = opcode[19];
    automatic logic [3:0]  op_lcmd  = opcode[18:15];
    automatic logic [5:0]  op_scmd  = opcode[17:12];
    automatic logic [14:0] op_addr  = op_lflag ? opcode[14:0]
                                               : {{3{opcode[18]}}, opcode[11:0]};

    // Print BESM instruction.
    $fwrite(tracefd, "(%0d) %05o: %o", ctime, pc[15:1], opcode);

    if ($isunknown(opcode)) begin
        $fdisplay(tracefd, " *** Unknown");
        return;
    end

    // Instruction name
    $fwrite(tracefd, " %s ", op_lflag ? long_name[op_lcmd] :
                                       short_name[op_scmd]);

    // Address
    if (op_addr != 0) begin
        $fwrite(tracefd, "%0o", op_addr);
    end

    // Register
    if (op_ir != 0)
        $fwrite(tracefd, "(%0d)", op_ir);
    $fdisplay(tracefd, "");
endtask

endmodule
