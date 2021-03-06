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
logic        clk, reset;
wire         irq;

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
wire [14:0] mem_addr;
wire mem_read;
wire mem_write;
wire mem_done;
wire [47:0] mem_rdata;
wire [47:0] mem_wdata;

dmemory ram(
    clk,                    // clock on rising edge
    mem_addr,              // memory address
    mem_read,                // read request
    mem_write,                // write request
    mem_rdata,            // data to memory
    mem_wdata,             // data from memory
    mem_done               // operation completed
);

// GPIO signals
wire        gpio_int;
reg  [47:0] gpio_inputs;
wire [47:0] gpio_outputs;
wire [14:0] gpio_addr;
wire        gpio_read;
wire        gpio_write;
wire [47:0] gpio_rdata;
wire [47:0] gpio_wdata;
wire        gpio_done;

mesm6_gpio gpio(
    clk, reset, gpio_int,


    gpio_addr,
    gpio_read,
    gpio_write,
    gpio_rdata,
    gpio_wdata,
    gpio_done,

    gpio_inputs,
    gpio_outputs
);

wire [47:0] pic_irq;
wire [14:0] pic_addr;
wire        pic_read;
wire        pic_write;
wire [47:0] pic_rdata;
wire [47:0] pic_wdata;
wire        pic_done;

mesm6_pic pic(
    clk, reset, irq,

    pic_irq,

    pic_addr,
    pic_read,
    pic_write,
    pic_rdata,
    pic_wdata,
    pic_done
);

// Timer signals
wire        tim_irq;
wire [14:0] tim_addr;
wire        tim_read;
wire        tim_write;
wire [47:0] tim_rdata;
wire [47:0] tim_wdata;
wire        tim_done;
wire        tim_pwm;

mesm6_timer tim(
    clk, reset, tim_irq,

    tim_addr,
    tim_read, tim_write,
    tim_rdata, tim_wdata,
    tim_done,
    
    tim_pwm
);

// UART wires
wire        uart_irq;
wire [14:0] uart_addr;
wire        uart_read;
wire        uart_write;
wire [47:0] uart_rdata;
wire [47:0] uart_wdata;
wire        uart_done;
wire        uart_tx_pin;
reg         uart_rx_pin;

mesm6_uart uart(
    clk, reset, uart_irq,

    uart_addr,
    uart_read, uart_write,
    uart_rdata, uart_wdata,
    uart_done,

    uart_tx_pin,
    uart_rx_pin
);


// dummy VGA signals
wire [14:0]  vga_addr;
wire         vga_read;
wire         vga_write;
wire [47:0]  vga_rdata;
wire [47:0]  vga_wdata;
wire         vga_done;
wire         vga_irq;

mesm6_mmu mmu(
    dbus_addr,
    dbus_rd, dbus_wr,
    dbus_output, dbus_input,
    dbus_done,

    mem_addr,
    mem_read,  mem_write,
    mem_wdata, mem_rdata,
    mem_done,

    pic_addr,
    pic_read,  pic_write,
    pic_rdata, pic_wdata,
    pic_done,
    pic_irq,

    gpio_addr,
    gpio_read, gpio_write,
    gpio_rdata, gpio_wdata,
    gpio_done,
    gpio_int,

    uart_addr,
    uart_read, uart_write,
    uart_rdata, uart_wdata,
    uart_done,
    uart_irq,

    vga_addr,
    vga_read, vga_write,
    vga_rdata, vga_wdata,
    vga_done,
    vga_irq,

    tim_addr,
    tim_read, tim_write,
    tim_rdata, tim_wdata,
    tim_done,
    tim_irq
);

string tracefile;
int limit;
int tracelevel = 0;         // Trace level
int tracefd;                // Trace file descriptor
int tty;                    // stdout descriptor for *71
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

    // Open display for *71 extracode.
    tty = 0;
    if ($test$plusargs("tty")) begin
        tty = $fopen("/dev/tty", "w");
        if (! tty) begin
            $display("Cannot open /dev/tty");
            $finish(1);
        end
    end

    // Dump waveforms.
    if ($test$plusargs("dump")) begin
        $dumpfile("output.vcd");
        $dumpvars();
    end

    // Enable detailed instruction trace to file.
    if ($value$plusargs("trace=%s", tracefile))
        tracelevel = 1;
    else if ($value$plusargs("utrace=%s", tracefile))
        tracelevel = 2;

    if (tracelevel) begin
        $display("Generate trace file %0S", tracefile);
        tracefd = $fopen(tracefile, "w");
    end

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
        $display("    +tty              Enable *71 output to /dev/tty");
        $display("");
        $finish(1);
    end
    load_oct(octfile);

    // Start with reset active
    clk = 1;
    reset = 1;
    //irq = 0;

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
            if (line[11] == "2" || line[11] == "3") begin
                word[43:39] = lop;
                word[38:24] = laddr;
            end else begin
                word[43:36] = lop;
                word[35:24] = laddr;
            end
            word[23:20] = rm;
            if (line[23] == "2" || line[23] == "3") begin
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
            if (tracelevel >= 2) begin
                // Print last executed micro-instruction
                print_uop();
            end

            // Print BESM instruction and changed registers
            print_insn_regs();

            // Print transactions on external bus
            print_ext_bus();

            if (cpu.irq)
                $fdisplay(tracefd, "(%0d) *** Interrupt", ctime);
        end
    end

    // Check for magic opcodes.
    if (fetch) begin
        if (opcode[19:15] == 'o33) begin
            // Stop.
            if (opcode == 'o33312345) begin
                // stop 12345(6) - success.
                terminate("Test PASS");
            end else begin
                // Fail in case of any other address/register.
                terminate("Test FAIL");
            end
        end
        if (~opcode[19] && opcode[17:12] == 'o71) begin
            extracode_71();
        end
    end

    if ($isunknown(cpu.upc)) begin
        $display("(%0d) Unknown microinstruction address: upc=%b", ctime, cpu.upc);
        if (tracefd)
            $fdisplay(tracefd, "(%0d) Unknown microinstruction address: upc=%b", ctime, cpu.upc);
        terminate("Fatal Error!");
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

    if (cpu.decode && ~cpu.irq && $isunknown(cpu.uentry)) begin
        $display("(%0d) Unknown address: cpu.uentry=%h", ctime, cpu.uentry);
        if (tracefd)
            $fdisplay(tracefd, "(%0d) *** Unknown address: cpu.uentry=%h", ctime, cpu.uentry);
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
        12:"B-A",      13:"|A|-|B|",  14:"A.sign.B", 15:"A+.exp.B",
        16:"A-.exp.B", 17:"A*B",      18:"A/B",      19:"YTA",
        default: "???"
    };
    static string acc_name[4] = '{
        0: "ALU",  1: "MEM",   2: "REG",  3: "RR"
    };
    static string md_name[8] = '{
        0: "PC",  1: "A",  2: "REG",  3: "R+1",
        4: "R-1", 5: "VA", 6: "UA",   7: "R+J"
    };
    static string mw_name[4] = '{
        0: "I",      1: "IMM",     2: "VA",  3: "UA"
    };
    static string mr_name[4] = '{
        0: "I",      1: "IMM",     2: "VA",  3: "UA"
    };
    static string rr_name[4] = '{
        0: "UA",     1: "MEM",     2: "REG", 3: "?3"
    };
    static string pc_name[8] = '{
        0: "UA",    1: "VA",  2: "REG", 3: "IMM",
        4: "PLUS1", 5: "?5",  6: "?6",  7: "?7"
    };

    logic [`UPC_BITS-1:0] imm;
    logic [5:0] alu_op;
    logic [1:0] sel_acc;
    logic [2:0] sel_md;
    logic [1:0] sel_mw;
    logic [1:0] sel_mr;
    logic [1:0] sel_rr;
    logic [2:0] sel_pc;
    logic       w_m;
    logic       sel_addr;
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
    logic       w_r;
    logic       cond_a_zero;
    logic       cond_a_nonzero;
    logic       cond_m_zero;
    logic       cond_m_nonzero;
    logic       branch;
    logic       c_active;
    logic       enter_interrupt;
    logic       exit_interrupt;
    logic       w_pc;

    assign imm                = uop[`P_IMM+`UPC_BITS-1:`P_IMM];
    assign alu_op             = uop[`P_ALU+5:`P_ALU];
    assign sel_acc            = uop[`P_SEL_ACC+1:`P_SEL_ACC];
    assign sel_md             = uop[`P_SEL_MD+2:`P_SEL_MD];
    assign sel_mw             = uop[`P_SEL_MW+1:`P_SEL_MW];
    assign sel_mr             = uop[`P_SEL_MR+1:`P_SEL_MR];
    assign sel_pc             = uop[`P_SEL_PC+2:`P_SEL_PC];
    assign sel_rr             = uop[`P_SEL_RR+1:`P_SEL_RR];
    assign w_m                = uop[`P_W_M];
    assign sel_addr           = uop[`P_SEL_ADDR];
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
    assign w_r                = uop[`P_W_RR];
    assign cond_a_zero        = uop[`P_A_ZERO];
    assign cond_a_nonzero     = uop[`P_A_NONZERO];
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
        cond_a_zero || cond_a_nonzero || cond_m_zero || cond_m_nonzero || branch)
        $fwrite(tracefd, " imm=%0d", imm);
    if (alu_op) $fwrite(tracefd, " alu=%0s",  op_name[alu_op]);
    if (w_a)    $fwrite(tracefd, " acc=%0s", acc_name[sel_acc]);
    if (w_m)    $fwrite(tracefd, " md=%0s",  md_name[sel_md]);
    if (w_m)    $fwrite(tracefd, " mw=%0s",  mw_name[sel_mw]);
    if (w_r)    $fwrite(tracefd, " rr=%0s",  rr_name[sel_rr]);
    if (sel_acc == `SEL_ACC_REG || sel_md == `SEL_MD_REG || sel_md == `SEL_MD_M_PLUS_J ||
        sel_md == `SEL_MD_REG_PLUS1 || sel_md == `SEL_MD_REG_MINUS1)
        $fwrite(tracefd, " mr=%0s",  mr_name[sel_mr]);
    if (w_pc) $fwrite(tracefd, " pc=%0s",  pc_name[sel_pc]);

    if (w_m)                $fwrite(tracefd, " w_m");
    if (sel_addr)           $fwrite(tracefd, " sel_addr");
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
    if (w_r)                $fwrite(tracefd, " w_r");
    if (cond_a_zero)        $fwrite(tracefd, " cond_a_zero");
    if (cond_a_nonzero)     $fwrite(tracefd, " cond_a_nonzero");
    if (cond_m_zero)        $fwrite(tracefd, " cond_m_zero");
    if (cond_m_nonzero)     $fwrite(tracefd, " cond_m_nonzero");
    if (branch)             $fwrite(tracefd, " branch");
    if (c_active)           $fwrite(tracefd, " c_active");
    if (enter_interrupt)    $fwrite(tracefd, " enter_interrupt");
    if (exit_interrupt)     $fwrite(tracefd, " exit_interrupt");
    if (w_pc)               $fwrite(tracefd, " w_pc");

    if (uop == 0) $fwrite(tracefd, " nop");

    if (busy)
        $fwrite(tracefd, " (busy)");
    $fdisplay(tracefd, "");

endtask

//
// Print BESM opcode and changed registers
//
task print_insn_regs();
    static string ir_name[32] = '{
        0:"M[0]",   1:"M[1]",   2:"M[2]",   3:"M[3]",
        4:"M[4]",   5:"M[5]",   6:"M[6]",   7:"M[7]",
        8:"M[8]",   9:"M[9]",  10:"M[10]", 11:"M[11]",
       12:"M[12]", 13:"M[13]", 14:"M[14]", 15:"SP",
       16:"K[0]",  17:"K[1]",  18:"K[2]",  19:"K[3]",
       20:"K[4]",  21:"K[5]",  22:"K[6]",  23:"K[7]",
       24:"K[8]",  25:"K[9]",  26:"K[10]", 27:"K[11]",
       28:"K[12]", 29:"K[13]", 30:"K[14]", 31:"KSP"
    };
    static logic [15:0] old_pc;
    static logic [47:0] old_acc, old_Y;
    static logic  [6:0] old_R;
    static logic [14:0] old_M[32], old_C;
    static logic        old_gie;

    // PC
`ifdef notdef
    if (tracelevel >= 2 && cpu.pc !== old_pc) begin
        $fdisplay(tracefd, "(%0d)        PC = %o:%b", ctime, cpu.pc[15:1], cpu.pc[0]);
        old_pc = cpu.pc;
    end
`endif

    //
    // Index-registers: print before opcode
    //
    for (int i=0; i<32; i+=1) begin
        if (cpu.M[i] !== old_M[i]) begin
            $fdisplay(tracefd, "(%0d)        %0s = %o",
                ctime, ir_name[i], cpu.M[i]);
            old_M[i] = cpu.M[i];
        end
    end

    // Print BESM opcode
    if (fetch)
        print_insn();

    // Accumulator
    if (cpu.acc !== old_acc) begin
        $fdisplay(tracefd, "(%0d)        A = %o %o %o %o",
            ctime, cpu.acc[47:36], cpu.acc[35:24],
            cpu.acc[23:12], cpu.acc[11:0]);
        old_acc = cpu.acc;
    end

    // Y register
    if (cpu.alu.rmr !== old_Y) begin
        $fdisplay(tracefd, "(%0d)        Y = %o %o %o %o",
            ctime, cpu.alu.rmr[47:36], cpu.alu.rmr[35:24],
            cpu.alu.rmr[23:12], cpu.alu.rmr[11:0]);
        old_Y = cpu.alu.rmr;
    end

    // C register
    if (cpu.C !== old_C || (uop[`P_W_C] & ~busy)) begin
        $fdisplay(tracefd, "(%0d)        C = %o", ctime, cpu.C);
        old_C = cpu.C;
    end

    // R register
    if (cpu.R !== old_R) begin
        $fwrite(tracefd, "(%0d)        R = %o (", ctime, cpu.R);
        if (cpu.R[6]) $fwrite(tracefd, "k,");
        if (cpu.R[5]) $fwrite(tracefd, "no-ovf,");
        casez (cpu.R[4:2])
            3'b1??:  $fwrite(tracefd, "add");
            3'b01?:  $fwrite(tracefd, "mul");
            3'b001:  $fwrite(tracefd, "log");
            default: $fwrite(tracefd, "undef");
        endcase
        if (cpu.R[1]) $fwrite(tracefd, ",no-round");
        if (cpu.R[0]) $fwrite(tracefd, ",no-norm");
        $fdisplay(tracefd, ")");
        old_R = cpu.R;
    end

    // Global Interrupt Enable
    if (cpu.gie !== old_gie) begin
        $fdisplay(tracefd, "(%0d)        GIE = %o", ctime, cpu.gie);
        old_gie = cpu.gie;
    end
endtask

//
// Print transactions on external bus: memory loads/stores/fetches etc.
//
task print_ext_bus();
    if (ibus_rd & ibus_done && tracelevel >= 2)
        $fdisplay(tracefd, "(%0d)        Fetch [%o] = %o %o %o %o",
            ctime, ibus_addr, ibus_input[47:36], ibus_input[35:24],
            ibus_input[23:12], ibus_input[11:0]);

    if (dbus_wr & dbus_done)
        $fdisplay(tracefd, "(%0d)        Store [%o] = %o %o %o %o",
            ctime, dbus_addr, dbus_output[47:36], dbus_output[35:24],
            dbus_output[23:12], dbus_output[11:0]);

    else if (dbus_rd & dbus_done)
        $fdisplay(tracefd, "(%0d)        Load [%o] = %o %o %o %o",
            ctime, dbus_addr, dbus_input[47:36], dbus_input[35:24],
            dbus_input[23:12], dbus_input[11:0]);
endtask

//
// Print BESM-6 instruction.
//
task print_insn();
    static string long_name[16] = '{
        0:"20", 1:"21",   2:"utc", 3:"wtc",   4:"vtm",  5:"utm",  6:"uza", 7:"u1a",
        8:"uj", 9:"vjm", 10:"ij",  11:"stop", 12:"vzm", 13:"v1m", 14:"36", 15:"vlm"
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

//
// Extracode *71.
//
task extracode_71();
    // Print line to stdout.
    automatic bit [14:0] addr = cpu.Uaddr;
    automatic bit [47:0] word = ram.mem[addr];

    if (~word == '0) begin
        // Get mask of attached terminals.
        cpu.acc = 48'h004000000000;
        return;
    end
    forever begin
        //$display("*71 Uaddr = %o, word = %o", cpu.Uaddr, word);
        //if (tracefd)
        //    $fdisplay(tracefd, "*71 Uaddr = %o, word = %o", cpu.Uaddr, word);

        if (word[42]) begin
            // Status/release.
            cpu.acc = 48'o1000_0002_0000_0012;
            return;
        end

        if (word[41:39] == 'b010) begin
            // Emit text to stdout.
            automatic bit [14:0] addr1 = cpu.M[word[47:44]] + word[35:24];
            automatic bit [14:0] addr2 = cpu.M[word[23:20]] + word[11:0];
            automatic bit [14:0] limit = addr1 + 324;
            automatic bit     raw_flag = word[36];

            if (addr2 >= addr1 && addr2 + 1 < limit)
                limit = addr2 + 1;

            //$display("--- output from %o, to %o, flag %o", addr1, addr2, raw_flag);
            //if (tracefd)
            //    $fdisplay(tracefd, "--- output from %o, to %o, flag %o", addr1, addr2, raw_flag);

            while (addr1 <= addr2) begin
                if (raw_flag) begin
                    tty_putw_raw(ram.mem[addr1]);
                end else begin
                    //TODO
                    //tty_putw(ram.mem[addr1]);
                end
                addr1 += 1;
            end
        end

        if (word[18])
            break;
        addr += 1;
        word = ram.mem[addr];
    end
endtask

//
// Print a symbol to stdout.
//
task tty_putc(bit [7:0] c);
    if (tty)
        $fwrite(tty, "%c", c);
    if (tracefd)
        $fwrite(tracefd, "%c", c);
endtask

//
// Print a string to stdout.
//
task tty_puts(string s);
    if (tty)
        $fwrite(tty, "%s", s);
    if (tracefd)
        $fwrite(tracefd, "%s", s);
endtask

//
// Print a word to stdout.
//
task tty_putw_raw(bit [47:0] word);
    int offset;

    for (offset = 40; offset >= 0; offset -= 8) begin
        automatic byte c = word >> offset;

        if (c == 0)
            break;

        /* Convert control chars to ANSI escapes */
        case (c[6:0])
        'o037:   tty_puts("\033[H\033[J");  // clrscr
        'o014:   tty_puts("\033[H");        // home
        'o031:   tty_puts("\033[A");        // up
        'o032:   tty_puts("\033[B");        // down
        'o030:   tty_puts("\033[C");        // right
        'o010:   tty_puts("\033[D");        // left
        'o140:   tty_puts("Ю");
        'o141:   tty_puts("А");
        'o142:   tty_puts("Б");
        'o143:   tty_puts("Ц");
        'o144:   tty_puts("Д");
        'o145:   tty_puts("Е");
        'o146:   tty_puts("Ф");
        'o147:   tty_puts("Г");
        'o150:   tty_puts("Х");
        'o151:   tty_puts("И");
        'o152:   tty_puts("Й");
        'o153:   tty_puts("К");
        'o154:   tty_puts("Л");
        'o155:   tty_puts("М");
        'o156:   tty_puts("Н");
        'o157:   tty_puts("О");
        'o160:   tty_puts("П");
        'o161:   tty_puts("Я");
        'o162:   tty_puts("Р");
        'o163:   tty_puts("С");
        'o164:   tty_puts("Т");
        'o165:   tty_puts("У");
        'o166:   tty_puts("Ж");
        'o167:   tty_puts("В");
        'o170:   tty_puts("Ь");
        'o171:   tty_puts("Ы");
        'o172:   tty_puts("З");
        'o173:   tty_puts("Ш");
        'o174:   tty_puts("Э");
        'o175:   tty_puts("Щ");
        'o176:   tty_puts("Ч");
        default: tty_putc(c[6:0]);
        endcase
    end
endtask

endmodule
