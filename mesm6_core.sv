`timescale 1ns / 1ps
`include "mesm6_defines.sv"

// all signals are polled on clk rising edge
// all signals positive

module mesm6_core (
    input  wire         clk,            // clock on rising edge
    input  wire         reset,          // reset on rising edge
    input  wire         interrupt,      // interrupt request
    output wire         mem_read,       // request memory read
    output wire         mem_write,      // request memory write
    input  wire         mem_done,       // memory operation completed
    output wire [14:0]  mem_addr,       // memory address
    input  wire [47:0]  mem_data_read,  // data readed
    output wire [47:0]  mem_data_write  // data written
);

// ------ datapath registers and connections -----------
reg  [15:0] pc;                     // program counter (half word align)
reg  [14:0] sp;                     // stack counter (word align)
reg  [47:0] a;                      // operand (address_out, data_out, alu_in)
reg  [47:0] b;                      // operand (address_out)
reg  [15:1] pc_cached;              // cached PC
reg  [47:0] opcode_cache;           // cached opcodes (current word)
wire [23:0] opcode;                 // opcode being processed

reg         int_requested;          // interrupt has been requested
reg         on_interrupt;           // serving interrupt
wire        exit_interrupt;         // microcode says this is poppc_interrupt
wire        enter_interrupt;        // microcode says we are entering interrupt

wire        sel_read;               // mux for data-in
wire  [1:0] sel_alu;                // mux for alu
wire  [1:0] sel_addr;               // mux for addr
wire        w_pc;                   // write PC
wire        w_sp;                   // write SP
wire        w_a;                    // write A (from ALU result)
wire        w_a_mem;                // write A (from MEM read)
wire        w_b;                    // write B
wire        w_op;                   // write OPCODE (opcode cache)
wire        is_op_cached = (pc[15:1] == pc_cached) ? 1'b1 : 1'b0;    // is opcode available?
wire        a_is_zero;              // A == 0
wire        a_is_neg;               // A[31] == 1
wire        busy;                   // busy signal to microcode sequencer (stalls cpu)

reg  [`UPC_BITS-1:0] upc;           // microcode PC
wire [`UOP_BITS-1:0] uop;           // current microcode operation

// memory addr / write ports
assign mem_addr = (sel_addr == `SEL_ADDR_SP) ? sp :
                  (sel_addr == `SEL_ADDR_A)  ? a  :
                  (sel_addr == `SEL_ADDR_B)  ? b  : pc[15:1];
assign mem_data_write = a;          // only A can be written to memory

// select left or right opcode from the cached opcode word
assign opcode = (pc[0] == 0) ? opcode_cache[47:24]
                             : opcode_cache[23:0];

// ------- alu instantiation -------
wire [47:0] alu_a;
wire [47:0] alu_b;
wire [47:0] alu_r;
wire [`ALU_OP_WIDTH-1:0] alu_op;
wire        alu_done;

// alu inputs multiplexors
// constant in microcode is sign extended (in order to implement substractions like adds)
assign alu_a = (sel_read == `SEL_READ_DATA) ? mem_data_read : mem_addr;
assign alu_b = (sel_alu == `SEL_ALU_CONST)  ? { {25{uop[`P_ADDR+6]}} , uop[`P_ADDR+6:`P_ADDR] } :    // most priority
               (sel_alu == `SEL_ALU_A)      ? a :
               (sel_alu == `SEL_ALU_B)      ? b : { {24{1'b0}} , opcode };    // `SEL_ALU_OPCODE is less priority

mesm6_alu alu(
    .alu_a      (alu_a),
    .alu_b      (alu_b),
    .alu_r      (alu_r),
    .alu_op     (alu_op),
    .clk        (clk),
    .done       (alu_done)
);

// -------- pc : program counter --------
always @(posedge clk)
begin
    if (w_pc)
        pc <= {alu_r, 1'b0};
end

// -------- sp : stack pointer --------
always @(posedge clk)
begin
    if (w_sp)
        sp <= alu_r;
end

// -------- a : acumulator register ---------
always @(posedge clk)
begin
    if (w_a)
        a <= alu_r;
    else if (w_a_mem)
        a <= mem_data_read;
end

// alu results over a register instead of alu result
// in order to improve speed
assign a_is_zero = (a == 0);
assign a_is_neg  = a[31];

// -------- b : auxiliary register ---------
always @(posedge clk)
begin
    if (w_b)
        b <= alu_r;
end

// -------- opcode and opcode_cache  --------
always @(posedge clk)
begin
    if (w_op) begin
        opcode_cache <= alu_r;          // store all opcodes in the word
        pc_cached <= pc[15:1];          // store PC address of cached opcodes
    end
end

// ------ on interrupt status bit -----
always @(posedge clk)
begin
    if (reset | exit_interrupt)
        on_interrupt <= 1'b0;
    else if (enter_interrupt)
        on_interrupt <= 1'b1;
end

// ------ microcode execution unit --------
assign sel_read  = uop[`P_SEL_READ];    // map datapath signals with microcode program bits
assign sel_alu   = uop[`P_SEL_ALU+1:`P_SEL_ALU];
assign sel_addr  = uop[`P_SEL_ADDR+1:`P_SEL_ADDR];
assign alu_op    = uop[`P_ALU+3:`P_ALU];
assign w_sp      = uop[`P_W_SP] & ~busy;
assign w_pc      = uop[`P_W_PC] & ~busy;
assign w_a       = uop[`P_W_A] & ~busy;
assign w_a_mem   = uop[`P_W_A_MEM] & ~busy;
assign w_b       = uop[`P_W_B] & ~busy;
assign w_op      = uop[`P_W_OPCODE] & ~busy;
assign mem_read  = uop[`P_MEM_R];
assign mem_write = uop[`P_MEM_W];

assign exit_interrupt  = uop[`P_EXIT_INT]  & ~busy;
assign enter_interrupt = uop[`P_ENTER_INT] & ~busy;

wire   cond_op_not_cached = uop[`P_OP_NOT_CACHED];    // conditional: true if opcode not cached
wire   cond_a_zero        = uop[`P_A_ZERO];           // conditional: true if A is zero
wire   cond_a_neg         = uop[`P_A_NEG];            // conditional: true if A is negative
wire   decode             = uop[`P_DECODE];           // decode means jumps to apropiate microcode based on besm6 opcode
wire   branch             = uop[`P_BRANCH];           // unconditional jump inside microcode

wire [`UPC_BITS-1:0] u_goto  = { uop[`P_ADDR+6:`P_ADDR], 2'b00 };   // microcode goto (goto = high 7 bits)
wire [`UPC_BITS-1:0] u_entry = { opcode[21:15], 2'b00 };            // microcode entry point for opcode TODO

wire cond_branch = (cond_op_not_cached & ~is_op_cached) |       // sum of all conditionals
                   (cond_a_zero & a_is_zero) |
                   (cond_a_neg & a_is_neg);

assign busy = ((mem_read | mem_write) & ~mem_done) | ~alu_done; // busy signal for microcode sequencer

// ------- handle interrupts ---------
always @(posedge clk) begin
    if (reset | on_interrupt)
        int_requested <= 0;

    else if (interrupt & ~on_interrupt & ~int_requested)
        int_requested <= 1;                         // interrupt requested
end

// Update microcode PC address
always @(posedge clk) begin
    if (reset) begin
        upc <= `UADDR_RESET - 1;            // reset vector

    end else if (~busy) begin
        // get next microcode instruction
        if (branch | cond_branch) begin
            upc <= u_goto;                  // jump to mucrocode address

        end else if (decode) begin
            // decode: entry point of a new besm6 opcode
            if (int_requested)
                upc <= `UADDR_INTERRUPT;    // enter interrupt mode
            else
                upc <= u_entry;             // jump to routine to emulate a given besm6 opcode

        end else begin
            upc <= upc + 1;                 // default, next microcode instruction
        end
    end
end

// ----- microcode program ------
mesm6_rom microcode(
    .addr   (upc),
    .data   (uop),
    .clk    (clk)
);

endmodule
