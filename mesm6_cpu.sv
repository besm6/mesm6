//
// MESM-6 processor
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
`timescale 1ns / 1ps
`include "mesm6_defines.sv"

// all signals are polled on clk rising edge
// all signals positive

module mesm6_core (
    input  wire         clk,            // clock on rising edge
    input  wire         reset,          // reset on rising edge
    input  wire         interrupt,      // interrupt request

    // Instruction memory bus.
    output wire         ibus_fetch,     // request instruction fetch
    output wire [14:0]  ibus_addr,      // memory address
    input  wire [47:0]  ibus_input,     // instruction word read
    input  wire         ibus_done,      // memory operation completed

    // Data memory bus.
    output wire         dbus_read,      // request data read
    output wire         dbus_write,     // request data write
    output wire [14:0]  dbus_addr,      // memory address
    output wire [47:0]  dbus_output,    // data word written
    input  wire [47:0]  dbus_input,     // data word read
    input  wire         dbus_done       // memory operation completed
);

//--------------------------------------------------------------
// Datapath registers and connections.
//
reg  [15:0] pc;                     // program counter (half word granularity)
reg  [47:0] acc;                    // A: accumulator
reg  [47:0] lsb;                    // Y: least significant bits
reg  [14:0] M[16];                  // M1-M15: index registers (modifiers)
reg  [14:0] amod;                   // C: address modifier (m16)
reg  [5:0]  rr;                     // R: rounding mode register
reg  [15:1] pc_cached;              // cached PC
reg  [47:0] opcode_cache;           // cached instruction word
wire [23:0] opcode;                 // opcode being processed
wire [14:0] exaddr;                 // executive address (opcode.addr + mN)
wire [14:0] sp;                     // stack pointer (m15)

reg         int_requested;          // interrupt has been requested
reg         on_interrupt;           // serving interrupt
wire        exit_interrupt;         // microcode says this is poppc_interrupt
wire        enter_interrupt;        // microcode says we are entering interrupt

wire        sel_read;               // mux for data-in
wire  [1:0] sel_alu;                // mux for alu
wire  [1:0] sel_addr;               // mux for addr
wire        w_pc;                   // write PC
wire        w_m;                    // write M[i]
wire        w_acc;                  // write A (from ALU result)
wire        w_acc_mem;              // write A (from MEM read)
wire        w_lsb;                  // write Y
wire        w_opcode;               // write OPCODE (opcode cache)
wire        is_op_cached;           // is opcode available?
wire        acc_is_zero;            // A == 0
wire        acc_is_neg;             // A[41] == 1
wire        busy;                   // busy signal to microcode sequencer (stalls cpu)

reg  [`UPC_BITS-1:0] upc;           // microcode PC
reg  [`UOP_BITS-1:0] uop;           // current microcode operation
wire [`UPC_BITS-1:0] uop_imm;       // immediate field of microinstruction

// Opcode fields.
wire [3:0]  op_ir    = opcode[23:20];   // index register
wire        op_lflag = opcode[19];      // long format versus short format flag
wire [3:0]  op_lcmd  = opcode[18:15];   // long format instruction: 020-037
wire [5:0]  op_scmd  = opcode[17:12];   // short format instruction: 000-077
wire [14:0] op_addr  =                  // address field
                op_lflag ? opcode[14:0]
                         : {{3{opcode[18]}}, opcode[11:0]};

// memory addr / write ports
assign ibus_addr   = pc[15:1];
assign dbus_addr   = (sel_addr == `SEL_ADDR_SP) ? sp : exaddr;
assign dbus_output = acc;        // only A can be written to memory

// select left or right opcode from the cached opcode word
assign opcode = (pc[0] == 0) ? opcode_cache[47:24]
                             : opcode_cache[23:0];

// Executive address.
assign exaddr = op_addr + M[op_ir];

//--------------------------------------------------------------
// ALU instantiation.
//
wire [47:0] alu_a;
wire [47:0] alu_b;
wire [47:0] alu_r;
wire [`ALU_OP_WIDTH-1:0] alu_op;
wire        alu_done;

// alu inputs multiplexors
// constant in microcode is sign extended (in order to implement substractions like adds)
assign alu_a = (sel_read == `SEL_READ_DATA) ? dbus_input : dbus_addr;
assign alu_b = (sel_alu == `SEL_ALU_CONST)  ? { {39{uop_imm[`UPC_BITS-1]}}, uop_imm } : // most priority
               (sel_alu == `SEL_ALU_A)      ? acc :
               (sel_alu == `SEL_ALU_B)      ? lsb : { {24{1'b0}} , opcode };    // `SEL_ALU_OPCODE is less priority

mesm6_alu alu(
    .alu_a      (alu_a),
    .alu_b      (alu_b),
    .alu_r      (alu_r),
    .alu_op     (alu_op),
    .clk        (clk),
    .done       (alu_done)
);

//--------------------------------------------------------------
// PC: program counter.
//
always @(posedge clk)
begin
    if (w_pc)
        pc <= {alu_r, 1'b0};
end

//--------------------------------------------------------------
// Accumulator register.
//
always @(posedge clk)
begin
    if (w_acc)
        acc <= alu_r;
    else if (w_acc_mem)
        acc <= dbus_input;
end

// alu results over A register instead of alu result
// in order to improve speed
assign acc_is_zero = (acc == 0);
assign acc_is_neg  = acc[31];

//--------------------------------------------------------------
// Y register: least significant bits of mantissa.
//
always @(posedge clk)
begin
    if (w_lsb)
        lsb <= alu_r;
end

//--------------------------------------------------------------
// Modifiers M1-M15.
//
always @(posedge clk)
begin
    if (reset)
        M[0] <= 0;
    else if (w_m)
        M[op_ir] <= (op_ir == 0) ? 0 : alu_r;
end

//--------------------------------------------------------------
// Instruction opcode and opcode_cache.
//
always @(posedge clk)
begin
    if (w_opcode) begin
        opcode_cache <= ibus_input;     // store all opcodes in the word
        pc_cached <= pc[15:1];          // store PC address of cached opcodes
    end
end
assign is_op_cached = (pc[15:1] == pc_cached) ? 1'b1 : 1'b0;

//--------------------------------------------------------------
// Handle interrupts.
//
always @(posedge clk) begin
    if (reset | on_interrupt)
        int_requested <= 0;

    else if (interrupt & ~on_interrupt & ~int_requested)
        int_requested <= 1;                         // interrupt requested
end

//--------------------------------------------------------------
// `On interrupt' status bit.
//
always @(posedge clk)
begin
    if (reset | exit_interrupt)
        on_interrupt <= 1'b0;
    else if (enter_interrupt)
        on_interrupt <= 1'b1;
end

//--------------------------------------------------------------
// Microinstruction ROM.
//
logic [`UOP_BITS-1:0] uop_rom[(1<<`UPC_BITS)-1:0] = '{
    `include "microcode.v"
    default: 0
};

//--------------------------------------------------------------
// Tables of entry addresses per opcode.
//
logic [`UPC_BITS-1:0] entry16[16] = '{
    `include "jumptab16.v"
    default: 0
};

logic [`UPC_BITS-1:0] entry64[64] = '{
    `include "jumptab64.v"
    default: 0
};

//--------------------------------------------------------------
// Microcode execution unit.
//
assign sel_read   = uop[`P_SEL_READ];    // map datapath signals with microcode program bits
assign sel_alu    = uop[`P_SEL_ALU+1:`P_SEL_ALU];
assign sel_addr   = uop[`P_SEL_ADDR+1:`P_SEL_ADDR];
assign alu_op     = uop[`P_ALU+3:`P_ALU];
assign w_m        = uop[`P_W_M] & ~busy;
assign w_pc       = uop[`P_W_PC] & ~busy;
assign w_acc      = uop[`P_W_A] & ~busy;
assign w_acc_mem  = uop[`P_W_A_MEM] & ~busy;
//assign w_lsb      = uop[`P_W_Y] & ~busy;
assign w_opcode   = uop[`P_W_OPCODE] & ~busy;
assign ibus_fetch = uop[`P_FETCH];
assign dbus_read  = uop[`P_MEM_R];
assign dbus_write = uop[`P_MEM_W];
assign uop_imm    = uop[`P_IMM+`UPC_BITS-1:`P_IMM];

assign exit_interrupt  = uop[`P_EXIT_INT]  & ~busy;
assign enter_interrupt = uop[`P_ENTER_INT] & ~busy;

wire   cond_op_not_cached = uop[`P_OP_NOT_CACHED];  // conditional: true if opcode not cached
wire   cond_acc_zero      = uop[`P_A_ZERO];         // conditional: true if A is zero
wire   cond_acc_neg       = uop[`P_A_NEG];          // conditional: true if A is negative
wire   decode             = uop[`P_DECODE];         // decode means jumps to apropiate microcode based on besm6 opcode
wire   uncond_branch      = uop[`P_BRANCH];         // unconditional jump inside microcode

// Branch flag: sum of all conditionals and unconditionals.
wire branch = (cond_op_not_cached & ~is_op_cached) |
              (cond_acc_zero & acc_is_zero) |
              (cond_acc_neg & acc_is_neg) |
              uncond_branch;

// Busy signal for microcode sequencer.
assign busy = ((dbus_read | dbus_write) & ~dbus_done) |
              (ibus_fetch & ~ibus_done) |
              ~alu_done;

// Next microcode PC address.
wire [`UPC_BITS-1:0] upc_next = reset ? `UADDR_RESET - 1 :  // reset vector
                                 busy ? upc :               // stall
                               branch ? uop_imm :           // jump to mucrocode address
                              ~decode ? upc + 1 :           // next microcode instruction (default)
                        int_requested ? `UADDR_INTERRUPT :  // enter interrupt mode
                             op_lflag ? entry16[op_lcmd] :  // entry for long format opcode
                                        entry64[op_scmd];   // entry for short format opcode
always @(posedge clk)
    upc <= upc_next;

// Microinstruction.
always @(posedge clk)
    if (reset)
        uop <= '0;
    else
        uop <= uop_rom[upc];

endmodule
