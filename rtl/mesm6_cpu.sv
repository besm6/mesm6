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
`default_nettype none
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
// Microcode execution unit.
//
reg  [`UPC_BITS-1:0] upc;           // microcode PC
reg  [`UOP_BITS-1:0] uop;           // current microcode operation
wire [`UPC_BITS-1:0] uentry;        // entry for current opcode
reg                  irq;           // interrupt has been requested
wire                 busy;          // busy signal to microcode sequencer (stalls cpu)

wire [`UPC_BITS-1:0]     uop_imm = uop[`P_IMM+`UPC_BITS-1:`P_IMM];  // immediate field of microinstruction
wire [`ALU_OP_WIDTH-1:0] alu_op  = uop[`P_ALU+5:`P_ALU];            // ALU operation

wire [2:0] sel_pc   = uop[`P_SEL_PC+2:`P_SEL_PC];   // mux for pc
wire [1:0] sel_mr   = uop[`P_SEL_MR+1:`P_SEL_MR];   // mux for M[i] read address
wire [1:0] sel_mw   = uop[`P_SEL_MW+1:`P_SEL_MW];   // mux for M[i] write address
wire [2:0] sel_md   = uop[`P_SEL_MD+2:`P_SEL_MD];   // mux for M[i] write data
wire [2:0] sel_acc  = uop[`P_SEL_ACC+2:`P_SEL_ACC]; // mux for A write data

wire   sel_addr     = uop[`P_SEL_ADDR];             // mux for data address
wire   sel_c_mem    = uop[`P_SEL_C_MEM];            // use memory output for C instead of Uaddr
wire   sel_alu_mem  = uop[`P_SEL_ALU_MEM];          // use memory output for ALU input B instead of Uaddr
wire   c_active     = uop[`P_C_ACTIVE];             // use C register
wire   r_add        = uop[`P_R_ADD];                // use Mr for Uaddr instead of Vaddr
wire   w_pc         = uop[`P_W_PC] & ~busy;         // write PC
wire   w_m          = uop[`P_W_M] & ~busy;          // write M[i]
wire   w_acc        = uop[`P_W_A] & ~busy;          // write A (from ALU result)
wire   w_c          = uop[`P_W_C] & ~busy;          // write C
wire   w_opcode     = uop[`P_W_OPCODE] & ~busy;     // write OPCODE (opcode cache)

wire   exit_interrupt  = uop[`P_EXIT_INT]  & ~busy; // enable interrupts
wire   enter_interrupt = uop[`P_ENTER_INT] & ~busy; // disable interrupts

wire   cond_op_not_cached = uop[`P_OP_NOT_CACHED];  // conditional: true if opcode not cached
wire   cond_acc_zero      = uop[`P_A_ZERO];         // conditional: true if A is zero
wire   cond_acc_nonzero   = uop[`P_A_NONZERO];      // conditional: true if A is non-zero
wire   cond_m_zero        = uop[`P_M_ZERO];         // conditional: true if M[i] is zero
wire   cond_m_nonzero     = uop[`P_M_NONZERO];      // conditional: true if M[i] is non-zero
wire   cond_acc_neg       = uop[`P_A_NEG];          // conditional: true if A is negative
wire   decode             = uop[`P_DECODE] & ~busy; // decode means jumps to apropiate microcode based on besm6 opcode
wire   uncond_branch      = uop[`P_BRANCH];         // unconditional jump inside microcode

assign ibus_fetch = uop[`P_FETCH];
assign dbus_read  = uop[`P_MEM_R];
assign dbus_write = uop[`P_MEM_W];

// Branch conditions.
wire   is_op_cached;                // is opcode available?
wire   acc_is_zero;                 // A == 0
wire   acc_is_neg;                  // A[41] == 1
wire   Mi_is_zero;                  // M[i] == 0
wire   alu_done;

// Branch flag: sum of all conditionals and unconditionals.
wire branch = (cond_op_not_cached & ~is_op_cached) |
              (cond_acc_zero & acc_is_zero) | (cond_acc_nonzero & !acc_is_zero) |
              (cond_acc_neg & acc_is_neg) |
              (cond_m_zero & Mi_is_zero) | (cond_m_nonzero & !Mi_is_zero) |
              uncond_branch;

// Busy signal for microcode sequencer.
assign busy = ((dbus_read | dbus_write) & ~dbus_done) |
              (ibus_fetch & ~ibus_done) |
              ((alu_op != `ALU_NOP) & ~alu_done);

// Next microcode PC address.
wire [`UPC_BITS-1:0] upc_next = reset ? `UADDR_RESET - 1 :  // reset vector
                                 busy ? upc :               // stall
                               branch ? uop_imm :           // jump to mucrocode address
                              ~decode ? upc + 1 :           // next microcode instruction (default)
                                  irq ? `UADDR_INTERRUPT :  // enter interrupt mode
                                        uentry;             // entry for current opcode
always @(posedge clk)
    upc <= upc_next;

// Microinstruction ROM.
logic [`UOP_BITS-1:0] uop_rom[(1<<`UPC_BITS)-1:0] = '{
    `include "microcode.v"
    default: 0
};

// Microinstruction.
always @(posedge clk)
    if (reset)
        uop <= '0;
    else if (~busy)
        uop <= uop_rom[upc_next];

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
// Datapath registers and connections.
//
reg  [15:0] pc;                     // program counter (half word granularity)
reg  [47:0] acc;                    // A: accumulator
wire [47:0] Y;                      // Y: least significant bits
reg  [14:0] M[16];                  // M1-M15: index registers (modifiers)
reg  [14:0] C;                      // C: address modifier
reg  [5:0]  R;                      // R: rounding mode register
reg  [15:1] pc_cached;              // cached PC
reg  [47:0] opcode_cache;           // cached instruction word

wire [23:0] opcode;                 // opcode being processed
reg  [14:0] Vaddr;                  // full address, latched (opcode.addr + C)
wire [14:0] Uaddr;                  // executive address (opcode.addr + C + M[i])
reg  [3:0]  reg_index;              // index of M register, latched
wire [14:0] Mi;                     // output of M[reg_index] read
wire [14:0] Mr;                     // output of M[m_ra] read

reg         gie;                    // global interrupt enable

// Opcode fields.
wire [3:0]  op_ir    = opcode[23:20];   // index register
wire        op_lflag = opcode[19];      // long format versus short format flag
wire [3:0]  op_lcmd  = opcode[18:15];   // long format instruction: 020-037
wire [5:0]  op_scmd  = opcode[17:12];   // short format instruction: 000-077
wire [14:0] op_addr  =                  // address field
                op_lflag ? opcode[14:0]
                         : {{3{opcode[18]}}, opcode[11:0]};

wire op_utc0 = (opcode == 'o02200000) &         // utc 0(0)
               !c_active;
wire op_xta0 = ((opcode == 'o00100000) |        // xta 0(0)
                (opcode == 'o00420000)) &       // ita 0(0)
               !c_active;

assign uentry = op_utc0 ? `UADDR_NOP :          // fast utc 0(0)
                op_xta0 ? `UADDR_NOP :          // fast xta 0(0) or ita 0(0)
               op_lflag ? entry16[op_lcmd] :    // entry for long format opcode
                          entry64[op_scmd];     // entry for short format opcode

// memory addr / write ports
assign ibus_addr   = pc[15:1];
assign dbus_addr   = sel_addr ? Mr : Uaddr;
assign dbus_output = acc;                       // only A can be written to memory

// select left or right opcode from the cached opcode word
assign opcode = (pc[0] == 0) ? opcode_cache[47:24]
                             : opcode_cache[23:0];

// Full address: latch it on decode.
always @(posedge clk) begin
    if (decode) begin
        Vaddr <= c_active ? op_addr + C         // address modified by C
                          : op_addr;            // address field
        reg_index <= op_ir;                     // index register
    end
end

// Executive address.
assign Uaddr = Mi + (r_add ? Mr : Vaddr);

//--------------------------------------------------------------
// ALU instantiation.
//
wire [47:0] alu_b;
wire [47:0] alu_result;

// alu B input multiplexor
assign alu_b = sel_alu_mem ? dbus_input : Uaddr;

mesm6_alu alu(
    .a      (acc),
    .b      (alu_b),
    .result (alu_result),
    .y      (Y),
    .op     (alu_op),
    .clk    (clk),
    .done   (alu_done)
);

//--------------------------------------------------------------
// PC: program counter.
//
wire [15:0] pc_plus_1 = pc + 1;

always @(posedge clk) begin
    if (w_pc)
        pc <= (sel_pc == `SEL_PC_IMM)   ? {uop_imm, 1'b0} : // constant
              (sel_pc == `SEL_PC_REG)   ? {Mi, 1'b0} :      // M[i]
              (sel_pc == `SEL_PC_PLUS1) ? pc_plus_1 :       // pc + 1
              (sel_pc == `SEL_PC_VA)    ? {Vaddr, 1'b0} :   // addr + C
                        /*SEL_PC_UA*/     {Uaddr, 1'b0};    // addr + C + M[i]
    else if (decode & is_op_cached)
        pc <= pc_plus_1;                                    // decode increments PC
end

//--------------------------------------------------------------
// C address modifier.
//
always @(posedge clk) begin
    if (w_c)
        C <= sel_c_mem ? dbus_input :   // from memory
                         Uaddr;         // addr + C + M[i]
end

//--------------------------------------------------------------
// Accumulator register.
//
always @(posedge clk) begin
    if (w_acc)
        acc <= (sel_acc == `SEL_ACC_MEM) ? dbus_input : // from memory
               (sel_acc == `SEL_ACC_REG) ? Mr :         // M[m_ra]
               (sel_acc == `SEL_ACC_RR)  ? R :          // R register
               (sel_acc == `SEL_ACC_Y)   ? Y :          // Y register
                          /*SEL_ACC_ALU*/  alu_result;  // from ALU
    else if (decode & op_xta0)
        acc <= 0;
end

// Status of accumulator.
assign acc_is_zero = (acc == 0);
assign acc_is_neg  = acc[40];

//--------------------------------------------------------------
// Modifiers M1-M15.
//
wire [14:0] M1 = M[1];                  // aliases for gtkwave
wire [14:0] M2 = M[2];
wire [14:0] M3 = M[3];
wire [14:0] M4 = M[4];
wire [14:0] M5 = M[5];
wire [14:0] M6 = M[6];
wire [14:0] M7 = M[7];
wire [14:0] M8 = M[8];
wire [14:0] M9 = M[9];
wire [14:0] M10 = M[10];
wire [14:0] M11 = M[11];
wire [14:0] M12 = M[12];
wire [14:0] M13 = M[13];
wire [14:0] M14 = M[14];
wire [14:0] M15 = M[15];

// Read address.
wire [14:0] m_ra = (sel_mr == `SEL_MR_IMM) ? uop_imm :      // constant
                   (sel_mr == `SEL_MR_VA)  ? Vaddr :        // addr + C
                   (sel_mr == `SEL_MR_UA)  ? Uaddr :        // addr + C + M[i]
                             /*SEL_MR_REG*/  reg_index;     // opcode[24:21] latched
// Write address.
wire [14:0] m_wa = (sel_mw == `SEL_MW_IMM) ? uop_imm :      // constant
                   (sel_mw == `SEL_MW_VA)  ? Vaddr :        // addr + C
                   (sel_mw == `SEL_MW_UA)  ? Uaddr :        // addr + C + M[i]
                             /*SEL_MW_REG*/  reg_index;     // opcode[24:21] latched
// Read results.
assign Mi = M[reg_index];
assign Mr = M[m_ra];
assign Mi_is_zero = (Mi == 0);

always @(posedge clk) begin
    if (w_m)
        M[m_wa] <= (m_wa == 0)                    ? 0 :         // M[0]
                   (sel_md == `SEL_MD_PC)         ? pc[15:1] :  // PC
                   (sel_md == `SEL_MD_A)          ? acc :       // accumulator
                   (sel_md == `SEL_MD_REG)        ? Mr :        // M[m_ra]
                   (sel_md == `SEL_MD_REG_PLUS1)  ? Mr + 1 :    // M[m_ra] + 1
                   (sel_md == `SEL_MD_REG_MINUS1) ? Mr - 1 :    // M[m_ra] - 1
                   (sel_md == `SEL_MD_VA)         ? Vaddr :     // addr + C
                             /*SEL_MD_UA*/          Uaddr;      // addr + C + M[i]
end

//--------------------------------------------------------------
// Instruction opcode and opcode_cache.
//
always @(posedge clk) begin
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
    if (reset)
        irq <= 0;
    else
        irq <= interrupt & gie;         // interrupt requested
end

//--------------------------------------------------------------
// `On interrupt' status bit.
//
always @(posedge clk)
begin
    if (reset | enter_interrupt)
        gie <= 0;
    else if (exit_interrupt)
        gie <= 1;
end

endmodule
