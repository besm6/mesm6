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
`define RESET_VECTOR            'o00001     // reset vector
`define INTERRUPT_VECTOR        'o00010     // interrupt & exception vectors

// Accumulator source selector
`define SEL_ACC_ALU             0   // from ALU
`define SEL_ACC_MEM             1   // from memory
`define SEL_ACC_REG             2   // M[i]
`define SEL_ACC_RR              3   // R register
`define SEL_ACC_Y               4   // Y register

// M[i] read index selector
`define SEL_MR_REG              0
`define SEL_MR_IMM              1
`define SEL_MR_VA               2
`define SEL_MR_UA               3

// M[i] write index selector
`define SEL_MW_REG              0
`define SEL_MW_IMM              1
`define SEL_MW_VA               2
`define SEL_MW_UA               3

// M[i] write data selector
`define SEL_MD_PC               0
`define SEL_MD_A                1
`define SEL_MD_REG              2
`define SEL_MD_REG_PLUS1        3
`define SEL_MD_REG_MINUS1       4
`define SEL_MD_VA               5
`define SEL_MD_UA               6

// PC source selector
`define SEL_PC_UA               0   // from Uaddr
`define SEL_PC_VA               1   // from Vaddr
`define SEL_PC_REG              2   // from M[i]
`define SEL_PC_IMM              3   // from immediate
`define SEL_PC_PLUS1            4   // pc + 1

// ALU operations
`define ALU_NOP                 0   // r = a
`define ALU_NOP_B               1   // r = b
`define ALU_PLUS                2   // r = a + b
`define ALU_PLUS_OFFSET         3   // r = a + { 27'b0, ~b[4], b[3:0] }
`define ALU_AND                 4   // r = a AND b
`define ALU_OR                  5   // r = a OR b
`define ALU_NOT                 6   // r = NOT a

`define ALU_OP_WIDTH            6   // alu operation is 6 bits

// Microcode memory settings
`define UPC_BITS                9   // microcode address width
`define UOP_BITS                50  // microcode opcode width

// Dedicated microcode addresses
`define UADDR_RESET             0   // start from zero
`define UADDR_NOP               22  // no operation
`define UADDR_INTERRUPT         23  // defined by mesm6_microcode.sv at runtime

// Micro-instruction fields
`define P_IMM                   0   // microcode address (9 bits) or constant to be used at microcode level
`define P_ALU                   9   // alu operation (6 bits)
`define P_SEL_ACC               15  // accumulator multiplexor (3 bits)
`define P_SEL_MD                18  // M[i] write data multiplexor (3 bits)
`define P_SEL_MW                21  // M[i] write address multiplexor (2 bits)
`define P_W_M                   23  // write M[i] (from alu-out)
`define P_SEL_MR                24  // M read address multiplexor (2 bits)
`define P_M_NONZERO             26  // microcode branch if M[i] is non-zero
`define P_SEL_PC                27  // PC multiplexor (3 bits)
`define P_SEL_ADDR              30  // addr-out multiplexor between Uaddr and M[i]
`define P_SEL_J_ADD             31  // use M[j] for Uaddr instead of Vaddr
`define P_SEL_C_MEM             32  // use memory output for C instead of Uaddr
`define P_OP_NOT_CACHED         33  // microcode branch if byte[pc] is not cached at opcode
`define P_FETCH                 34  // request instruction fetch
`define P_W_OPCODE              35  // write opcode
`define P_DECODE                36  // jump to microcode entry point based on current opcode
`define P_MEM_R                 37  // request memory read
`define P_MEM_W                 38  // request memory write
`define P_W_A                   39  // write accumulator (from alu-out)
`define P_W_C                   40  // write C register
`define P_A_NONZERO             41  // microcode branch if acc is zero
`define P_A_NEG                 42  // microcode branch if acc is negative (acc[41] = 1)
`define P_A_ZERO                43  // microcode branch if acc is zero
`define P_BRANCH                44  // microcode inconditional branch to address
`define P_C_ACTIVE              45  // clear C register flag
`define P_ENTER_INT             46  // set interrupt flag (enter interrupt)
`define P_EXIT_INT              47  // clear interrupt flag (exit from interrupt)
`define P_W_PC                  48  // write pc (from alu-out)
`define P_M_ZERO                49  // microcode branch if M[i] is zero
