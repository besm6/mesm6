`timescale 1ns / 1ps
`include "mesm6_defines.sv"

`define READ_DATA               (`SEL_READ_DATA << `P_SEL_READ)     // 1 bit
`define READ_ADDR               (`SEL_READ_ADDR << `P_SEL_READ)

`define ALU_A                   (`SEL_ALU_A << `P_SEL_ALU)          // 2 bit
`define ALU_OPCODE              (`SEL_ALU_OPCODE << `P_SEL_ALU)
`define ALU_CONST(addr)         (`SEL_ALU_CONST << `P_SEL_ALU | (addr) << `P_ADDR)
`define ALU_B                   (`SEL_ALU_B << `P_SEL_ALU)

`define ADDR_PC                 (`SEL_ADDR_PC << `P_SEL_ADDR)       // 2 bits
`define ADDR_SP                 (`SEL_ADDR_SP << `P_SEL_ADDR)
`define ADDR_A                  (`SEL_ADDR_A <<  `P_SEL_ADDR)
`define ADDR_B                  (`SEL_ADDR_B << `P_SEL_ADDR)

`define NOP                     (`ALU_NOP << `P_ALU)                // 4 bits
`define NOP_B                   (`ALU_NOP_B << `P_ALU)
`define PLUS                    (`ALU_PLUS << `P_ALU)
`define AND                     (`ALU_AND << `P_ALU)
`define OR                      (`ALU_OR << `P_ALU)
`define NOT                     (`ALU_NOT << `P_ALU)
`define PLUS_OFFSET             (`ALU_PLUS_OFFSET << `P_ALU)

`define W_SP                    (1 << `P_W_SP)
`define W_PC                    (1 << `P_W_PC)
`define W_A                     (1 << `P_W_A)
`define W_A_MEM                 (1 << `P_W_A_MEM)
`define W_B                     (1 << `P_W_B)
`define W_OPCODE                (1 << `P_W_OPCODE)
`define EXIT_INTERRUPT          (1 << `P_EXIT_INT)
`define ENTER_INTERRUPT         (1 << `P_ENTER_INT)

`define MEM_R                   (1 << `P_MEM_R)
`define MEM_W                   (1 << `P_MEM_W)

`define DECODE                      (1 << `P_DECODE)
`define BRANCH(addr)                (1 << `P_BRANCH | ((addr) >> 2) << `P_ADDR)
`define BRANCHIF_OP_NOT_CACHED(a)   (1 << `P_OP_NOT_CACHED | ((a) >> 2) << `P_ADDR)
`define BRANCHIF_A_ZERO(addr)       (1 << `P_A_ZERO | ((addr) >> 2) << `P_ADDR)
`define BRANCHIF_A_NEG(addr)        (1 << `P_A_NEG | ((addr) >> 2) << `P_ADDR)

// microcode common operations

`define PC_PLUS_1               (`ADDR_PC | `READ_ADDR | `ALU_CONST(1) | `PLUS  | `W_PC)
`define SP_MINUS_4              (`ADDR_SP | `READ_ADDR | `ALU_CONST(-4 & 127) | `PLUS | `W_SP)
`define SP_PLUS_4               (`ADDR_SP | `READ_ADDR | `ALU_CONST(4) | `PLUS  | `W_SP)

`define FETCH                   (`BRANCHIF_OP_NOT_CACHED(`UADDR_FETCH) | `DECODE) // fetch and decode current PC opcode
`define GO_NEXT                 `BRANCH(`UADDR_FETCH_NEXT)  // go to next opcode (PC=PC+1, fetch, decode)
`define GO_FETCH                `BRANCH(`UADDR_FETCH)       // go to fetch opcode at PC, then decode
`define GO_BREAKPOINT           `BRANCH(0)                  // go to breakpoint opcode

module mesm6_rom (
    input  wire                 clk,
    input  wire [`UPC_BITS-1:0] addr,
    output reg  [`UOP_BITS-1:0] data
);

reg [`UOP_BITS-1:0] memory[(1<<`UPC_BITS)-1:0];

initial data <= 0;
always @(posedge clk)
    data <= memory[addr];

// --- clear all memory at startup; for any reason, xilinx xst
// will not syntetize as block ram if not all memory is initialized ---
integer n;
initial begin
// initialize all memory array
for(n = 0; n < (1<<`UPC_BITS); n = n + 1) memory[n] = 0;

// ------------------------- MICROCODE MEMORY START -----------------------------------

// As per mesm6_core.v, each opcode is executed by microcode. Each opcode microcode entry point
// is at <opcode> << 2 (example pushsp = 0x02 has microcode entry point of 0x08); this leaves
// room of 4 microcode operations per opcode; if the opcode microcode needs more space,
// it can jump & link to other microcode address (with the two lower bits at 0). The lower 256 addresses
// of microcode memory are entry points and code for 0..127 opcodes; other specific opcodes like im, storesp, etc.
// are directly hardwired to specific microcode addresses at the memory end. Upper 256 addresses are
// used by microcode continuation (eg. opcodes which needs more microcode operations), entry points, initializations, etc.
// the idea is to fit the microcode program in a xilinx blockram 512x36.

// ----- OPCODES WITHOUT CONSTANT ------

// 0000_0000 (00) breakpoint -------------------------------------
memory[0] = `NOP_B | `ALU_CONST(4) | `W_B;                          // b = 4 (#1 in emulate table)
memory[1] = `BRANCH(`UADDR_EMULATE);                                // emulate #1 (exception)

// 0000_0001 (01) shiftleft  -------------------------------------
memory[4] = `GO_BREAKPOINT;

// 0000_0010 (02) pushsp  -------------------------------------
//    mem[sp-1] = sp
//    sp = sp - 1
memory[8] = `ADDR_SP | `READ_ADDR | `NOP | `W_A;                    // a = sp
memory[9] = `SP_MINUS_4;                                            // sp = sp - 1
memory[10] = `ADDR_SP | `MEM_W | `GO_NEXT;                          // mem[sp]=a

// 0000_0011 (03) popint  -------------------------------------
// pc=mem[sp]-1        (emulate stores pc+1 but we must return to
// sp=sp+1             pc because interrupt takes precedence to decode)
// fetch & decode, then clear_interrupt_flag
// this guarantees that a continous interrupt allows to execute at least one
// opcode of mainstream program before reentry to interrupt handler
memory[12] = `ADDR_SP | `READ_DATA | `MEM_R | `PLUS | `W_PC |       // pc = mem[sp]-1
             `ALU_CONST(-1 & 127);
memory[13] = `ADDR_PC | `READ_DATA | `MEM_R | `W_OPCODE;            // opcode_cache = mem[pc]
memory[14] = `SP_PLUS_4 | `DECODE | `EXIT_INTERRUPT;                // sp=sp+1, decode opcode, exit_interrupt

// 0000_0100 (04) poppc  -------------------------------------
//    pc=mem[sp]
//    sp = sp + 1
memory[16] = `ADDR_SP | `READ_DATA | `MEM_R | `W_PC;                // pc = mem[sp]
memory[17] = `SP_PLUS_4;                                            // sp = sp + 1
memory[18] = `FETCH;                                                // opcode cached ? decode : fetch,decode

// 0000_0101 (05) add   -------------------------------------
//    mem[sp+1] = mem[sp+1] + mem[sp]
//    sp = sp + 1
memory[20] = `ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4;             // a = mem[sp] || sp=sp+1
memory[21] = `ADDR_SP | `READ_DATA | `MEM_R | `PLUS | `ALU_A | `W_A; // a = a + mem[sp]
memory[22] = `ADDR_SP | `MEM_W | `GO_NEXT;                          // mem[sp] = a

// 0000_0110 (06) and   -------------------------------------
//    mem[sp+1] = mem[sp+1] & mem[sp]
//    sp = sp + 1
memory[24] = `ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4;             // a = mem[sp] || sp=sp+1
memory[25] = `ADDR_SP | `READ_DATA | `MEM_R | `AND |`ALU_A | `W_A;  // a = a & mem[sp]
memory[26] = `ADDR_SP | `MEM_W | `GO_NEXT;                          // mem[sp] = a

// 0000_0111 (07) or   -------------------------------------
//    mem[sp+1] = mem[sp+1] | mem[sp]
//    sp = sp + 1
memory[28] = `ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4;             // a = mem[sp] || sp=sp+1
memory[29] = `ADDR_SP | `READ_DATA | `MEM_R | `OR | `ALU_A | `W_A;  // a = a | mem[sp]
memory[30] = `ADDR_SP | `MEM_W | `GO_NEXT;                          // mem[sp] = a

// 0000_1000 (08) load   -------------------------------------
//    mem[sp] = mem[ mem[sp] ]
memory[32] = `ADDR_SP | `READ_DATA | `MEM_R | `W_A;                 // a = mem[sp]
memory[33] = `ADDR_A | `READ_DATA | `MEM_R | `W_A;                  // a = mem[a]
memory[34] = `ADDR_SP | `MEM_W | `GO_NEXT;                          // mem[sp] = a

// 0000_1001 (09) not   -------------------------------------
//    mem[sp] = ~mem[sp]
memory[36] = `ADDR_SP | `READ_DATA | `MEM_R | `NOT | `W_A;          // a = ~mem[sp]
memory[37] = `ADDR_SP | `MEM_W | `GO_NEXT;                          // mem[sp] = a

// 0000_1010 (0a) flip   -------------------------------------
//    mem[sp] = flip(mem[sp])
memory[40] = `GO_BREAKPOINT;

// 0000_1011 (0b) nop   -------------------------------------
memory[44] = `PC_PLUS_1;
memory[45] = `FETCH;

// 0000_1100 (0c) store   -------------------------------------
//    mem[mem[sp]] <= mem[sp+1]
//    sp = sp + 2
memory[48] = `ADDR_SP | `READ_DATA | `MEM_R | `W_B;                 // b = mem[sp]
memory[49] = `SP_PLUS_4;                                            // sp = sp + 1
memory[50] = `ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4;             // a = mem[sp] || sp = sp + 1
memory[51] = `ADDR_B | `MEM_W | `GO_NEXT;                           // mem[b] = a

// 0000_1101 (0d) popsp   -------------------------------------
//    sp = mem[sp]
memory[52] = `ADDR_SP | `MEM_R | `W_SP | `GO_NEXT;                  // sp = mem[sp]


// 0000_1110 (0e) ipsum    ------------------------------------
memory[56] = `GO_BREAKPOINT;

// 0000_1111 (0f) sncpy   ---------------------------------------
memory[60] = `GO_BREAKPOINT;

// ------------- microcode opcode continuations ---------------
// wset_continue1: ------------------------
memory[64] = `ADDR_A | `READ_ADDR | `PLUS | `W_A | `ALU_CONST(12);  // a = a+12    save clear stack on mem[4]
memory[65] = `ADDR_B | `MEM_W;                                      // mem[b] = a
memory[66] = `ADDR_SP | `MEM_R | `READ_DATA | `W_PC;                // pc = mem[sp] (data)
memory[67] = `SP_PLUS_4;                                            // sp = sp+4
memory[68] = `ADDR_SP | `MEM_R | `READ_DATA | `W_B;                 // b = mem[sp] (count)
memory[69] = `SP_PLUS_4;                                            // sp = sp+4
memory[70] = `ADDR_SP | `MEM_R | `READ_DATA | `W_SP;                // sp = mem[sp] (destination @)
memory[71] = `ADDR_B | `READ_ADDR | `W_A;                           // a = b (count)
// wset_loop:
memory[72] = `BRANCHIF_A_ZERO(80);                                  // if (a==0) goto @wset_end
memory[73] = `ADDR_B | `READ_ADDR | `PLUS | `W_B |                  // b = b-1 (count)
             `ALU_CONST(-1 & 127);
memory[74] = `ADDR_PC | `READ_ADDR | `W_A;                          // a = pc (data)
memory[75] = `ADDR_SP | `MEM_W | `SP_PLUS_4;                        // mem[sp] = a || sp = sp+4 (sp=destination@)
memory[76] = `ADDR_B | `READ_ADDR | `W_A | `BRANCH(72);             // a = b (count) || goto @wset_loop
// wset_end: wcpy_end: sncpy_end:
memory[80] = `ADDR_A | `MEM_R | `READ_DATA | `W_PC;                 // pc = mem[a] (a is 0)
memory[81] = `NOP_B | `ALU_CONST(4) | `W_B;                         // b = 4
memory[82] = `ADDR_B | `MEM_R | `READ_DATA | `W_SP | `FETCH;        // sp=mem[b] || goto @fetch

// wcpy_continue1: ------------------------
memory[84] = `ADDR_A | `READ_ADDR | `PLUS | `ALU_CONST(12) | `W_A;  // a = a+12    save clear stack on mem[4]
memory[85] = `ADDR_B | `MEM_W;                                      // mem[b] = a
memory[86] = `ADDR_SP | `MEM_R | `READ_DATA | `W_B;                 // b = mem[sp] (count)
memory[87] = `SP_PLUS_4;                                            // sp = sp+4
memory[88] = `ADDR_SP | `MEM_R | `READ_DATA | `W_PC;                // pc = mem[sp] (destination @)
memory[89] = `SP_PLUS_4;                                            // sp = sp+4
memory[90] = `ADDR_SP | `MEM_R | `READ_DATA | `W_SP;                // sp = mem[sp] (source @)
memory[91] = `ADDR_B | `READ_ADDR | `W_A;                           // a = b (count)
// wcpy_loop:
memory[92] = `BRANCHIF_A_ZERO(80);                                  // if (a==0) goto @wcpy_end
memory[93] = `ADDR_B | `READ_ADDR | `PLUS | `W_B |                  // b = b-1 (count)
             `ALU_CONST(-1 & 127);
memory[94] = `ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4;             // a = mem[sp] || sp = sp+4 (sp=source@)
memory[95] = `ADDR_PC | `MEM_W | `READ_ADDR | `PLUS | `W_PC |       // mem[pc] = a || pc = pc+4 (pc=destination@)
             `ALU_CONST(4);
memory[96] = `ADDR_B | `READ_ADDR | `W_A | `BRANCH(92);             // a = b (count) || goto @wcpy_loop

// -------------------------------------------------------------

// 001_00000 (20) wcpy -----------------------------------------
// before using this opcode you must save mem[0] & mem[4] words, then wcpy, then restore mems
// c=mem[sp],d=mem[sp+1],s=mem[sp+2]; while(c-->0) mem[d++]=mem[s++]; sp=sp+3
memory[128] = `NOP_B | `ALU_CONST(0) | `W_B;                        // b = 0
memory[129] = `ADDR_PC | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A; // a = pc+1
memory[130] = `ADDR_B | `MEM_W | `NOP_B | `W_B | `ALU_CONST(4);     // mem[b] = a || b = 4
memory[131] = `ADDR_SP | `READ_ADDR | `W_A | `BRANCH(84);           // a = sp || goto @wcpy_continue1

// 001_00001 (21) wset ----------------------------------------
// before using this opcode you must save mem[0] & mem[4] words, then wset, then restore mems
// v=mem[sp],c=mem[sp+1],d=mem[sp+2]; while(c-->0) mem[d++]=v; sp=sp+3
memory[132] = `NOP_B | `ALU_CONST(0) | `W_B;                        // b = 0
memory[133] = `ADDR_PC | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A; // a = pc+1
memory[134] = `ADDR_B | `MEM_W | `NOP_B | `W_B | `ALU_CONST(4);     // mem[b] = a || b = 4
memory[135] = `ADDR_SP | `READ_ADDR | `W_A | `BRANCH(64);           // a = sp || goto @wset_continue1

// 001_00010 (22) loadh   -------------------------------------
memory[136] = `GO_BREAKPOINT;

// 001_00011 (23) storeh   -------------------------------------
memory[140] = `GO_BREAKPOINT;

// 001_00100 (24) lessthan   -------------------------------------
// (mem[sp]-mem[sp+1]) < 0 ? mem[sp+1]=1 : mem[sp+1]=0
// sp=sp+1
memory[144] = `GO_BREAKPOINT;

// 001_00101 (25) lessthanorequal   -------------------------------------
// (mem[sp]-mem[sp+1]) <= 0 ? mem[sp+1]=1 : mem[sp+1]=0
// sp=sp+1
memory[148] = `GO_BREAKPOINT;

// 001_00110 (26) ulessthan   -------------------------------------
// signA!=signB -> (unsigA < unsigB) == ~(sigA < sigA)
// signA==signB -> (unsigA < unsigB) ==  (sigA < sigB)
// (mem[sp]-mem[sp+1]) < 0 ? mem[sp+1]=1 : mem[sp+1]=0
// sp=sp+1
memory[152] = `GO_BREAKPOINT;

// 001_00111 (27) ulessthanorequal   -------------------------------------
// (mem[sp]-mem[sp+1]) <= 0 ? mem[sp+1]=1 : mem[sp+1]=0
// sp=sp+1
memory[156] = `GO_BREAKPOINT;

// 001_01000 (28) swap   -------------------------------------
memory[160] = `GO_BREAKPOINT;

// 001_01001 (29) mult   -------------------------------------
memory[164] = `NOP_B | `ALU_CONST(8) | `W_B;                        // b = 8 (#2 in emulate table)
memory[165] = `BRANCH(`UADDR_EMULATE);                              // emulate #2 (mult opcode)

// 001_01010 (2a) lshiftright   -------------------------------------
memory[168] = `GO_BREAKPOINT;

// 001_01011 (2b) ashiftleft   -------------------------------------
// a = mem[sp] & 5'b11111
// sp = sp + 1
// b = mem[sp]
// label: a <= 0 ? goto @fin
// b = b << 1
// a = a - 1 || goto @label
// fin: a = b
// mem[sp] = a
memory[172] = `ADDR_SP | `READ_DATA | `MEM_R | `AND | `W_A |        // a = mem[sp] & 5'b11111
              `ALU_CONST(31);
memory[173] = `SP_PLUS_4;                                           // sp = sp + 1
memory[174] = `ADDR_SP | `READ_DATA | `MEM_R | `W_B;                // b = mem[sp]
memory[175] = `BRANCH(440);                                         // goto @ashiftleft_loop

// 001_01100 (2c) ashiftright   -------------------------------------
memory[176] = `GO_BREAKPOINT;

// 001_01101 (2d) call   -------------------------------------
//    a = mem[sp]
//    mem[sp]=pc+1
//    pc = a
memory[180] = `ADDR_SP | `READ_DATA | `MEM_R | `W_B;                // b = mem[sp]
memory[181] = `ADDR_PC | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A; // a = pc + 1
memory[182] = `ADDR_SP | `MEM_W | `NOP_B | `ALU_B | `W_PC;          // mem[sp] = a || pc = b
memory[183] = `FETCH;                                               // op_cached ? decode : goto next

// 001_01110 (2e) eq   -------------------------------------
//    a = mem[sp]
//    sp = sp + 1
//    (mem[sp] - a == 0) ? mem[sp] = 1 : mem[sp] = 0
memory[184] = `ADDR_SP | `MEM_R | `READ_DATA | `NOT | `W_A;         // a = NOT(mem[sp])
memory[185] = `ADDR_A | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A;  // a = a + 1
memory[186] = `SP_PLUS_4;                                           // sp = sp + 1
memory[187] = `ADDR_SP | `READ_DATA | `MEM_R | `PLUS | `ALU_A |     // a = mem[sp] + a || goto @eq_check
              `W_A | `BRANCH(416);

// 001_01111 (2f) neq   -------------------------------------
//    a = mem[sp]
//    sp = sp + 1
//    (mem[sp] - a != 0) ? mem[sp] = 1 : mem[sp] = 0
memory[188] = `ADDR_SP | `READ_DATA | `MEM_R | `NOT | `W_A;         // a = NOT(mem[sp])
memory[189] = `ADDR_A | `READ_ADDR |`PLUS | `ALU_CONST(1) | `W_A;   // a = a + 1
memory[190] = `SP_PLUS_4;                                           // sp = sp + 1
memory[191] = `ADDR_SP | `READ_DATA | `MEM_R | `PLUS | `ALU_A |     // a = mem[sp] + a || goto @neq_check
              `W_A | `BRANCH(412);

// 001_10000 (30) neg   -------------------------------------
//    a = NOT(mem[sp])
//    a = a + 1
//    mem[sp] = a
memory[192] = `ADDR_SP | `READ_DATA | `MEM_R | `NOT | `W_A;         // a = NOT(mem[sp])
memory[193] = `ADDR_A | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A;  // a = a + 1
memory[194] = `ADDR_SP | `MEM_W | `GO_NEXT;                         // mem[sp] = a

// 001_10001 (31) sub   -------------------------------------
//    mem[sp+1] = mem[sp+1] - mem[sp]
//  sp = sp + 1
memory[196] = `ADDR_SP | `READ_DATA | `MEM_R | `NOT | `W_A;         // a = NOT(mem[sp])
memory[197] = `ADDR_A | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A;  // a = a + 1
memory[198] = `SP_PLUS_4;                                           // sp = sp + 1
memory[199] = `ADDR_SP | `READ_DATA | `MEM_R | `PLUS | `ALU_A |     // a = mem[sp] + a || goto @sub_cont (set_mem[sp]=a)
              `W_A | `BRANCH(400);

// 001_10010 (32) xor   -------------------------------------
// ALU doesn't perform XOR operation
// mem[sp+1] = mem[sp] ^ mem[sp+1]  -> A^B=(A&~B)|(~A&B)
// a = ~mem[sp] --> a = ~A
// sp = sp + 1
// a = mem[sp] & a --> a = ~A&B
// b = ~a  --> b = A&~B
// a = a | b --> a = ~A&B | A&~B
// mem[sp] = a
memory[200] = `ADDR_SP | `READ_DATA | `MEM_R | `NOT | `W_A;         // a = ~mem[sp] --> a=~A
memory[201] = `SP_PLUS_4;                                           // sp = sp + 1
memory[202] = `ADDR_SP | `READ_DATA | `MEM_R | `AND | `ALU_A | `W_A; // a = mem[sp] & a --> a = ~A&B
memory[203] = `ADDR_A | `READ_ADDR | `NOT | `W_B | `BRANCH(428);    // b = ~a || goto @xor_cont --> b = A&~B

// 001_10011 (33) loadb   -------------------------------------
// b=pc
// pc = mem[sp]
// opcode_cache=mem[pc]
// a = opcode
// mem[sp]=a
// pc=b
// fetch
memory[204] = `ADDR_PC | `READ_ADDR | `W_B;                         // b = pc
memory[205] = `ADDR_SP | `READ_DATA | `MEM_R | `W_PC;               // pc = mem[sp]
memory[206] = `ADDR_PC | `READ_DATA | `MEM_R | `W_OPCODE;           // opcode_cache = mem[pc]
memory[207] = `ALU_OPCODE | `NOP_B | `W_A | `BRANCH(396);           // a = opcode -> byte(pc, mem[pc]) || goto @loadb_continued

// 001_10100 (34) storeb   -------------------------------------
memory[208] = `GO_BREAKPOINT;

// 001_10101 (35) div    -------------------------------------
memory[212] = `GO_BREAKPOINT;

// 001_10110 (36) mod   -------------------------------------
memory[216] = `GO_BREAKPOINT;

// 001_10111 (37) eqbranch   -------------------------------------
//    a = sp + 1
//    a = mem[a]
//    a = mem[sp] || a == 0 ? { pc = pc + a; sp = sp + 2 }
//    else { sp = sp + 2, pc = pc + 1 }
memory[220] = `ADDR_SP | `READ_ADDR | `PLUS | `ALU_CONST(4) | `W_A; // a = sp + 1
memory[221] = `ADDR_A | `MEM_R | `W_A;                              // a = mem[a]
memory[222] = `ADDR_SP | `MEM_R | `W_A | `BRANCHIF_A_ZERO(456);     // a = mem[sp] || a == 0 ? goto 456 (sp=sp+2, pc=pc+a)
memory[223] = `BRANCH(460);                                         // else goto 460 (sp=sp+2, pc=pc+1)

// 001_11000 (38) neqbranch   -------------------------------------
//    a = sp + 1
//    a = mem[a]
//    a = mem[sp] || a == 0 ? { sp = sp + 2, pc = pc + 1 }
//    else { sp = sp + 2, pc = pc + a }
memory[224] = `ADDR_SP | `READ_ADDR | `PLUS | `ALU_CONST(4) | `W_A; // a = sp + 1
memory[225] = `ADDR_A | `MEM_R | `W_A;                              // a = mem[a]
memory[226] = `ADDR_SP | `MEM_R | `W_A | `BRANCHIF_A_ZERO(460);     // a = mem[sp] || a == 0 ? goto 460 (sp=sp+2, pc=pc+1)
memory[227] = `BRANCH(456);                                         // else goto 456 (sp=sp+2, pc=pc+a)

// 001_11001 (39) poppcrel   -------------------------------------
//    a = mem[sp]
//    sp = sp + 1
//    pc = pc + a
memory[228] = `ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4;            // a=mem[sp] || sp=sp+1
memory[229] = `ADDR_PC | `READ_ADDR | `ALU_A | `PLUS | `W_PC;       // pc = pc + a
memory[230] = `FETCH;                                               // op_cached? decode : goto next

// 001_11010 (3a) config   -------------------------------------
memory[232] = `GO_BREAKPOINT;

// 001_11011 (3b) pushpc   -------------------------------------
//    sp = sp - 1
//    mem[sp] = pc
memory[236] = `SP_MINUS_4 | `W_A;                                   // a = sp = sp - 1
memory[237] = `ADDR_SP | `MEM_W | `GO_NEXT;                         // mem[sp] = a

// 001_11100 (3c) syscall_emulate  ------------------------------
memory[240] = `GO_BREAKPOINT;

// 001_11101 (3d) pushspadd   -------------------------------------
//    a = mem[sp] << 2
//    mem[sp] = a + sp
memory[244] = `ADDR_SP | `MEM_R | `W_A_MEM;                         // a = mem[sp]
memory[245] = `ADDR_A | `READ_ADDR | `ALU_A | `PLUS | `W_A;         // a = a + a
memory[246] = `ADDR_A | `READ_ADDR | `ALU_A | `PLUS | `W_A;         // a = a + a
memory[247] = `ADDR_SP | `READ_ADDR | `ALU_A | `PLUS | `W_A |       // a = a + sp || goto @cont (->mem[sp] = a)
              `BRANCH(400);

// 001_11110 (3e) halfmult   -------------------------------------
memory[248] = `GO_BREAKPOINT;

// 001_11111 (3f) callpcrel   -------------------------------------
//    a = mem[sp]
//    mem[sp]=pc+1
//    pc = pc + a
memory[252] = `ADDR_SP | `READ_DATA | `MEM_R | `W_B;                // b = mem[sp]
memory[253] = `ADDR_PC | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A; // a = pc + 1
memory[254] = `ADDR_SP | `MEM_W;                                    // mem[sp] = a;
memory[255] = `ADDR_PC | `READ_ADDR | `ALU_B | `PLUS | `W_PC |      // pc = pc + b, goto @fetch
              `GO_FETCH;

// --------------------- MICROCODE HOLE -----------------------------------

// --------------------- CONTINUATION OF COMPLEX OPCODES ------------------

// loadb continued microcode -----
// mem[sp]=a || pc=b
// opcode_cache=mem[pc] || go next
memory[396] = `ADDR_SP | `MEM_W | `ALU_B | `NOP_B | `W_PC;          // mem[sp] = a || pc=b
memory[397] = `ADDR_PC | `MEM_R | `W_OPCODE | `GO_NEXT;             // opcode_cache = mem[pc] || go next

// sub/pushspadd continued microcode ----------------
memory[400] = `ADDR_SP | `MEM_W | `GO_NEXT;                         // mem[sp] = a

// ----- hole ------

// neqcheck ----------
memory[412] = `BRANCHIF_A_ZERO(468);                                // a == 0 ? goto @set_mem[sp]=0
memory[413] = `BRANCH(464);                                         // else goto @set_mem[sp]=1

// eqcheck ----------
memory[416] = `BRANCHIF_A_ZERO(464);                                // a == 0 ? goto @set_mem[sp]=1
memory[417] = `BRANCH(468);                                         // else goto @set_mem[sp]=0

// lessthanorequal_check ----
memory[420] = `BRANCHIF_A_ZERO(464) | `BRANCHIF_A_NEG(464);         // a <= 0 ? goto @set_mem[sp]=1
memory[421] = `BRANCH(468);                                         // else goto @set_mem[sp]=0

// lessthan_check ----
memory[424] = `BRANCHIF_A_NEG(464);                                 // a < 0 ? goto @set_mem[sp]=1
memory[425] = `BRANCH(468);                                         // else goto @set_mem[sp]=0

// xor_cont continued microcode -----------------------------------
memory[428] = `ADDR_A | `READ_ADDR | `OR | `ALU_B | `W_A;           // a = a | b --> a = ~A&B | A&~B
memory[429] = `ADDR_SP | `MEM_W | `GO_NEXT;                         // mem[sp] = a

// ashiftright_loop continued microcode -----------------------------------

// ashiftleft_loop continued microcode -----------------------------------
memory[440] = `BRANCHIF_A_ZERO(444) | `BRANCHIF_A_NEG(444);         // (a <= 0) ? goto @ashiftleft_exit
memory[441] = `ADDR_A | `READ_ADDR | `PLUS | `W_A |                 // a = a + (-1)
              `ALU_CONST(-1 & 127);
memory[442] = `ADDR_B | `READ_ADDR | `PLUS | `ALU_B | `W_B |        // b = b << 1 || goto @ashiftleft_loop
              `BRANCH(440);
// ashiftleft_exit
memory[444] = `ADDR_B | `READ_ADDR | `NOP | `W_A;                   // a = b
memory[445] = `ADDR_SP | `MEM_W | `GO_NEXT;                         // mem[sp] = a

// neqbranch / eqbranch --- continued microcode   -------------------------------------
//    sp = sp + 2
//    pc = pc + a
memory[456] = `ADDR_SP | `READ_ADDR | `PLUS | `ALU_CONST(8) | `W_SP; // sp = sp + 2
memory[457] = `ADDR_PC | `READ_ADDR | `ALU_A | `PLUS | `W_PC;       // pc = pc + a
memory[458] = `FETCH;                                               // op_cached ? decode : goto fetch

// neqbranch / eqbranch  --- continued microcode   -------------------------------------
//    sp = sp + 2
//  pc = pc + 1
memory[460] = `ADDR_SP | `READ_ADDR | `PLUS | `ALU_CONST(8) | `W_SP; // sp = sp + 2
memory[461] = `PC_PLUS_1;                                           // pc = pc + 1
memory[462] = `FETCH;                                               // op_cached? decode : goto fetch

// neq / eq / lessthan_1 --- continued microcode   --------------------
//     mem[sp] = 1
memory[464] = `ALU_CONST(1) | `NOP_B | `W_A;                        // a = 1
memory[465] = `ADDR_SP | `MEM_W | `GO_NEXT;                         // mem[sp] = a

// neq / eq / lessthan_0 --- continued microcode   --------------------
//    mem[sp] = 0
memory[468] = `ALU_CONST(0) | `NOP_B | `W_A;                        // a = 0
memory[469] = `ADDR_SP | `MEM_W | `GO_NEXT;                         // mem[sp] = a

// MICROCODE ENTRY POINT AFTER RESET   -------------------------------
// initialize cpu registers
//    sp = @SP_START
//    pc = @RESET_VECTOR
memory[473] = 0;                                                    // reserved and empty for correct cpu startup
memory[474] = `ALU_CONST(`SP_START) | `NOP_B | `W_SP;               // sp = @SP_START
memory[475] = `ALU_CONST(`RESET_VECTOR) | `NOP_B | `W_PC |          // pc = @RESET
              `EXIT_INTERRUPT;                                      // enable interrupts on reset
// fall throught fetch/decode

// FETCH / DECODE   -------------------------------------
//    opcode=mem[pc]
//    decode (goto microcode entry point for opcode)
memory[476] = `ADDR_PC | `READ_DATA | `MEM_R | `W_OPCODE;           // opcode_cache = mem[pc]
memory[477] = `DECODE;                                              // decode jump to microcode

// NEXT OPCODE   -------------------------------------
//    pc = pc + 1
//  opcode cached ? decode : goto fetch
memory[480] = `PC_PLUS_1;                                           // pc = pc + 1
memory[481] = `FETCH;                                               // pc_cached ? decode else fetch,decode

// INTERRUPT REQUEST   -------------------------------------
//    sp = sp - 1
//    mem[sp] = pc
//    pc = mem[EMULATED_VECTORS + 0]
memory[484] = `NOP_B | `ALU_CONST(0) | `W_B | `ENTER_INTERRUPT;     // b = 0 (#0 in emulate table) || disable interrupts
memory[485] = `BRANCH(`UADDR_EMULATE);                              // emulate #0 (interrupt)

// ---------------- OPCODES WITH PARAMETER IN OPCODE ----------------

// 1_xxxxxxx im x        -------------------------------------
memory[488] = `GO_BREAKPOINT;

// 1_xxxxxxx im x        -------------------------------------
memory[491] = `GO_BREAKPOINT;

// 010_xxxxx storesp x
//    mem[sp + x<<2] = mem[sp]
//    sp = sp + 1
memory[493] = `ADDR_SP | `READ_ADDR | `PLUS_OFFSET | `ALU_OPCODE | `W_B; // b = sp + offset
memory[494] = `ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4;                // a=mem[sp] || sp=sp+1
memory[495] = `ADDR_B | `MEM_W | `GO_NEXT;                              // mem[b] = a

// 011_xxxxx loadsp x       -------------------------------------
//    mem[sp-1] = mem [sp + x<<2]
//    sp = sp - 1
memory[496] = `ADDR_SP | `READ_ADDR | `PLUS_OFFSET | `ALU_OPCODE | `W_A; // a = sp + offset
memory[497] = `ADDR_A | `READ_DATA | `MEM_R | `W_A;                     // a = mem[a]
memory[498] = `SP_MINUS_4;                                              // sp = sp - 1
memory[499] = `ADDR_SP | `MEM_W | `GO_NEXT;                             // mem[sp] = a

// 0001_xxxx addsp x       -------------------------------------
//     mem[sp] = mem[sp] + mem[sp + x<<2]
memory[500] = `ADDR_SP | `READ_ADDR | `PLUS_OFFSET | `ALU_OPCODE | `W_A; // a = sp + offset
memory[501] = `ADDR_A | `READ_DATA | `MEM_R | `W_A;                     // a = mem[a]
memory[502] = `ADDR_SP | `READ_DATA | `MEM_R | `PLUS | `ALU_A | `W_A;   // a = a + mem[sp]
memory[503] = `ADDR_SP | `MEM_W | `GO_NEXT;                             // mem[sp] = a

// 001_xxxxx emulate x       -------------------------------------
//  <expects b = offset into table for emulated opcode>
//    sp = sp - 1
//    mem[sp] = pc + 1          emulated opcode microcode must set b to
//  a = @EMULATION_TABLE        offset inside emulated_table prior to
//  pc = mem[a + b]             calling the emulate microcode
//  fetch
memory[504] = `ADDR_PC | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A; // a = pc + 1
memory[505] = `SP_MINUS_4;                                          // sp = sp - 1
memory[506] = `ADDR_SP | `MEM_W;                                    // mem[sp] = a
memory[507] = `NOP_B | `ALU_CONST(`EMULATION_VECTOR) | `W_A;        // a = @vector_emulated
memory[508] = `ADDR_A | `READ_ADDR | `PLUS | `ALU_B | `W_A;         // a = a + b
memory[509] = `ADDR_A | `MEM_R | `READ_DATA | `NOP | `W_PC;         // pc = mem[a]
memory[510] = `FETCH;

// --------------------- END OF MICROCODE PROGRAM --------------------------
end

endmodule
