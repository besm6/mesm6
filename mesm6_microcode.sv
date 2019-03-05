`timescale 1ns / 1ps
`include "mesm6_defines.sv"

`define READ_DATA               (`SEL_READ_DATA << `P_SEL_READ)     // 1 bit
`define READ_ADDR               (`SEL_READ_ADDR << `P_SEL_READ)

`define ALU_A                   (`SEL_ALU_A << `P_SEL_ALU)          // 2 bit
`define ALU_OPCODE              (`SEL_ALU_OPCODE << `P_SEL_ALU)
`define ALU_CONST(addr)         (`SEL_ALU_CONST << `P_SEL_ALU | (addr) << `P_IMM)
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

`define W_M                     (1 << `P_W_M)
`define W_PC                    (1 << `P_W_PC)
`define W_A                     (1 << `P_W_A)
`define W_A_MEM                 (1 << `P_W_A_MEM)
`define W_B                     0 // unused
`define W_OPCODE                (1 << `P_W_OPCODE)
`define EXIT_INTERRUPT          (1 << `P_EXIT_INT)
`define ENTER_INTERRUPT         (1 << `P_ENTER_INT)

`define MEM_FETCH               (1 << `P_FETCH)
`define MEM_R                   (1 << `P_MEM_R)
`define MEM_W                   (1 << `P_MEM_W)

`define DECODE                      (1 << `P_DECODE)
`define BRANCH(addr)                (1 << `P_BRANCH | (addr) << `P_IMM)
`define BRANCHIF_OP_NOT_CACHED(a)   (1 << `P_OP_NOT_CACHED | (a) << `P_IMM)
`define BRANCHIF_A_ZERO(addr)       (1 << `P_A_ZERO | (addr) << `P_IMM)
`define BRANCHIF_A_NEG(addr)        (1 << `P_A_NEG | (addr) << `P_IMM)

// microcode common operations

`define PC_PLUS_1               (`ADDR_PC | `READ_ADDR | `ALU_CONST(1) | `PLUS  | `W_PC)
`define SP_MINUS_4              (`ADDR_SP | `READ_ADDR | `ALU_CONST(-4 & 127) | `PLUS | `W_M)
`define SP_PLUS_4               (`ADDR_SP | `READ_ADDR | `ALU_CONST(4) | `PLUS  | `W_M)

`define FETCH                   (`BRANCHIF_OP_NOT_CACHED(`UADDR_FETCH) | `DECODE) // fetch and decode current PC opcode
`define GO_NEXT                 `BRANCH(`UADDR_FETCH_NEXT)  // go to next opcode (PC=PC+1, fetch, decode)
`define GO_FETCH                `BRANCH(`UADDR_FETCH)       // go to fetch opcode at PC, then decode
`define GO_EMULATE              `BRANCH(`UADDR_EMULATE)     // go to emulation routine

module gendata();

reg [`UOP_BITS-1:0] memory[(1<<`UPC_BITS)-1:0];

reg [`UPC_BITS-1:0] stab[64];
reg [`UPC_BITS-1:0] ltab[16];

int c, n, fd;

//
// Add microinstruction to the table.
//
task op(integer uop);
    memory[c] = uop;
    c = c + 1;
endtask

//
// Add given opcode to the jump table.
//
task opcode(integer op);
    if (op[7])
        ltab[op[6:3]] = c;
    else
        stab[op] = c;
endtask

initial begin
c = 0;

//--------------------------------------------------------------
// Microcode entry point after reset.
//
// initialize cpu registers
//    sp = 0
//    pc = @RESET_VECTOR
//
op(0);                                                  // reserved and empty for correct cpu startup
$display("`define UADDR_RESET %0d", c);
op(`ALU_CONST(0) | `NOP_B | `W_M);                      // sp = 0
op(`ALU_CONST(`RESET_VECTOR) | `NOP_B | `W_PC |         // pc = @RESET
              `EXIT_INTERRUPT);                         // enable interrupts on reset
// fall throught fetch/decode

//--------------------------------------------------------------
// Fetch / decode.
//
//    opcode=mem[pc]
//    decode (goto microcode entry point for opcode)
//
$display("`define UADDR_FETCH %0d", c);
op(`ADDR_PC | `READ_DATA | `MEM_FETCH | `W_OPCODE);       // opcode_cache = mem[pc]
op(`DECODE);                                              // decode jump to microcode

//--------------------------------------------------------------
// Next opcode.
//
//    pc = pc + 1
//    opcode cached ? decode : goto fetch
//    decode (goto microcode entry point for opcode)
//
$display("`define UADDR_FETCH_NEXT %0d", c);
op(`PC_PLUS_1);                                           // pc = pc + 1
op(`FETCH);                                               // pc_cached ? decode else fetch,decode

//--------------------------------------------------------------
// Interrupt request.
//
//    sp = sp - 1
//    mem[sp] = pc
//    pc = mem[EMULATED_VECTORS + 0]
//
$display("`define UADDR_INTERRUPT %0d", c);
op(`NOP_B | `ALU_CONST(0) | `W_B | `ENTER_INTERRUPT);     // b = 0 (#0 in emulate table) || disable interrupts
op(`GO_EMULATE);                                          // emulate #0 (interrupt)

//--------------------------------------------------------------
// Emulate opcode.
//
//  <expects b = offset into table for emulated opcode>
//    sp = sp - 1
//    mem[sp] = pc + 1          emulated opcode microcode must set b to
//  a = @EMULATION_TABLE        offset inside emulated_table prior to
//  pc = mem[a + b]             calling the emulate microcode
//  fetch
//
$display("`define UADDR_EMULATE %0d", c);
op(`ADDR_PC | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A); // a = pc + 1
op(`SP_MINUS_4);                                          // sp = sp - 1
op(`ADDR_SP | `MEM_W);                                    // mem[sp] = a
op(`NOP_B | `ALU_CONST(`EMULATION_VECTOR) | `W_A);        // a = @vector_emulated
op(`ADDR_A | `READ_ADDR | `PLUS | `ALU_B | `W_A);         // a = a + b
op(`ADDR_A | `MEM_R | `READ_DATA | `NOP | `W_PC);         // pc = mem[a]
op(`FETCH);

//--------------------------------------------------------------
// Opcodes 000-077.
//
opcode('o000);  // ATX
op(`GO_EMULATE);

opcode('o001);  // STX
op(`GO_EMULATE);

opcode('o002);  // MOD
op(`GO_EMULATE);

opcode('o003);  // XTS
op(`GO_EMULATE);

opcode('o004);  // A+X
op(`GO_EMULATE);

opcode('o005);  // A-X
op(`GO_EMULATE);

opcode('o006);  // X-A
op(`GO_EMULATE);

opcode('o007);  // AMX
op(`GO_EMULATE);

opcode('o010);  // XTA
op(`GO_EMULATE);

opcode('o011);  // AAX
op(`GO_EMULATE);

opcode('o012);  // AEX
op(`GO_EMULATE);

opcode('o013);  // ARX
op(`GO_EMULATE);

opcode('o014);  // AVX
op(`GO_EMULATE);

opcode('o015);  // AOX
op(`GO_EMULATE);

opcode('o016);  // A/X
op(`GO_EMULATE);

opcode('o017);  // A*X
op(`GO_EMULATE);

opcode('o020);  // APX
op(`GO_EMULATE);

opcode('o021);  // AUX
op(`GO_EMULATE);

opcode('o022);  // ACX
op(`GO_EMULATE);

opcode('o023);  // ANX
op(`GO_EMULATE);

opcode('o024);  // E+X
op(`GO_EMULATE);

opcode('o025);  // E-X
op(`GO_EMULATE);

opcode('o026);  // ASX
op(`GO_EMULATE);

opcode('o027);  // XTR
op(`GO_EMULATE);

opcode('o030);  // RTE
op(`GO_EMULATE);

opcode('o031);  // YTA
op(`GO_EMULATE);

opcode('o032);  // 032
op(`GO_EMULATE);

opcode('o033);  // EXT
op(`GO_EMULATE);

opcode('o034);  // E+N
op(`GO_EMULATE);

opcode('o035);  // E-N
op(`GO_EMULATE);

opcode('o036);  // ASN
op(`GO_EMULATE);

opcode('o037);  // NTR
op(`GO_EMULATE);

opcode('o040);  // ATI
op(`GO_EMULATE);

opcode('o041);  // STI
op(`GO_EMULATE);

opcode('o042);  // ITA
op(`GO_EMULATE);

opcode('o043);  // ITS
op(`GO_EMULATE);

opcode('o044);  // MTJ
op(`GO_EMULATE);

opcode('o045);  // J+M
op(`GO_EMULATE);

opcode('o046);  // E46
op(`GO_EMULATE);

opcode('o047);  // E47
op(`GO_EMULATE);

opcode('o050);  // E50
op(`GO_EMULATE);

opcode('o051);  // E51
op(`GO_EMULATE);

opcode('o052);  // E52
op(`GO_EMULATE);

opcode('o053);  // E53
op(`GO_EMULATE);

opcode('o054);  // E54
op(`GO_EMULATE);

opcode('o055);  // E55
op(`GO_EMULATE);

opcode('o056);  // E56
op(`GO_EMULATE);

opcode('o057);  // E57
op(`GO_EMULATE);

opcode('o060);  // E60
op(`GO_EMULATE);

opcode('o061);  // E61
op(`GO_EMULATE);

opcode('o062);  // E62
op(`GO_EMULATE);

opcode('o063);  // E63
op(`GO_EMULATE);

opcode('o064);  // E64
op(`GO_EMULATE);

opcode('o065);  // E65
op(`GO_EMULATE);

opcode('o066);  // E66
op(`GO_EMULATE);

opcode('o067);  // E67
op(`GO_EMULATE);

opcode('o070);  // E70
op(`GO_EMULATE);

opcode('o071);  // E71
op(`GO_EMULATE);

opcode('o072);  // E72
op(`GO_EMULATE);

opcode('o073);  // E73
op(`GO_EMULATE);

opcode('o074);  // E74
op(`GO_EMULATE);

opcode('o075);  // E75
op(`GO_EMULATE);

opcode('o076);  // E76
op(`GO_EMULATE);

opcode('o077);  // E77
op(`GO_EMULATE);

//--------------------------------------------------------------
// Opcodes 20-37.
opcode('o200);  // E20
op(`GO_EMULATE);

opcode('o210);  // E21
op(`GO_EMULATE);

opcode('o220);  // UTC
op(`GO_EMULATE);

opcode('o230);  // WTC
op(`GO_EMULATE);

opcode('o240);  // VTM
op(`GO_EMULATE);

opcode('o250);  // UTM
op(`GO_EMULATE);

opcode('o260);  // UZA
op(`GO_EMULATE);

opcode('o270);  // UIA
op(`GO_EMULATE);

opcode('o300);  // UJ
op(`GO_EMULATE);

opcode('o310);  // VJM
op(`GO_EMULATE);

opcode('o320);  // IJ
op(`GO_EMULATE);

opcode('o330);  // STOP
op(`GO_EMULATE);

opcode('o340);  // VZM
op(`GO_EMULATE);

opcode('o350);  // VIM
op(`GO_EMULATE);

opcode('o360);  // E36
op(`GO_EMULATE);

opcode('o370);  // VLM
op(`GO_EMULATE);

`ifdef notdef
// ----- OPCODES WITHOUT CONSTANT ------

// 0000_0010 (02) pushsp  -------------------------------------
//    mem[sp-1] = sp
//    sp = sp - 1
//op(`ADDR_SP | `READ_ADDR | `NOP | `W_A);                    // a = sp
//op(`SP_MINUS_4);                                            // sp = sp - 1
//op(`ADDR_SP | `MEM_W | `GO_NEXT);                          // mem[sp]=a

// 0000_0011 (03) popint  -------------------------------------
// pc=mem[sp]-1        (emulate stores pc+1 but we must return to
// sp=sp+1             pc because interrupt takes precedence to decode)
// fetch & decode, then clear_interrupt_flag
// this guarantees that a continous interrupt allows to execute at least one
// opcode of mainstream program before reentry to interrupt handler
op(`ADDR_SP | `READ_DATA | `MEM_R | `PLUS | `W_PC |       // pc = mem[sp]-1
             `ALU_CONST(-1 & 127));
op(`ADDR_PC | `READ_DATA | `MEM_FETCH | `W_OPCODE);        // opcode_cache = mem[pc]
op(`SP_PLUS_4 | `DECODE | `EXIT_INTERRUPT);                // sp=sp+1, decode opcode, exit_interrupt

// 0000_0100 (04) poppc  -------------------------------------
//    pc=mem[sp]
//    sp = sp + 1
op(`ADDR_SP | `READ_DATA | `MEM_R | `W_PC);                // pc = mem[sp]
op(`SP_PLUS_4);                                            // sp = sp + 1
op(`FETCH);                                                // opcode cached ? decode : fetch,decode

// 0000_0101 (05) add   -------------------------------------
//    mem[sp+1] = mem[sp+1] + mem[sp]
//    sp = sp + 1
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);             // a = mem[sp] || sp=sp+1
op(`ADDR_SP | `READ_DATA | `MEM_R | `PLUS | `ALU_A | `W_A); // a = a + mem[sp]
op(`ADDR_SP | `MEM_W | `GO_NEXT);                          // mem[sp] = a

// 0000_0110 (06) and   -------------------------------------
//    mem[sp+1] = mem[sp+1] & mem[sp]
//    sp = sp + 1
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);             // a = mem[sp] || sp=sp+1
op(`ADDR_SP | `READ_DATA | `MEM_R | `AND |`ALU_A | `W_A);  // a = a & mem[sp]
op(`ADDR_SP | `MEM_W | `GO_NEXT);                          // mem[sp] = a

// 0000_0111 (07) or   -------------------------------------
//    mem[sp+1] = mem[sp+1] | mem[sp]
//    sp = sp + 1
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);             // a = mem[sp] || sp=sp+1
op(`ADDR_SP | `READ_DATA | `MEM_R | `OR | `ALU_A | `W_A);  // a = a | mem[sp]
op(`ADDR_SP | `MEM_W | `GO_NEXT);                          // mem[sp] = a

// 0000_1000 (08) load   -------------------------------------
//    mem[sp] = mem[ mem[sp] ]
op(`ADDR_SP | `READ_DATA | `MEM_R | `W_A);                 // a = mem[sp]
op(`ADDR_A | `READ_DATA | `MEM_R | `W_A);                  // a = mem[a]
op(`ADDR_SP | `MEM_W | `GO_NEXT);                          // mem[sp] = a

// 0000_1001 (09) not   -------------------------------------
//    mem[sp] = ~mem[sp]
op(`ADDR_SP | `READ_DATA | `MEM_R | `NOT | `W_A);          // a = ~mem[sp]
op(`ADDR_SP | `MEM_W | `GO_NEXT);                          // mem[sp] = a

// 0000_1010 (0a) flip   -------------------------------------
//    mem[sp] = flip(mem[sp])
op(`GO_EMULATE);

// 0000_1011 (0b) nop   -------------------------------------
op(`PC_PLUS_1);
op(`FETCH);

// 0000_1100 (0c) store   -------------------------------------
//    mem[mem[sp]] <= mem[sp+1]
//    sp = sp + 2
op(`ADDR_SP | `READ_DATA | `MEM_R | `W_B);                 // b = mem[sp]
op(`SP_PLUS_4);                                            // sp = sp + 1
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);             // a = mem[sp] || sp = sp + 1
op(`ADDR_B | `MEM_W | `GO_NEXT);                           // mem[b] = a

// 0000_1101 (0d) popsp   -------------------------------------
//    sp = mem[sp]
op(`ADDR_SP | `MEM_R | `W_M | `GO_NEXT);                    // sp = mem[sp]


// 0000_1110 (0e) ipsum    ------------------------------------
op(`GO_EMULATE);

// 0000_1111 (0f) sncpy   ---------------------------------------
op(`GO_EMULATE);

// ------------- microcode opcode continuations ---------------
// wset_continue1: ------------------------
op(`ADDR_A | `READ_ADDR | `PLUS | `W_A | `ALU_CONST(12));  // a = a+12    save clear stack on mem[4]
op(`ADDR_B | `MEM_W);                                      // mem[b] = a
op(`ADDR_SP | `MEM_R | `READ_DATA | `W_PC);                // pc = mem[sp] (data)
op(`SP_PLUS_4);                                            // sp = sp+4
op(`ADDR_SP | `MEM_R | `READ_DATA | `W_B);                 // b = mem[sp] (count)
op(`SP_PLUS_4);                                            // sp = sp+4
op(`ADDR_SP | `MEM_R | `READ_DATA | `W_M);                  // sp = mem[sp] (destination @)
op(`ADDR_B | `READ_ADDR | `W_A);                           // a = b (count)
// wset_loop:
op(`BRANCHIF_A_ZERO(80));                                  // if (a==0) goto @wset_end
op(`ADDR_B | `READ_ADDR | `PLUS | `W_B |                  // b = b-1 (count)
             `ALU_CONST(-1 & 127));
op(`ADDR_PC | `READ_ADDR | `W_A);                          // a = pc (data)
op(`ADDR_SP | `MEM_W | `SP_PLUS_4);                        // mem[sp] = a || sp = sp+4 (sp=destination@)
op(`ADDR_B | `READ_ADDR | `W_A | `BRANCH(72));             // a = b (count) || goto @wset_loop
// wset_end: wcpy_end: sncpy_end:
op(`ADDR_A | `MEM_R | `READ_DATA | `W_PC);                 // pc = mem[a] (a is 0)
op(`NOP_B | `ALU_CONST(4) | `W_B);                         // b = 4
op(`ADDR_B | `MEM_R | `READ_DATA | `W_M | `FETCH);          // sp=mem[b] || goto @fetch

// wcpy_continue1: ------------------------
op(`ADDR_A | `READ_ADDR | `PLUS | `ALU_CONST(12) | `W_A);  // a = a+12    save clear stack on mem[4]
op(`ADDR_B | `MEM_W);                                      // mem[b] = a
op(`ADDR_SP | `MEM_R | `READ_DATA | `W_B);                 // b = mem[sp] (count)
op(`SP_PLUS_4);                                            // sp = sp+4
op(`ADDR_SP | `MEM_R | `READ_DATA | `W_PC);                // pc = mem[sp] (destination @)
op(`SP_PLUS_4);                                            // sp = sp+4
op(`ADDR_SP | `MEM_R | `READ_DATA | `W_M);                  // sp = mem[sp] (source @)
op(`ADDR_B | `READ_ADDR | `W_A);                           // a = b (count)
// wcpy_loop:
op(`BRANCHIF_A_ZERO(80));                                  // if (a==0) goto @wcpy_end
op(`ADDR_B | `READ_ADDR | `PLUS | `W_B |                  // b = b-1 (count)
             `ALU_CONST(-1 & 127));
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);             // a = mem[sp] || sp = sp+4 (sp=source@)
op(`ADDR_PC | `MEM_W | `READ_ADDR | `PLUS | `W_PC |       // mem[pc] = a || pc = pc+4 (pc=destination@)
             `ALU_CONST(4));
op(`ADDR_B | `READ_ADDR | `W_A | `BRANCH(92));             // a = b (count) || goto @wcpy_loop

// -------------------------------------------------------------

// 001_00000 (20) wcpy -----------------------------------------
// before using this opcode you must save mem[0] & mem[4] words, then wcpy, then restore mems
// c=mem[sp],d=mem[sp+1],s=mem[sp+2]); while(c-->0) mem[d++]=mem[s++]); sp=sp+3
op(`NOP_B | `ALU_CONST(0) | `W_B);                        // b = 0
op(`ADDR_PC | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A); // a = pc+1
op(`ADDR_B | `MEM_W | `NOP_B | `W_B | `ALU_CONST(4));     // mem[b] = a || b = 4
op(`ADDR_SP | `READ_ADDR | `W_A | `BRANCH(84));           // a = sp || goto @wcpy_continue1

// 001_00001 (21) wset ----------------------------------------
// before using this opcode you must save mem[0] & mem[4] words, then wset, then restore mems
// v=mem[sp],c=mem[sp+1],d=mem[sp+2]; while(c-->0) mem[d++]=v; sp=sp+3
op(`NOP_B | `ALU_CONST(0) | `W_B);                        // b = 0
op(`ADDR_PC | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A); // a = pc+1
op(`ADDR_B | `MEM_W | `NOP_B | `W_B | `ALU_CONST(4));     // mem[b] = a || b = 4
op(`ADDR_SP | `READ_ADDR | `W_A | `BRANCH(64));           // a = sp || goto @wset_continue1

// 001_00010 (22) loadh   -------------------------------------
op(`GO_EMULATE;

// 001_00011 (23) storeh   -------------------------------------
op(`GO_EMULATE);

// 001_00100 (24) lessthan   -------------------------------------
// (mem[sp]-mem[sp+1]) < 0 ? mem[sp+1]=1 : mem[sp+1]=0
// sp=sp+1
op(`GO_EMULATE);

// 001_00101 (25) lessthanorequal   -------------------------------------
// (mem[sp]-mem[sp+1]) <= 0 ? mem[sp+1]=1 : mem[sp+1]=0
// sp=sp+1
op(`GO_EMULATE);

// 001_00110 (26) ulessthan   -------------------------------------
// signA!=signB -> (unsigA < unsigB) == ~(sigA < sigA)
// signA==signB -> (unsigA < unsigB) ==  (sigA < sigB)
// (mem[sp]-mem[sp+1]) < 0 ? mem[sp+1]=1 : mem[sp+1]=0
// sp=sp+1
op(`GO_EMULATE);

// 001_00111 (27) ulessthanorequal   -------------------------------------
// (mem[sp]-mem[sp+1]) <= 0 ? mem[sp+1]=1 : mem[sp+1]=0
// sp=sp+1
op(`GO_EMULATE);

// 001_01000 (28) swap   -------------------------------------
op(`GO_EMULATE);

// 001_01001 (29) mult   -------------------------------------
op(`NOP_B | `ALU_CONST(8) | `W_B);                        // b = 8 (#2 in emulate table)
op(`GO_EMULATE);                                          // emulate #2 (mult opcode)

// 001_01010 (2a) lshiftright   -------------------------------------
op(`GO_EMULATE);

// 001_01011 (2b) ashiftleft   -------------------------------------
// a = mem[sp] & 5'b11111
// sp = sp + 1
// b = mem[sp]
// label: a <= 0 ? goto @fin
// b = b << 1
// a = a - 1 || goto @label
// fin: a = b
// mem[sp] = a
op(`ADDR_SP | `READ_DATA | `MEM_R | `AND | `W_A |        // a = mem[sp] & 5'b11111
              `ALU_CONST(31));
op(`SP_PLUS_4);                                           // sp = sp + 1
op(`ADDR_SP | `READ_DATA | `MEM_R | `W_B);                // b = mem[sp]
op(`BRANCH(440));                                         // goto @ashiftleft_loop

// 001_01100 (2c) ashiftright   -------------------------------------
op(`GO_EMULATE);

// 001_01101 (2d) call   -------------------------------------
//    a = mem[sp]
//    mem[sp]=pc+1
//    pc = a
op(`ADDR_SP | `READ_DATA | `MEM_R | `W_B);                // b = mem[sp]
op(`ADDR_PC | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A); // a = pc + 1
op(`ADDR_SP | `MEM_W | `NOP_B | `ALU_B | `W_PC);          // mem[sp] = a || pc = b
op(`FETCH);                                               // op_cached ? decode : goto next

// 001_01110 (2e) eq   -------------------------------------
//    a = mem[sp]
//    sp = sp + 1
//    (mem[sp] - a == 0) ? mem[sp] = 1 : mem[sp] = 0
op(`ADDR_SP | `MEM_R | `READ_DATA | `NOT | `W_A);         // a = NOT(mem[sp])
op(`ADDR_A | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A);  // a = a + 1
op(`SP_PLUS_4);                                           // sp = sp + 1
op(`ADDR_SP | `READ_DATA | `MEM_R | `PLUS | `ALU_A |     // a = mem[sp] + a || goto @eq_check
              `W_A | `BRANCH(416));

// 001_01111 (2f) neq   -------------------------------------
//    a = mem[sp]
//    sp = sp + 1
//    (mem[sp] - a != 0) ? mem[sp] = 1 : mem[sp] = 0
op(`ADDR_SP | `READ_DATA | `MEM_R | `NOT | `W_A);         // a = NOT(mem[sp])
op(`ADDR_A | `READ_ADDR |`PLUS | `ALU_CONST(1) | `W_A);   // a = a + 1
op(`SP_PLUS_4);                                           // sp = sp + 1
op(`ADDR_SP | `READ_DATA | `MEM_R | `PLUS | `ALU_A |     // a = mem[sp] + a || goto @neq_check
              `W_A | `BRANCH(412));

// 001_10000 (30) neg   -------------------------------------
//    a = NOT(mem[sp])
//    a = a + 1
//    mem[sp] = a
op(`ADDR_SP | `READ_DATA | `MEM_R | `NOT | `W_A);         // a = NOT(mem[sp])
op(`ADDR_A | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A);  // a = a + 1
op(`ADDR_SP | `MEM_W | `GO_NEXT);                         // mem[sp] = a

// 001_10001 (31) sub   -------------------------------------
//    mem[sp+1] = mem[sp+1] - mem[sp]
//  sp = sp + 1
op(`ADDR_SP | `READ_DATA | `MEM_R | `NOT | `W_A);         // a = NOT(mem[sp])
op(`ADDR_A | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A);  // a = a + 1
op(`SP_PLUS_4);                                           // sp = sp + 1
op(`ADDR_SP | `READ_DATA | `MEM_R | `PLUS | `ALU_A |     // a = mem[sp] + a || goto @sub_cont (set_mem[sp]=a)
              `W_A | `BRANCH(400));

// 001_10010 (32) xor   -------------------------------------
// ALU doesn't perform XOR operation
// mem[sp+1] = mem[sp] ^ mem[sp+1]  -> A^B=(A&~B)|(~A&B)
// a = ~mem[sp] --> a = ~A
// sp = sp + 1
// a = mem[sp] & a --> a = ~A&B
// b = ~a  --> b = A&~B
// a = a | b --> a = ~A&B | A&~B
// mem[sp] = a
op(`ADDR_SP | `READ_DATA | `MEM_R | `NOT | `W_A);         // a = ~mem[sp] --> a=~A
op(`SP_PLUS_4);                                           // sp = sp + 1
op(`ADDR_SP | `READ_DATA | `MEM_R | `AND | `ALU_A | `W_A); // a = mem[sp] & a --> a = ~A&B
op(`ADDR_A | `READ_ADDR | `NOT | `W_B | `BRANCH(428));    // b = ~a || goto @xor_cont --> b = A&~B

// 001_10011 (33) loadb   -------------------------------------
// b=pc
// pc = mem[sp]
// opcode_cache=mem[pc]
// a = opcode
// mem[sp]=a
// pc=b
// fetch
op(`ADDR_PC | `READ_ADDR | `W_B);                         // b = pc
op(`ADDR_SP | `READ_DATA | `MEM_R | `W_PC);               // pc = mem[sp]
op(`ADDR_PC | `READ_DATA | `MEM_FETCH | `W_OPCODE);       // opcode_cache = mem[pc]
op(`ALU_OPCODE | `NOP_B | `W_A | `BRANCH(396));           // a = opcode -> byte(pc, mem[pc]) || goto @loadb_continued

// 001_10100 (34) storeb   -------------------------------------
op(`GO_EMULATE);

// 001_10101 (35) div    -------------------------------------
op(`GO_EMULATE);

// 001_10110 (36) mod   -------------------------------------
op(`GO_EMULATE);

// 001_10111 (37) eqbranch   -------------------------------------
//    a = sp + 1
//    a = mem[a]
//    a = mem[sp] || a == 0 ? { pc = pc + a; sp = sp + 2 }
//    else { sp = sp + 2, pc = pc + 1 }
op(`ADDR_SP | `READ_ADDR | `PLUS | `ALU_CONST(4) | `W_A); // a = sp + 1
op(`ADDR_A | `MEM_R | `W_A);                              // a = mem[a]
op(`ADDR_SP | `MEM_R | `W_A | `BRANCHIF_A_ZERO(456));     // a = mem[sp] || a == 0 ? goto 456 (sp=sp+2, pc=pc+a)
op(`BRANCH(460));                                         // else goto 460 (sp=sp+2, pc=pc+1)

// 001_11000 (38) neqbranch   -------------------------------------
//    a = sp + 1
//    a = mem[a]
//    a = mem[sp] || a == 0 ? { sp = sp + 2, pc = pc + 1 }
//    else { sp = sp + 2, pc = pc + a }
op(`ADDR_SP | `READ_ADDR | `PLUS | `ALU_CONST(4) | `W_A); // a = sp + 1
op(`ADDR_A | `MEM_R | `W_A);                              // a = mem[a]
op(`ADDR_SP | `MEM_R | `W_A | `BRANCHIF_A_ZERO(460));     // a = mem[sp] || a == 0 ? goto 460 (sp=sp+2, pc=pc+1)
op(`BRANCH(456));                                         // else goto 456 (sp=sp+2, pc=pc+a)

// 001_11001 (39) poppcrel   -------------------------------------
//    a = mem[sp]
//    sp = sp + 1
//    pc = pc + a
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);            // a=mem[sp] || sp=sp+1
op(`ADDR_PC | `READ_ADDR | `ALU_A | `PLUS | `W_PC);       // pc = pc + a
op(`FETCH);                                               // op_cached? decode : goto next

// 001_11010 (3a) config   -------------------------------------
op(`GO_EMULATE);

// 001_11011 (3b) pushpc   -------------------------------------
//    sp = sp - 1
//    mem[sp] = pc
op(`SP_MINUS_4 | `W_A);                                   // a = sp = sp - 1
op(`ADDR_SP | `MEM_W | `GO_NEXT);                         // mem[sp] = a

// 001_11100 (3c) syscall_emulate  ------------------------------
op(`GO_EMULATE);

// 001_11101 (3d) pushspadd   -------------------------------------
//    a = mem[sp] << 2
//    mem[sp] = a + sp
op(`ADDR_SP | `MEM_R | `W_A_MEM);                         // a = mem[sp]
op(`ADDR_A | `READ_ADDR | `ALU_A | `PLUS | `W_A);         // a = a + a
op(`ADDR_A | `READ_ADDR | `ALU_A | `PLUS | `W_A);         // a = a + a
op(`ADDR_SP | `READ_ADDR | `ALU_A | `PLUS | `W_A |       // a = a + sp || goto @cont (->mem[sp] = a)
              `BRANCH(400));

// 001_11110 (3e) halfmult   -------------------------------------
op(`GO_EMULATE);

// 001_11111 (3f) callpcrel   -------------------------------------
//    a = mem[sp]
//    mem[sp]=pc+1
//    pc = pc + a
op(`ADDR_SP | `READ_DATA | `MEM_R | `W_B);                // b = mem[sp]
op(`ADDR_PC | `READ_ADDR | `PLUS | `ALU_CONST(1) | `W_A); // a = pc + 1
op(`ADDR_SP | `MEM_W);                                    // mem[sp] = a;
op(`ADDR_PC | `READ_ADDR | `ALU_B | `PLUS | `W_PC |      // pc = pc + b, goto @fetch
              `GO_FETCH);

// --------------------- MICROCODE HOLE -----------------------------------

// --------------------- CONTINUATION OF COMPLEX OPCODES ------------------

// loadb continued microcode -----
// mem[sp]=a || pc=b
// opcode_cache=mem[pc] || go next
op(`ADDR_SP | `MEM_W | `ALU_B | `NOP_B | `W_PC);          // mem[sp] = a || pc=b
op(`ADDR_PC | `MEM_FETCH | `W_OPCODE | `GO_NEXT);         // opcode_cache = mem[pc] || go next

// sub/pushspadd continued microcode ----------------
op(`ADDR_SP | `MEM_W | `GO_NEXT);                         // mem[sp] = a

// ----- hole ------

// neqcheck ----------
op(`BRANCHIF_A_ZERO(468));                                // a == 0 ? goto @set_mem[sp]=0
op(`BRANCH(464));                                         // else goto @set_mem[sp]=1

// eqcheck ----------
op(`BRANCHIF_A_ZERO(464));                                // a == 0 ? goto @set_mem[sp]=1
op(`BRANCH(468));                                         // else goto @set_mem[sp]=0

// lessthanorequal_check ----
op(`BRANCHIF_A_ZERO(464) | `BRANCHIF_A_NEG(464));         // a <= 0 ? goto @set_mem[sp]=1
op(`BRANCH(468));                                         // else goto @set_mem[sp]=0

// lessthan_check ----
op(`BRANCHIF_A_NEG(464));                                 // a < 0 ? goto @set_mem[sp]=1
op(`BRANCH(468));                                         // else goto @set_mem[sp]=0

// xor_cont continued microcode -----------------------------------
op(`ADDR_A | `READ_ADDR | `OR | `ALU_B | `W_A);           // a = a | b --> a = ~A&B | A&~B
op(`ADDR_SP | `MEM_W | `GO_NEXT);                         // mem[sp] = a

// ashiftright_loop continued microcode -----------------------------------

// ashiftleft_loop continued microcode -----------------------------------
op(`BRANCHIF_A_ZERO(444) | `BRANCHIF_A_NEG(444));         // (a <= 0) ? goto @ashiftleft_exit
op(`ADDR_A | `READ_ADDR | `PLUS | `W_A |                 // a = a + (-1)
              `ALU_CONST(-1 & 127));
op(`ADDR_B | `READ_ADDR | `PLUS | `ALU_B | `W_B |        // b = b << 1 || goto @ashiftleft_loop
              `BRANCH(440));
// ashiftleft_exit
op(`ADDR_B | `READ_ADDR | `NOP | `W_A);                   // a = b
op(`ADDR_SP | `MEM_W | `GO_NEXT);                         // mem[sp] = a

// neqbranch / eqbranch --- continued microcode   -------------------------------------
//    sp = sp + 2
//    pc = pc + a
op(`ADDR_SP | `READ_ADDR | `PLUS | `ALU_CONST(8) | `W_M);   // sp = sp + 2
op(`ADDR_PC | `READ_ADDR | `ALU_A | `PLUS | `W_PC);       // pc = pc + a
op(`FETCH);                                               // op_cached ? decode : goto fetch

// neqbranch / eqbranch  --- continued microcode   -------------------------------------
//    sp = sp + 2
//  pc = pc + 1
op(`ADDR_SP | `READ_ADDR | `PLUS | `ALU_CONST(8) | `W_M);   // sp = sp + 2
op(`PC_PLUS_1);                                           // pc = pc + 1
op(`FETCH);                                               // op_cached? decode : goto fetch

// neq / eq / lessthan_1 --- continued microcode   --------------------
//     mem[sp] = 1
op(`ALU_CONST(1) | `NOP_B | `W_A);                        // a = 1
op(`ADDR_SP | `MEM_W | `GO_NEXT);                         // mem[sp] = a

// neq / eq / lessthan_0 --- continued microcode   --------------------
//    mem[sp] = 0
op(`ALU_CONST(0) | `NOP_B | `W_A);                        // a = 0
op(`ADDR_SP | `MEM_W | `GO_NEXT);                         // mem[sp] = a

// ---------------- OPCODES WITH PARAMETER IN OPCODE ----------------

// 1_xxxxxxx im x        -------------------------------------
op(`GO_EMULATE);

// 1_xxxxxxx im x        -------------------------------------
op(`GO_EMULATE);

// 010_xxxxx storesp x
//    mem[sp + x<<2] = mem[sp]
//    sp = sp + 1
op(`ADDR_SP | `READ_ADDR | `PLUS_OFFSET | `ALU_OPCODE | `W_B); // b = sp + offset
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);                // a=mem[sp] || sp=sp+1
op(`ADDR_B | `MEM_W | `GO_NEXT);                              // mem[b] = a

// 011_xxxxx loadsp x       -------------------------------------
//    mem[sp-1] = mem [sp + x<<2]
//    sp = sp - 1
op(`ADDR_SP | `READ_ADDR | `PLUS_OFFSET | `ALU_OPCODE | `W_A); // a = sp + offset
op(`ADDR_A | `READ_DATA | `MEM_R | `W_A);                     // a = mem[a]
op(`SP_MINUS_4);                                              // sp = sp - 1
op(`ADDR_SP | `MEM_W | `GO_NEXT);                             // mem[sp] = a

// 0001_xxxx addsp x       -------------------------------------
//     mem[sp] = mem[sp] + mem[sp + x<<2]
op(`ADDR_SP | `READ_ADDR | `PLUS_OFFSET | `ALU_OPCODE | `W_A); // a = sp + offset
op(`ADDR_A | `READ_DATA | `MEM_R | `W_A);                     // a = mem[a]
op(`ADDR_SP | `READ_DATA | `MEM_R | `PLUS | `ALU_A | `W_A);   // a = a + mem[sp]
op(`ADDR_SP | `MEM_W | `GO_NEXT);                             // mem[sp] = a
`endif

// --------------------- END OF MICROCODE PROGRAM --------------------------

// Generate output files.
fd = $fopen("microcode.v", "w");
for(n = 0; n < c; n = n + 1) begin
    $fdisplay(fd, "%3d: %0d'o%o,", n, `UOP_BITS, memory[n]);
end
$fclose(fd);

fd = $fopen("jumptab16.v", "w");
for(n = 0; n < 16; n = n + 1) begin
    $fdisplay(fd, "'o0%2o: %0d'd%0d,", n, `UPC_BITS, ltab[n]);
end
$fclose(fd);

fd = $fopen("jumptab64.v", "w");
for(n = 0; n < 64; n = n + 1) begin
    $fdisplay(fd, "'o0%2o: %0d'd%0d,", n, `UPC_BITS, stab[n]);
end
$fclose(fd);

end // initial
endmodule
