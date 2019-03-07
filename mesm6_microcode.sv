`timescale 1ns / 1ps
`include "mesm6_defines.sv"

`define ACC_ALU                 (`SEL_ACC_ALU << `P_SEL_ACC)
`define ACC_MEM                 (`SEL_ACC_MEM << `P_SEL_ACC)
`define ACC_REG                 (`SEL_ACC_REG << `P_SEL_ACC)
`define ACC_RR                  (`SEL_ACC_RR << `P_SEL_ACC)
`define ACC_Y                   (`SEL_ACC_Y << `P_SEL_ACC)

`define MR_I                    (`SEL_MR_I << `P_SEL_MR)
`define MR_IMM(val)             (`SEL_MR_IMM << `P_SEL_MR | (val) << `P_IMM)
`define MR_VADDR                (`SEL_MR_VADDR << `P_SEL_MR)
`define MR_UADDR                (`SEL_MR_UADDR << `P_SEL_MR)

`define MW_IMM(val)             (`SEL_MW_IMM << `P_SEL_MW | (val) << `P_IMM)

`define MD_PC                   (`SEL_MD_PC << `P_SEL_MD)
`define MD_A                    (`SEL_MD_A << `P_SEL_MD)
`define MD_ALU                  (`SEL_MD_ALU << `P_SEL_MD)
`define MD_REG                  (`SEL_MD_REG << `P_SEL_MD)
`define MD_REG_PLUS1            (`SEL_MD_REG_PLUS1 << `P_SEL_MD)
`define MD_REG_MINUS1           (`SEL_MD_REG_PLUS1 << `P_SEL_MD)
`define MD_VA                   (`SEL_MD_VA << `P_SEL_MD)
`define MD_UA                   (`SEL_MD_UA << `P_SEL_MD)

`define ADDR_M(i)               (1 << `P_SEL_ADDR | `MR_IMM(i))
`define ADDR_SP                 `ADDR_M(15)

`define PC_REG                  (`SEL_PC_REG << `P_SEL_PC)
`define PC_IMM(val)             (`SEL_PC_IMM << `P_SEL_PC | (val) << `P_IMM)
`define PC_VA                   (`SEL_PC_VA << `P_SEL_PC)
`define PC_UA                   (`SEL_PC_UA << `P_SEL_PC)
`define PC_PLUS1                (`SEL_PC_PLUS1 << `P_SEL_PC)

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
`define W_OPCODE                (1 << `P_W_OPCODE)
`define EXIT_INTERRUPT          (1 << `P_EXIT_INT)
`define ENTER_INTERRUPT         (1 << `P_ENTER_INT)
`define CLEAR_C                 (1 << `P_CLEAR_C)

`define MEM_FETCH               (1 << `P_FETCH)
`define MEM_R                   (1 << `P_MEM_R)
`define MEM_W                   (1 << `P_MEM_W)

`define DECODE                      (1 << `P_DECODE)
`define BRANCH(addr)                (1 << `P_BRANCH | (addr) << `P_IMM)
`define BRANCHIF_OP_NOT_CACHED(a)   (1 << `P_OP_NOT_CACHED | (a) << `P_IMM)
`define BRANCHIF_A_ZERO(addr)       (1 << `P_A_ZERO | (addr) << `P_IMM)
`define BRANCHIF_A_NEG(addr)        (1 << `P_A_NEG | (addr) << `P_IMM)

// microcode common operations
`define SP_PLUS_1               (`MR_IMM(15) | `MW_IMM(15) | `MD_REG_PLUS1 | `W_M)
`define SP_MINUS_1              (`MR_IMM(15) | `MW_IMM(15) | `MD_REG_MINUS1 | `W_M)

`define GO_FETCH_OR_DECODE      (`BRANCHIF_OP_NOT_CACHED(uaddr_fetch) | `DECODE) // fetch and decode current PC opcode
`define GO_NEXT                 `BRANCH(uaddr_fetch_next)   // go to next opcode (PC=PC+1, fetch, decode)
`define GO_FETCH                `BRANCH(uaddr_fetch)        // go to fetch opcode at PC, then decode

module gendata();

reg [`UOP_BITS-1:0] memory[(1<<`UPC_BITS)-1:0];

reg [`UPC_BITS-1:0] stab[64];
reg [`UPC_BITS-1:0] ltab[16];

reg [`UPC_BITS-1:0] uaddr_fetch;
reg [`UPC_BITS-1:0] uaddr_fetch_next;

int c, n, fd, ret;

//
// Add microinstruction to the table.
//
task op(reg [`UOP_BITS-1:0] uop);
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

task print_message();
    if (ret == 0)
        $display("*** Please update the following declarations in file mesm6_defines.sv:");
    ret = 1;
endtask

initial begin
c = 0;
ret = 0;

//--------------------------------------------------------------
// Microcode entry point after reset.
// Initialize cpu registers.
//
if (`UADDR_RESET != c) begin
    print_message();
    $display("`define UADDR_RESET %0d", c);
end
op(`MD_A | `MW_IMM(0) | `W_M | `CLEAR_C);               // m0 = 0, C = 0
op(`ACC_REG | `MR_IMM(0) | `W_A);                       // acc = 0
op(`MD_A | `MW_IMM(1) | `W_M);                          // m1 = 0
op(`MD_A | `MW_IMM(2) | `W_M);                          // m2 = 0
op(`MD_A | `MW_IMM(3) | `W_M);                          // m3 = 0
op(`MD_A | `MW_IMM(4) | `W_M);                          // m4 = 0
op(`MD_A | `MW_IMM(5) | `W_M);                          // m5 = 0
op(`MD_A | `MW_IMM(6) | `W_M);                          // m6 = 0
op(`MD_A | `MW_IMM(7) | `W_M);                          // m7 = 0
op(`MD_A | `MW_IMM(8) | `W_M);                          // m8 = 0
op(`MD_A | `MW_IMM(9) | `W_M);                          // m9 = 0
op(`MD_A | `MW_IMM(10) | `W_M);                         // m10 = 0
op(`MD_A | `MW_IMM(11) | `W_M);                         // m11 = 0
op(`MD_A | `MW_IMM(12) | `W_M);                         // m12 = 0
op(`MD_A | `MW_IMM(13) | `W_M);                         // m13 = 0
op(`MD_A | `MW_IMM(14) | `W_M);                         // m14 = 0
op(`MD_A | `MW_IMM(15) | `W_M);                         // sp = 0
op(`PC_IMM(`RESET_VECTOR) | `W_PC | `EXIT_INTERRUPT);   // pc = RESET_VECTOR, enable interrupts on reset
// fall throught fetch/decode

//--------------------------------------------------------------
// Fetch / decode.
//
//    opcode=mem[pc]
//    decode (goto microcode entry point for opcode)
//
uaddr_fetch = c;
op(`MEM_FETCH | `W_OPCODE);                                 // opcode_cache = mem[pc]
op(`DECODE);                                                // decode jump to microcode

//--------------------------------------------------------------
// Next opcode.
//
//    pc = pc + 1
//    opcode cached ? decode : goto fetch
//    decode (goto microcode entry point for opcode)
//
uaddr_fetch_next = c;
op(`PC_PLUS1);                                              // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

//--------------------------------------------------------------
// Interrupt request.
//
//    sp = sp - 1
//    mem[sp] = pc
//    pc = mem[EMULATED_VECTORS + 0]
//
if (`UADDR_INTERRUPT != c) begin
    print_message();
    $display("`define UADDR_INTERRUPT %0d", c);
end
op(`ENTER_INTERRUPT);                                       // disable interrupts
op(`GO_FETCH_OR_DECODE);

//--------------------------------------------------------------
// Opcodes 000-077.
//
opcode('o000);  // ATX
op(`GO_NEXT);

opcode('o001);  // STX
op(`GO_NEXT);

opcode('o002);  // MOD
op(`GO_NEXT);

opcode('o003);  // XTS
op(`GO_NEXT);

opcode('o004);  // A+X
op(`GO_NEXT);

opcode('o005);  // A-X
op(`GO_NEXT);

opcode('o006);  // X-A
op(`GO_NEXT);

opcode('o007);  // AMX
op(`GO_NEXT);

opcode('o010);  // XTA
op(`GO_NEXT);

opcode('o011);  // AAX
op(`GO_NEXT);

opcode('o012);  // AEX
op(`GO_NEXT);

opcode('o013);  // ARX
op(`GO_NEXT);

opcode('o014);  // AVX
op(`GO_NEXT);

opcode('o015);  // AOX
op(`GO_NEXT);

opcode('o016);  // A/X
op(`GO_NEXT);

opcode('o017);  // A*X
op(`GO_NEXT);

opcode('o020);  // APX
op(`GO_NEXT);

opcode('o021);  // AUX
op(`GO_NEXT);

opcode('o022);  // ACX
op(`GO_NEXT);

opcode('o023);  // ANX
op(`GO_NEXT);

opcode('o024);  // E+X
op(`GO_NEXT);

opcode('o025);  // E-X
op(`GO_NEXT);

opcode('o026);  // ASX
op(`GO_NEXT);

opcode('o027);  // XTR
op(`GO_NEXT);

opcode('o030);  // RTE
op(`GO_NEXT);

opcode('o031);  // YTA
op(`GO_NEXT);

opcode('o032);  // 032
op(`GO_NEXT);

opcode('o033);  // EXT
op(`GO_NEXT);

opcode('o034);  // E+N
op(`GO_NEXT);

opcode('o035);  // E-N
op(`GO_NEXT);

opcode('o036);  // ASN
op(`GO_NEXT);

opcode('o037);  // NTR
op(`GO_NEXT);

opcode('o040);  // ATI
op(`GO_NEXT);

opcode('o041);  // STI
op(`GO_NEXT);

opcode('o042);  // ITA
op(`GO_NEXT);

opcode('o043);  // ITS
op(`GO_NEXT);

opcode('o044);  // MTJ
op(`GO_NEXT);

opcode('o045);  // J+M
op(`GO_NEXT);

opcode('o046);  // E46
op(`GO_NEXT);

opcode('o047);  // E47
op(`GO_NEXT);

opcode('o050);  // E50
op(`GO_NEXT);

opcode('o051);  // E51
op(`GO_NEXT);

opcode('o052);  // E52
op(`GO_NEXT);

opcode('o053);  // E53
op(`GO_NEXT);

opcode('o054);  // E54
op(`GO_NEXT);

opcode('o055);  // E55
op(`GO_NEXT);

opcode('o056);  // E56
op(`GO_NEXT);

opcode('o057);  // E57
op(`GO_NEXT);

opcode('o060);  // E60
op(`GO_NEXT);

opcode('o061);  // E61
op(`GO_NEXT);

opcode('o062);  // E62
op(`GO_NEXT);

opcode('o063);  // E63
op(`GO_NEXT);

opcode('o064);  // E64
op(`GO_NEXT);

opcode('o065);  // E65
op(`GO_NEXT);

opcode('o066);  // E66
op(`GO_NEXT);

opcode('o067);  // E67
op(`GO_NEXT);

opcode('o070);  // E70
op(`GO_NEXT);

opcode('o071);  // E71
op(`GO_NEXT);

opcode('o072);  // E72
op(`GO_NEXT);

opcode('o073);  // E73
op(`GO_NEXT);

opcode('o074);  // E74
op(`GO_NEXT);

opcode('o075);  // E75
op(`GO_NEXT);

opcode('o076);  // E76
op(`GO_NEXT);

opcode('o077);  // E77
op(`GO_NEXT);

//--------------------------------------------------------------
// Opcodes 20-37.
opcode('o200);  // E20
op(`GO_NEXT);

opcode('o210);  // E21
op(`GO_NEXT);

opcode('o220);  // UTC
op(`GO_NEXT);

opcode('o230);  // WTC
op(`GO_NEXT);

opcode('o240);  // VTM
op(`GO_NEXT);

opcode('o250);  // UTM
op(`GO_NEXT);

opcode('o260);  // UZA
op(`GO_NEXT);

opcode('o270);  // UIA
op(`GO_NEXT);

opcode('o300);  // UJ
op(`GO_NEXT);

opcode('o310);  // VJM
op(`GO_NEXT);

opcode('o320);  // IJ
op(`GO_NEXT);

opcode('o330);  // STOP
op(`GO_NEXT);

opcode('o340);  // VZM
op(`GO_NEXT);

opcode('o350);  // VIM
op(`GO_NEXT);

opcode('o360);  // E36
op(`GO_NEXT);

opcode('o370);  // VLM
op(`GO_NEXT);

`ifdef notdef
// ----- OPCODES WITHOUT CONSTANT ------

// 0000_0010 (02) pushsp  -------------------------------------
//    mem[sp-1] = sp
//    sp = sp - 1
//op(`ADDR_SP | `NOP | `W_A);                               // a = sp
//op(`SP_MINUS_4);                                          // sp = sp - 1
//op(`ADDR_SP | `MEM_W | `GO_NEXT);                         // mem[sp]=a

// 0000_0011 (03) popint  -------------------------------------
// pc=mem[sp]-1        (emulate stores pc+1 but we must return to
// sp=sp+1             pc because interrupt takes precedence to decode)
// fetch & decode, then clear_interrupt_flag
// this guarantees that a continous interrupt allows to execute at least one
// opcode of mainstream program before reentry to interrupt handler
op(`ADDR_SP | `MEM_R | `PLUS | `W_PC |                      // pc = mem[sp]-1
             `ALU_CONST(-1 & 127));
op(`MEM_FETCH | `W_OPCODE);                                 // opcode_cache = mem[pc]
op(`SP_PLUS_4 | `DECODE | `EXIT_INTERRUPT);                 // sp=sp+1, decode opcode, exit_interrupt

// 0000_0100 (04) poppc  -------------------------------------
//    pc=mem[sp]
//    sp = sp + 1
op(`ADDR_SP | `MEM_R | `W_PC);                              // pc = mem[sp]
op(`SP_PLUS_4);                                             // sp = sp + 1
op(`GO_FETCH_OR_DECODE);                                    // opcode cached ? decode : fetch,decode

// 0000_0101 (05) add   -------------------------------------
//    mem[sp+1] = mem[sp+1] + mem[sp]
//    sp = sp + 1
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);              // a = mem[sp] || sp=sp+1
op(`ADDR_SP | `MEM_R | `PLUS | `W_A);                       // a = a + mem[sp]
op(`ADDR_SP | `MEM_W | `GO_NEXT);                           // mem[sp] = a

// 0000_0110 (06) and   -------------------------------------
//    mem[sp+1] = mem[sp+1] & mem[sp]
//    sp = sp + 1
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);              // a = mem[sp] || sp=sp+1
op(`ADDR_SP | `MEM_R | `AND | `W_A);                        // a = a & mem[sp]
op(`ADDR_SP | `MEM_W | `GO_NEXT);                           // mem[sp] = a

// 0000_0111 (07) or   -------------------------------------
//    mem[sp+1] = mem[sp+1] | mem[sp]
//    sp = sp + 1
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);              // a = mem[sp] || sp=sp+1
op(`ADDR_SP | `MEM_R | `OR | `W_A);                         // a = a | mem[sp]
op(`ADDR_SP | `MEM_W | `GO_NEXT);                           // mem[sp] = a

// 0000_1000 (08) load   -------------------------------------
//    mem[sp] = mem[ mem[sp] ]
op(`ADDR_SP | `MEM_R | `W_A);                               // a = mem[sp]
op(`MEM_R | `W_A);                                          // a = mem[a]
op(`ADDR_SP | `MEM_W | `GO_NEXT);                           // mem[sp] = a

// 0000_1001 (09) not   -------------------------------------
//    mem[sp] = ~mem[sp]
op(`ADDR_SP | `MEM_R | `NOT | `W_A);                        // a = ~mem[sp]
op(`ADDR_SP | `MEM_W | `GO_NEXT);                           // mem[sp] = a

// 0000_1011 (0b) nop   -------------------------------------
op(`PC_PLUS1);
op(`GO_FETCH_OR_DECODE);

// 0000_1100 (0c) store   -------------------------------------
//    mem[mem[sp]] <= mem[sp+1]
//    sp = sp + 2
op(`ADDR_SP | `MEM_R | `W_B);                               // b = mem[sp]
op(`SP_PLUS_4);                                             // sp = sp + 1
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);              // a = mem[sp] || sp = sp + 1
op(`MEM_W | `GO_NEXT);                                      // mem[b] = a

// 0000_1101 (0d) popsp   -------------------------------------
//    sp = mem[sp]
op(`ADDR_SP | `MEM_R | `W_M | `GO_NEXT);                    // sp = mem[sp]

// ------------- microcode opcode continuations ---------------
// wset_continue1: ------------------------
op(`PLUS | `W_A | `ALU_CONST(12));                          // a = a+12    save clear stack on mem[4]
op(`MEM_W);                                                 // mem[b] = a
op(`ADDR_SP | `MEM_R | `W_PC);                              // pc = mem[sp] (data)
op(`SP_PLUS_4);                                             // sp = sp+4
op(`ADDR_SP | `MEM_R | `W_B);                               // b = mem[sp] (count)
op(`SP_PLUS_4);                                             // sp = sp+4
op(`ADDR_SP | `MEM_R | `W_M);                               // sp = mem[sp] (destination @)
op(`W_A);                                                   // a = b (count)
// wset_loop:
op(`BRANCHIF_A_ZERO(80));                                   // if (a==0) goto @wset_end
op(`PLUS | `W_B | `ALU_CONST(-1 & 127));                    // b = b-1 (count)
op(`W_A);                                                   // a = pc (data)
op(`ADDR_SP | `MEM_W | `SP_PLUS_4);                         // mem[sp] = a || sp = sp+4 (sp=destination@)
op(`W_A | `BRANCH(72));                                     // a = b (count) || goto @wset_loop
// wset_end: wcpy_end: sncpy_end:
op(`MEM_R | `W_PC);                                         // pc = mem[a] (a is 0)
op(`NOP_B | `ALU_CONST(4) | `W_B);                          // b = 4
op(`MEM_R | `W_M | `GO_FETCH_OR_DECODE);                    // sp=mem[b] || goto @fetch

// wcpy_continue1: ------------------------
op(`PLUS | `ALU_CONST(12) | `W_A);                          // a = a+12    save clear stack on mem[4]
op(`MEM_W);                                                 // mem[b] = a
op(`ADDR_SP | `MEM_R | `W_B);                               // b = mem[sp] (count)
op(`SP_PLUS_4);                                             // sp = sp+4
op(`ADDR_SP | `MEM_R | `W_PC);                              // pc = mem[sp] (destination @)
op(`SP_PLUS_4);                                             // sp = sp+4
op(`ADDR_SP | `MEM_R | `W_M);                               // sp = mem[sp] (source @)
op(`W_A);                                                   // a = b (count)
// wcpy_loop:
op(`BRANCHIF_A_ZERO(80));                                   // if (a==0) goto @wcpy_end
op(`PLUS | `W_B | `ALU_CONST(-1 & 127));                    // b = b-1 (count)
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);              // a = mem[sp] || sp = sp+4 (sp=source@)
op(`MEM_W | `PLUS | `W_PC | `ALU_CONST(4));                 // mem[pc] = a || pc = pc+4 (pc=destination@)
op(`W_A | `BRANCH(92));                                     // a = b (count) || goto @wcpy_loop

// -------------------------------------------------------------

// 001_00000 (20) wcpy -----------------------------------------
// before using this opcode you must save mem[0] & mem[4] words, then wcpy, then restore mems
// c=mem[sp],d=mem[sp+1],s=mem[sp+2]); while(c-->0) mem[d++]=mem[s++]); sp=sp+3
op(`NOP_B | `ALU_CONST(0) | `W_B);                          // b = 0
op(`PLUS | `ALU_CONST(1) | `W_A);                           // a = pc+1
op(`MEM_W | `NOP_B | `W_B | `ALU_CONST(4));                 // mem[b] = a || b = 4
op(`ADDR_SP | `W_A | `BRANCH(84));                          // a = sp || goto @wcpy_continue1

// 001_00001 (21) wset ----------------------------------------
// before using this opcode you must save mem[0] & mem[4] words, then wset, then restore mems
// v=mem[sp],c=mem[sp+1],d=mem[sp+2]; while(c-->0) mem[d++]=v; sp=sp+3
op(`NOP_B | `ALU_CONST(0) | `W_B);                          // b = 0
op(`PLUS | `ALU_CONST(1) | `W_A);                           // a = pc+1
op(`MEM_W | `NOP_B | `W_B | `ALU_CONST(4));                 // mem[b] = a || b = 4
op(`ADDR_SP | `W_A | `BRANCH(64));                          // a = sp || goto @wset_continue1

// 001_01011 (2b) ashiftleft   -------------------------------------
// a = mem[sp] & 5'b11111
// sp = sp + 1
// b = mem[sp]
// label: a <= 0 ? goto @fin
// b = b << 1
// a = a - 1 || goto @label
// fin: a = b
// mem[sp] = a
op(`ADDR_SP | `MEM_R | `AND | `W_A | `ALU_CONST(31));       // a = mem[sp] & 5'b11111
op(`SP_PLUS_4);                                             // sp = sp + 1
op(`ADDR_SP | `MEM_R | `W_B);                               // b = mem[sp]
op(`BRANCH(440));                                           // goto @ashiftleft_loop

// 001_01101 (2d) call   -------------------------------------
//    a = mem[sp]
//    mem[sp]=pc+1
//    pc = a
op(`ADDR_SP | `MEM_R | `W_B);                               // b = mem[sp]
op(`PLUS | `ALU_CONST(1) | `W_A);                           // a = pc + 1
op(`ADDR_SP | `MEM_W | `NOP_B | `W_PC);                     // mem[sp] = a || pc = b
op(`GO_FETCH_OR_DECODE);                                    // op_cached ? decode : goto next

// 001_01110 (2e) eq   -------------------------------------
//    a = mem[sp]
//    sp = sp + 1
//    (mem[sp] - a == 0) ? mem[sp] = 1 : mem[sp] = 0
op(`ADDR_SP | `MEM_R | `NOT | `W_A);                        // a = NOT(mem[sp])
op(`PLUS | `ALU_CONST(1) | `W_A);                           // a = a + 1
op(`SP_PLUS_4);                                             // sp = sp + 1
op(`ADDR_SP | `MEM_R | `PLUS | `W_A | `BRANCH(416));        // a = mem[sp] + a || goto @eq_check


// 001_01111 (2f) neq   -------------------------------------
//    a = mem[sp]
//    sp = sp + 1
//    (mem[sp] - a != 0) ? mem[sp] = 1 : mem[sp] = 0
op(`ADDR_SP | `MEM_R | `NOT | `W_A);                        // a = NOT(mem[sp])
op(`PLUS | `ALU_CONST(1) | `W_A);                           // a = a + 1
op(`SP_PLUS_4);                                             // sp = sp + 1
op(`ADDR_SP | `MEM_R | `PLUS | `W_A | `BRANCH(412));        // a = mem[sp] + a || goto @neq_check


// 001_10000 (30) neg   -------------------------------------
//    a = NOT(mem[sp])
//    a = a + 1
//    mem[sp] = a
op(`ADDR_SP | `MEM_R | `NOT | `W_A);                        // a = NOT(mem[sp])
op(`PLUS | `ALU_CONST(1) | `W_A);                           // a = a + 1
op(`ADDR_SP | `MEM_W | `GO_NEXT);                           // mem[sp] = a

// 001_10001 (31) sub   -------------------------------------
//    mem[sp+1] = mem[sp+1] - mem[sp]
//  sp = sp + 1
op(`ADDR_SP | `MEM_R | `NOT | `W_A);                        // a = NOT(mem[sp])
op(`PLUS | `ALU_CONST(1) | `W_A);                           // a = a + 1
op(`SP_PLUS_4);                                             // sp = sp + 1
op(`ADDR_SP | `MEM_R | `PLUS | `W_A | `BRANCH(400));        // a = mem[sp] + a || goto @sub_cont (set_mem[sp]=a)

// 001_10010 (32) xor   -------------------------------------
// ALU doesn't perform XOR operation
// mem[sp+1] = mem[sp] ^ mem[sp+1]  -> A^B=(A&~B)|(~A&B)
// a = ~mem[sp] --> a = ~A
// sp = sp + 1
// a = mem[sp] & a --> a = ~A&B
// b = ~a  --> b = A&~B
// a = a | b --> a = ~A&B | A&~B
// mem[sp] = a
op(`ADDR_SP | `MEM_R | `NOT | `W_A);                        // a = ~mem[sp] --> a=~A
op(`SP_PLUS_4);                                             // sp = sp + 1
op(`ADDR_SP | `MEM_R | `AND | `W_A);                        // a = mem[sp] & a --> a = ~A&B
op(`NOT | `W_B | `BRANCH(428));                             // b = ~a || goto @xor_cont --> b = A&~B

// 001_10011 (33) loadb   -------------------------------------
// b=pc
// pc = mem[sp]
// opcode_cache=mem[pc]
// a = opcode
// mem[sp]=a
// pc=b
// fetch
op(`W_B);                                                   // b = pc
op(`ADDR_SP | `MEM_R | `W_PC);                              // pc = mem[sp]
op(`MEM_FETCH | `W_OPCODE);                                 // opcode_cache = mem[pc]
op(`NOP_B | `W_A | `BRANCH(396));                           // a = opcode -> byte(pc, mem[pc]) || goto @loadb_continued

// 001_10111 (37) eqbranch   -------------------------------------
//    a = sp + 1
//    a = mem[a]
//    a = mem[sp] || a == 0 ? { pc = pc + a; sp = sp + 2 }
//    else { sp = sp + 2, pc = pc + 1 }
op(`ADDR_SP | `PLUS | `ALU_CONST(4) | `W_A);                // a = sp + 1
op(`MEM_R | `W_A);                                          // a = mem[a]
op(`ADDR_SP | `MEM_R | `W_A | `BRANCHIF_A_ZERO(456));       // a = mem[sp] || a == 0 ? goto 456 (sp=sp+2, pc=pc+a)
op(`BRANCH(460));                                           // else goto 460 (sp=sp+2, pc=pc+1)

// 001_11000 (38) neqbranch   -------------------------------------
//    a = sp + 1
//    a = mem[a]
//    a = mem[sp] || a == 0 ? { sp = sp + 2, pc = pc + 1 }
//    else { sp = sp + 2, pc = pc + a }
op(`ADDR_SP | `PLUS | `ALU_CONST(4) | `W_A);                // a = sp + 1
op(`MEM_R | `W_A);                                          // a = mem[a]
op(`ADDR_SP | `MEM_R | `W_A | `BRANCHIF_A_ZERO(460));       // a = mem[sp] || a == 0 ? goto 460 (sp=sp+2, pc=pc+1)
op(`BRANCH(456));                                           // else goto 456 (sp=sp+2, pc=pc+a)

// 001_11001 (39) poppcrel   -------------------------------------
//    a = mem[sp]
//    sp = sp + 1
//    pc = pc + a
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);              // a=mem[sp] || sp=sp+1
op(`PLUS | `W_PC);                                          // pc = pc + a
op(`GO_FETCH_OR_DECODE);                                    // op_cached? decode : goto next

// 001_11011 (3b) pushpc   -------------------------------------
//    sp = sp - 1
//    mem[sp] = pc
op(`SP_MINUS_4 | `W_A);                                   // a = sp = sp - 1
op(`ADDR_SP | `MEM_W | `GO_NEXT);                         // mem[sp] = a

// 001_11101 (3d) pushspadd   -------------------------------------
//    a = mem[sp] << 2
//    mem[sp] = a + sp
op(`ADDR_SP | `MEM_R | `W_A_MEM);                           // a = mem[sp]
op(`PLUS | `W_A);                                           // a = a + a
op(`PLUS | `W_A);                                           // a = a + a
op(`ADDR_SP | `PLUS | `W_A | `BRANCH(400));                 // a = a + sp || goto @cont (->mem[sp] = a)

// 001_11111 (3f) callpcrel   -------------------------------------
//    a = mem[sp]
//    mem[sp]=pc+1
//    pc = pc + a
op(`ADDR_SP | `MEM_R | `W_B);                               // b = mem[sp]
op(`PLUS | `ALU_CONST(1) | `W_A);                           // a = pc + 1
op(`ADDR_SP | `MEM_W);                                      // mem[sp] = a;
op(`PLUS | `W_PC | `GO_FETCH);                              // pc = pc + b, goto @fetch

// --------------------- MICROCODE HOLE -----------------------------------

// --------------------- CONTINUATION OF COMPLEX OPCODES ------------------

// loadb continued microcode -----
// mem[sp]=a || pc=b
// opcode_cache=mem[pc] || go next
op(`ADDR_SP | `MEM_W | `NOP_B | `W_PC);                     // mem[sp] = a || pc=b
op(`MEM_FETCH | `W_OPCODE | `GO_NEXT);                      // opcode_cache = mem[pc] || go next

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
op(`OR | `W_A);                                             // a = a | b --> a = ~A&B | A&~B
op(`ADDR_SP | `MEM_W | `GO_NEXT);                           // mem[sp] = a

// ashiftright_loop continued microcode -----------------------------------

// ashiftleft_loop continued microcode -----------------------------------
op(`BRANCHIF_A_ZERO(444) | `BRANCHIF_A_NEG(444));           // (a <= 0) ? goto @ashiftleft_exit
op(`PLUS | `W_A | `ALU_CONST(-1 & 127));                    // a = a + (-1)
op(`PLUS | `W_B | `BRANCH(440));                            // b = b << 1 || goto @ashiftleft_loop

// ashiftleft_exit
op(`NOP | `W_A);                                            // a = b
op(`ADDR_SP | `MEM_W | `GO_NEXT);                           // mem[sp] = a

// neqbranch / eqbranch --- continued microcode   -------------------------------------
//    sp = sp + 2
//    pc = pc + a
op(`ADDR_SP | `PLUS | `ALU_CONST(8) | `W_M);                // sp = sp + 2
op(`PLUS | `W_PC);                                          // pc = pc + a
op(`GO_FETCH_OR_DECODE);                                    // op_cached ? decode : goto fetch

// neqbranch / eqbranch  --- continued microcode   -------------------------------------
//    sp = sp + 2
//  pc = pc + 1
op(`ADDR_SP | `PLUS | `ALU_CONST(8) | `W_M);                // sp = sp + 2
op(`PC_PLUS1);                                              // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // op_cached? decode : goto fetch

// neq / eq / lessthan_1 --- continued microcode   --------------------
//     mem[sp] = 1
op(`ALU_CONST(1) | `NOP_B | `W_A);                        // a = 1
op(`ADDR_SP | `MEM_W | `GO_NEXT);                         // mem[sp] = a

// neq / eq / lessthan_0 --- continued microcode   --------------------
//    mem[sp] = 0
op(`ALU_CONST(0) | `NOP_B | `W_A);                        // a = 0
op(`ADDR_SP | `MEM_W | `GO_NEXT);                         // mem[sp] = a

// ---------------- OPCODES WITH PARAMETER IN OPCODE ----------------

// 010_xxxxx storesp x
//    mem[sp + x<<2] = mem[sp]
//    sp = sp + 1
op(`ADDR_SP | `PLUS_OFFSET | `W_B);                         // b = sp + offset
op(`ADDR_SP | `MEM_R | `W_A_MEM | `SP_PLUS_4);              // a=mem[sp] || sp=sp+1
op(`MEM_W | `GO_NEXT);                                      // mem[b] = a

// 011_xxxxx loadsp x       -------------------------------------
//    mem[sp-1] = mem [sp + x<<2]
//    sp = sp - 1
op(`ADDR_SP | `PLUS_OFFSET | `W_A);                         // a = sp + offset
op(`MEM_R | `W_A);                                          // a = mem[a]
op(`SP_MINUS_4);                                            // sp = sp - 1
op(`ADDR_SP | `MEM_W | `GO_NEXT);                           // mem[sp] = a

// 0001_xxxx addsp x       -------------------------------------
//     mem[sp] = mem[sp] + mem[sp + x<<2]
op(`ADDR_SP | `PLUS_OFFSET | `W_A);                         // a = sp + offset
op(`MEM_R | `W_A);                                          // a = mem[a]
op(`ADDR_SP | `MEM_R | `PLUS | `W_A);                       // a = a + mem[sp]
op(`ADDR_SP | `MEM_W | `GO_NEXT);                           // mem[sp] = a
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

$finish(ret);
end // initial
endmodule
