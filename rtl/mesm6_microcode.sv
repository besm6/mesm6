`timescale 1ns / 1ps
`include "mesm6_defines.sv"

// Accumulator source selector
`define ACC_ALU                 (`SEL_ACC_ALU << `P_SEL_ACC)
`define ACC_MEM                 (`SEL_ACC_MEM << `P_SEL_ACC)
`define ACC_REG                 (`SEL_ACC_REG << `P_SEL_ACC)
`define ACC_RR                  (`SEL_ACC_RR << `P_SEL_ACC)
`define ACC_Y                   (`SEL_ACC_Y << `P_SEL_ACC)

// M[r] read index selector
`define MR_REG                  (`SEL_MR_REG << `P_SEL_MR)
`define MR_IMM(val)             (`SEL_MR_IMM << `P_SEL_MR | (val) << `P_IMM)
`define MR_VA                   (`SEL_MR_VA << `P_SEL_MR)
`define MR_UA                   (`SEL_MR_UA << `P_SEL_MR)

// M[] write index selector
`define MW_REG                  (`SEL_MW_REG << `P_SEL_MW)
`define MW_IMM(val)             (`SEL_MW_IMM << `P_SEL_MW | (val) << `P_IMM)
`define MW_VA                   (`SEL_MW_VA << `P_SEL_MW)
`define MW_UA                   (`SEL_MW_UA << `P_SEL_MW)

// M[] write data selector
`define MD_PC                   (`SEL_MD_PC << `P_SEL_MD)
`define MD_A                    (`SEL_MD_A << `P_SEL_MD)
`define MD_REG                  (`SEL_MD_REG << `P_SEL_MD)
`define MD_REG_PLUS1            (`SEL_MD_REG_PLUS1 << `P_SEL_MD)
`define MD_REG_MINUS1           (`SEL_MD_REG_MINUS1 << `P_SEL_MD)
`define MD_VA                   (`SEL_MD_VA << `P_SEL_MD)
`define MD_UA                   (`SEL_MD_UA << `P_SEL_MD)

`define ADDR_M(i)               (1 << `P_SEL_ADDR | `MR_IMM(i))
`define ADDR_SP                 `ADDR_M(15)

// PC source selector
`define PC_REG                  (`SEL_PC_REG << `P_SEL_PC)
`define PC_IMM(val)             (`SEL_PC_IMM << `P_SEL_PC | (val) << `P_IMM)
`define PC_VA                   (`SEL_PC_VA << `P_SEL_PC)
`define PC_UA                   (`SEL_PC_UA << `P_SEL_PC)
`define PC_PLUS1                (`SEL_PC_PLUS1 << `P_SEL_PC)

// ALU operations
`define AND                     (`ALU_AND << `P_ALU)
`define OR                      (`ALU_OR << `P_ALU)
`define XOR                     (`ALU_XOR << `P_ALU)
`define SHIFT                   (`ALU_SHIFT << `P_ALU)
`define ADD_CARRY_AROUND        (`ALU_ADD_CARRY_AROUND << `P_ALU)
`define PACK                    (`ALU_PACK << `P_ALU)
`define UNPACK                  (`ALU_UNPACK << `P_ALU)
`define COUNT                   (`ALU_COUNT << `P_ALU)
`define CLZ                     (`ALU_CLZ << `P_ALU)
`define FADD                    (`ALU_FADD << `P_ALU)
`define FSUB                    (`ALU_FSUB << `P_ALU)
`define FREVSUB                 (`ALU_FREVSUB << `P_ALU)
`define FSUBABS                 (`ALU_FSUBABS << `P_ALU)
`define FSIGN                   (`ALU_FSIGN << `P_ALU)
`define ADDEXP                  (`ALU_ADDEXP << `P_ALU)
`define SUBEXP                  (`ALU_SUBEXP << `P_ALU)
`define FMUL                    (`ALU_FMUL << `P_ALU)
`define FDIV                    (`ALU_FDIV << `P_ALU)

// Other micro-instruction fields
`define W_M                     (1 << `P_W_M)
`define W_PC                    (1 << `P_W_PC)
`define W_A                     (1 << `P_W_A)
`define W_C                     (1 << `P_W_C)
`define W_OPCODE                (1 << `P_W_OPCODE)
`define EXIT_INTERRUPT          (1 << `P_EXIT_INT)
`define ENTER_INTERRUPT         (1 << `P_ENTER_INT)
`define C_ACTIVE                (1 << `P_C_ACTIVE)
`define R_ADD                   (1 << `P_R_ADD)
`define C_MEM                   (1 << `P_SEL_C_MEM)
`define ALU_MEM                 (1 << `P_SEL_ALU_MEM)
`define MEM_FETCH               (1 << `P_FETCH)
`define MEM_R                   (1 << `P_MEM_R)
`define MEM_W                   (1 << `P_MEM_W)
`define DECODE                  (1 << `P_DECODE)

// Branches
`define BRANCH(addr)                (1 << `P_BRANCH | (addr) << `P_IMM)
`define BRANCHIF_OP_NOT_CACHED(a)   (1 << `P_OP_NOT_CACHED | (a) << `P_IMM)
`define BRANCHIF_A_ZERO(addr)       (1 << `P_A_ZERO | (addr) << `P_IMM)
`define BRANCHIF_A_NONZERO(addr)    (1 << `P_A_NONZERO | (addr) << `P_IMM)
`define BRANCHIF_A_NEG(addr)        (1 << `P_A_NEG | (addr) << `P_IMM)
`define BRANCHIF_M_ZERO(addr)       (1 << `P_M_ZERO | (addr) << `P_IMM)
`define BRANCHIF_M_NONZERO(addr)    (1 << `P_M_NONZERO | (addr) << `P_IMM)

// Stack increment and decrement
`define STACK_INCR              (`MR_IMM(15) | `MW_IMM(15) | `MD_REG_PLUS1 | `W_M)
`define STACK_DECR              (`MR_IMM(15) | `MW_IMM(15) | `MD_REG_MINUS1 | `W_M)

// Fetch and decode current PC opcode: without and with C modifier active
`define GO_FETCH_OR_DECODE      (`BRANCHIF_OP_NOT_CACHED(uaddr_fetch) | `DECODE)
`define GO_FETCH_OR_DECODE_C    (`BRANCHIF_OP_NOT_CACHED(uaddr_fetch_c) | `DECODE | `C_ACTIVE)

module gendata();

reg [`UOP_BITS-1:0] memory[(1<<`UPC_BITS)-1:0];

bit [`UPC_BITS-1:0] stab[64];
bit [`UPC_BITS-1:0] ltab[16];
bit [`UPC_BITS-1:0] stab_stack[64];
bit [`UPC_BITS-1:0] ltab_stack[16];

reg [`UPC_BITS-1:0] uaddr_fetch;
reg [`UPC_BITS-1:0] uaddr_fetch_c;

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

task stack_mode(integer op);
    if (op[7])
        ltab_stack[op[6:3]] = c;
    else
        stab_stack[op] = c;
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
op(`MD_A | `MW_IMM(0) | `W_M);                          // m0 = 0
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

uaddr_fetch_c = c;
op(`MEM_FETCH | `W_OPCODE);                                 // opcode_cache = mem[pc]
op(`DECODE | `C_ACTIVE);                                    // decode jump to microcode

//--------------------------------------------------------------
// No operation.
//
if (`UADDR_NOP != c) begin
    print_message();
    $display("`define UADDR_NOP %0d", c);
end
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
opcode('o000);          // ATX
op(`MEM_W);                                                 // memory[Uaddr] = A
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o000);      // ATX in stack mode
op(`MEM_W);                                                 // memory[Uaddr] = A (TODO: merge with next)
op(`STACK_INCR);                                            // m[15] = m[15] + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o001);          // STX
op(`MEM_W);                                                 // memory[Uaddr] = A (TODO: merge with next)
op(`STACK_DECR);                                            // m[15] = m[15] - 1
op(`MEM_R | `ADDR_SP | `ACC_MEM | `W_A);                    // A = memory[m15]
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o002);          // MOD
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o003);          // XTS
op(`MEM_W | `ADDR_SP);                                      // memory[m15] = A;
op(`STACK_INCR);                                            // m[15] = m[15] + 1
op(`MEM_R | `ACC_MEM | `W_A);                               // A = memory[Uaddr]
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o004);      // A+X in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o004);          // A+X
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o005);      // A-X in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o005);          // A-X
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o006);      // X-A in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o006);          // X-A
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o007);      // AMX in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o007);          // AMX
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o010);      // XTA in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o010);          // XTA
op(`MEM_R | `ACC_MEM | `W_A);                               // A = memory[Uaddr]
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o011);      // AAX in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o011);          // AAX
op(`MEM_R | `ACC_MEM);                                      // x = memory[Uaddr]
op(`ALU_MEM | `AND | `ACC_ALU | `W_A);                      // a &= x
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o012);      // AEX in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o012);          // AEX
op(`MEM_R | `ACC_MEM);                                      // x = memory[Uaddr]
op(`ALU_MEM | `XOR | `ACC_ALU | `W_A);                      // a ^= x
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o013);      // ARX in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o013);          // ARX
op(`MEM_R | `ACC_MEM);                                      // x = memory[Uaddr]
op(`ALU_MEM | `ADD_CARRY_AROUND | `ACC_ALU | `W_A);         // a += x with carry around
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o014);      // AVX in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o014);          // AVX
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o015);      // AOX in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o015);          // AOX
op(`MEM_R | `ACC_MEM);                                      // x = memory[Uaddr]
op(`ALU_MEM | `OR | `ACC_ALU | `W_A);                       // a |= x
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o016);      // A/X in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o016);          // A/X
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o017);      // A*X in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o017);          // A*X
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o020);      // APX in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o020);          // APX
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o021);      // AUX in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o021);          // AUX
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o022);      // ACX in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o022);          // ACX
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o023);      // ANX in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o023);          // ANX
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o024);      // E+X in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o024);          // E+X
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o025);      // E-X in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o025);          // E-X
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o026);      // ASX in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o026);          // ASX
op(`MEM_R | `ACC_MEM);                                      // x = memory[Uaddr]
op(`ALU_MEM | `SHIFT | `ACC_ALU | `W_A);                    // a <<= x
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

stack_mode('o027);      // XTR in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o027);          // XTR
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o030);          // RTE
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o031);          // YTA
op(`ACC_Y | `W_A);                                          // a <<= y
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o032);          // 032
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o033);          // EXT
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o034);          // E+N
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o035);          // E-N
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o036);          // ASN
op(`SHIFT | `ACC_ALU | `W_A);                               // a <<= Uaddr
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o037);          // NTR
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o040);          // ATI
op(`MW_UA | `MD_A | `W_M | `GO_FETCH_OR_DECODE);            // m[Uaddr] = A; pc_cached ? decode else fetch,decode

opcode('o041);          // STI
op(`MW_UA | `MD_A | `W_M);                                  // m[Uaddr] = A
op(`STACK_DECR);                                            // m[15] = m[15] - 1
op(`MEM_R | `ADDR_SP | `ACC_MEM | `W_A);                    // A = memory[m15]
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o042);          // ITA
op(`ACC_REG | `MR_UA | `W_A | `GO_FETCH_OR_DECODE);         // acc = m[r]; pc_cached ? decode else fetch,decode

opcode('o043);          // ITS
op(`MEM_W | `ADDR_SP | `STACK_INCR);                        // memory[m15] = A; m[15] = m[15] + 1
op(`ACC_REG | `MR_UA | `W_A | `GO_FETCH_OR_DECODE);         // acc = m[r]; pc_cached ? decode else fetch,decode

opcode('o044);          // MTJ
op(`MR_REG | `MW_VA | `MD_REG | `W_M |                      // m[r] = m[i]; pc_cached ? decode else fetch,decode
    `GO_FETCH_OR_DECODE);

opcode('o045);          // J+M
op(`MR_VA | `MW_VA | `MD_UA | `R_ADD | `W_M |               // m[r] = m[i] + m[r]; pc_cached ? decode else fetch,decode
    `GO_FETCH_OR_DECODE);

opcode('o046);          // E46
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o047);          // E47
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o050);          // E50
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o051);          // E51
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o052);          // E52
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o053);          // E53
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o054);          // E54
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o055);          // E55
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o056);          // E56
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o057);          // E57
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o060);          // E60
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o061);          // E61
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o062);          // E62
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o063);          // E63
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o064);          // E64
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o065);          // E65
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o066);          // E66
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o067);          // E67
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o070);          // E70
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o071);          // E71
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o072);          // E72
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o073);          // E73
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o074);          // E74
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o075);          // E75
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o076);          // E76
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o077);          // E77
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

//--------------------------------------------------------------
// Opcodes 20-37.
opcode('o200);          // E20
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o210);          // E21
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o220);          // UTC
op(`W_C);                                                   // C = Uaddr
op(`GO_FETCH_OR_DECODE_C);                                  // pc_cached ? decode else fetch,decode

stack_mode('o230);      // WTC in stack mode
op(`STACK_DECR);                                            // m[15] = m[15] - 1
opcode('o230);          // WTC
op(`MEM_R | `C_MEM | `W_C);                                 // C = memory[Uaddr]
op(`GO_FETCH_OR_DECODE_C);                                  // pc_cached ? decode else fetch,decode

opcode('o240);          // VTM
op(`MW_REG | `MD_VA | `W_M | `GO_FETCH_OR_DECODE);          // pc_cached ? decode else fetch,decode

opcode('o250);          // UTM
op(`MW_REG | `MD_UA | `W_M | `GO_FETCH_OR_DECODE);          // m[i] = Uaddr; pc_cached ? decode else fetch,decode

opcode('o260);          // UZA
op(`BRANCHIF_A_ZERO(c+2));                                  // if (A == 0) goto +2
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode
op(`PC_UA | `W_PC);                                         // pc = Uaddr
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o270);          // UIA
op(`BRANCHIF_A_NONZERO(c+2));                               // if (A != 0) goto +2
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode
op(`PC_UA | `W_PC);                                         // pc = Uaddr
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o300);          // UJ
op(`PC_UA | `W_PC);                                         // pc = Uaddr
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o310);          // VJM
op(`MW_REG | `MD_PC | `W_M | `PC_VA | `W_PC);               // m[i] = pc; pc = Vaddr
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o320);          // IJ
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o330);          // STOP
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o340);          // VZM
op(`BRANCHIF_M_ZERO(c+2));                                  // if (m[i]==0) goto +2
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode
op(`PC_VA | `W_PC);                                         // pc = Vaddr
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o350);          // VIM
op(`BRANCHIF_M_NONZERO(c+2));                               // if (m[i]!=0) goto +2
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode
op(`PC_VA | `W_PC);                                         // pc = Vaddr
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o360);          // E36
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o370);          // VLM
op(`BRANCHIF_M_NONZERO(c+2));                               // if (m[i]!=0) goto +2
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode
op(`MR_REG | `MW_REG | `MD_REG_PLUS1 | `W_M | `PC_VA | `W_PC); // pc = Vaddr; m[i] += 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

// --------------------- END OF MICROCODE PROGRAM --------------------------

// Generate output files.
fd = $fopen("microcode.v", "w");
for(n = 0; n < c; n = n + 1) begin
    $fdisplay(fd, "%3d: %0d'o%o,", n, `UOP_BITS, memory[n]);
end
$fclose(fd);

fd = $fopen("jumptab16.v", "w");
for(n = 0; n < 16; n = n + 1) begin
    $fdisplay(fd, "'o0%2o<<1: %0d'd%0d, 'o0%2o<<1|1: %0d'd%0d,",
        n, `UPC_BITS, ltab[n],
        n, `UPC_BITS, ltab_stack[n] == 0 ? ltab[n] : ltab_stack[n]);
end
$fclose(fd);

fd = $fopen("jumptab64.v", "w");
for(n = 0; n < 64; n = n + 1) begin
    $fdisplay(fd, "'o0%2o<<1: %0d'd%0d, 'o0%2o<<1|1: %0d'd%0d,",
        n, `UPC_BITS, stab[n],
        n, `UPC_BITS, stab_stack[n] == 0 ? stab[n] : stab_stack[n]);
end
$fclose(fd);

$finish(ret);
end // initial
endmodule
