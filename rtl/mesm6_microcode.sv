`timescale 1ns / 1ps
`include "mesm6_defines.sv"

`define ACC_ALU                 (`SEL_ACC_ALU << `P_SEL_ACC)
`define ACC_MEM                 (`SEL_ACC_MEM << `P_SEL_ACC)
`define ACC_REG                 (`SEL_ACC_REG << `P_SEL_ACC)
`define ACC_RR                  (`SEL_ACC_RR << `P_SEL_ACC)
`define ACC_Y                   (`SEL_ACC_Y << `P_SEL_ACC)

`define MR_REG                  (`SEL_MR_REG << `P_SEL_MR)
`define MR_IMM(val)             (`SEL_MR_IMM << `P_SEL_MR | (val) << `P_IMM)
`define MR_VADDR                (`SEL_MR_VADDR << `P_SEL_MR)
`define MR_UADDR                (`SEL_MR_UADDR << `P_SEL_MR)

`define MW_REG                  (`SEL_MW_REG << `P_SEL_MW)
`define MW_IMM(val)             (`SEL_MW_IMM << `P_SEL_MW | (val) << `P_IMM)
`define MW_VA                   (`SEL_MW_VA << `P_SEL_MW)
`define MW_UA                   (`SEL_MW_UA << `P_SEL_MW)

`define MD_PC1                  (`SEL_MD_PC1 << `P_SEL_MD)
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
`define W_C                     (1 << `P_W_C)
`define W_OPCODE                (1 << `P_W_OPCODE)
`define EXIT_INTERRUPT          (1 << `P_EXIT_INT)
`define ENTER_INTERRUPT         (1 << `P_ENTER_INT)
`define CLEAR_C                 (1 << `P_CLEAR_C)
`define J_ADD                   (1 << `P_SEL_J_ADD)
`define C_MEM                   (1 << `P_SEL_C_MEM)

`define MEM_FETCH               (1 << `P_FETCH)
`define MEM_R                   (1 << `P_MEM_R)
`define MEM_W                   (1 << `P_MEM_W)

`define DECODE                      (1 << `P_DECODE)
`define BRANCH(addr)                (1 << `P_BRANCH | (addr) << `P_IMM)
`define BRANCHIF_OP_NOT_CACHED(a)   (1 << `P_OP_NOT_CACHED | (a) << `P_IMM)
`define BRANCHIF_A_ZERO(addr)       (1 << `P_A_ZERO | (addr) << `P_IMM)
`define BRANCHIF_A_NEG(addr)        (1 << `P_A_NEG | (addr) << `P_IMM)
`define BRANCHIF_M_ZERO(addr)       (1 << `P_M_ZERO | (addr) << `P_IMM)
`define BRANCHIF_M_NONZERO(addr)    (1 << `P_M_NONZERO | (addr) << `P_IMM)

// microcode common operations
`define GO_FETCH_OR_DECODE      (`BRANCHIF_OP_NOT_CACHED(uaddr_fetch) | `DECODE | `CLEAR_C) // fetch and decode current PC opcode
`define GO_FETCH_OR_DECODE_C    (`BRANCHIF_OP_NOT_CACHED(uaddr_fetch) | `DECODE) // fetch and decode current PC opcode

module gendata();

reg [`UOP_BITS-1:0] memory[(1<<`UPC_BITS)-1:0];

reg [`UPC_BITS-1:0] stab[64];
reg [`UPC_BITS-1:0] ltab[16];

reg [`UPC_BITS-1:0] uaddr_fetch;

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
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o001);  // STX
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o002);  // MOD
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o003);  // XTS
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o004);  // A+X
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o005);  // A-X
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o006);  // X-A
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o007);  // AMX
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o010);  // XTA
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o011);  // AAX
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o012);  // AEX
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o013);  // ARX
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o014);  // AVX
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o015);  // AOX
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o016);  // A/X
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o017);  // A*X
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o020);  // APX
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o021);  // AUX
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o022);  // ACX
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o023);  // ANX
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o024);  // E+X
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o025);  // E-X
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o026);  // ASX
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o027);  // XTR
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o030);  // RTE
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o031);  // YTA
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o032);  // 032
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o033);  // EXT
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o034);  // E+N
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o035);  // E-N
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o036);  // ASN
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o037);  // NTR
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o040);  // ATI
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o041);  // STI
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o042);  // ITA
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o043);  // ITS
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o044);  // MTJ
op(`MW_VA | `MD_REG | `W_M | `PC_PLUS1 | `W_PC);            // m[j] = m[i]; pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o045);  // J+M
op(`MW_VA | `MD_UA | `J_ADD | `W_M | `PC_PLUS1 | `W_PC);    // m[j] = Mi + Mj; pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o046);  // E46
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o047);  // E47
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o050);  // E50
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o051);  // E51
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o052);  // E52
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o053);  // E53
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o054);  // E54
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o055);  // E55
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o056);  // E56
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o057);  // E57
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o060);  // E60
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o061);  // E61
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o062);  // E62
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o063);  // E63
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o064);  // E64
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o065);  // E65
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o066);  // E66
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o067);  // E67
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o070);  // E70
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o071);  // E71
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o072);  // E72
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o073);  // E73
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o074);  // E74
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o075);  // E75
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o076);  // E76
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o077);  // E77
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

//--------------------------------------------------------------
// Opcodes 20-37.
opcode('o200);  // E20
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o210);  // E21
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o220);  // UTC
op(`PC_PLUS1 | `W_PC | `W_C);                               // pc = pc + 1; C = Uaddr
op(`GO_FETCH_OR_DECODE_C);                                  // pc_cached ? decode else fetch,decode

opcode('o230);  // WTC
op(`PC_PLUS1 | `W_PC | `MEM_R | `C_MEM | `W_C);             // pc = pc + 1; C = memory[Uaddr]
op(`GO_FETCH_OR_DECODE_C);                                  // pc_cached ? decode else fetch,decode

opcode('o240);  // VTM
op(`MW_REG | `MD_VA | `W_M | `PC_PLUS1 | `W_PC);            // m[i] = Vaddr; pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o250);  // UTM
op(`MW_REG | `MD_UA | `W_M | `PC_PLUS1 | `W_PC);            // m[i] = Uaddr; pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o260);  // UZA
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o270);  // UIA
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o300);  // UJ
op(`PC_UA | `W_PC);                                         // pc = Uaddr
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o310);  // VJM
op(`MW_REG | `MD_PC1 | `W_M | `PC_VA | `W_PC);              // m[i] = pc+1; pc = Vaddr
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o320);  // IJ
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o330);  // STOP
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o340);  // VZM
op(`BRANCHIF_M_ZERO(c+2) | `PC_PLUS1 | `W_PC);              // pc = pc + 1; if (m[i]==0) goto +2
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode
op(`PC_VA | `W_PC);                                         // pc = Vaddr
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o350);  // VIM
op(`BRANCHIF_M_NONZERO(c+2) | `PC_PLUS1 | `W_PC);           // pc = pc + 1; if (m[i]!=0) goto +2
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode
op(`PC_VA | `W_PC);                                         // pc = Vaddr
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o360);  // E36
op(`PC_PLUS1 | `W_PC);                                      // pc = pc + 1
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode

opcode('o370);  // VLM
op(`BRANCHIF_M_NONZERO(c+2) | `PC_PLUS1 | `W_PC);           // pc = pc + 1; if (m[i]!=0) goto +2
op(`GO_FETCH_OR_DECODE);                                    // pc_cached ? decode else fetch,decode
op(`MW_REG | `MD_REG_PLUS1 | `W_M | `PC_VA | `W_PC);        // pc = Vaddr; m[i] += 1
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
