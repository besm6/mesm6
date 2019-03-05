`define EMULATION_VECTOR    15'o10      // table of emulated opcodes (interrupt & exception vectors plus up to 5 emulated opcodes) TODO: remove
`define RESET_VECTOR        15'o1       // reset entry point (can be moved up to 0x3c as per emulation table needs)

// ------- microcode core datapath selectors --------
`define SEL_READ_DATA           0
`define SEL_READ_ADDR           1

`define SEL_ALU_A               0
`define SEL_ALU_OPCODE          1
`define SEL_ALU_CONST           2
`define SEL_ALU_B               3

`define SEL_ADDR_PC             0
`define SEL_ADDR_SP             1
`define SEL_ADDR_A              2
`define SEL_ADDR_B              3

`define ALU_OP_WIDTH            4   // alu operation is 4 bits

`define ALU_NOP                 0   // r = a
`define ALU_NOP_B               1   // r = b
`define ALU_PLUS                2   // r = a + b
`define ALU_PLUS_OFFSET         3   // r = a + { 27'b0, ~b[4], b[3:0] }
`define ALU_AND                 4   // r = a AND b
`define ALU_OR                  5   // r = a OR b
`define ALU_NOT                 6   // r = NOT a

// ------- special opcodes ------
`define OP_NOP                  8'b0000_1011 // default value for opcode cache on reset
`define OP_EMULATE              3'b001
`define OP_STORESP              3'b010
`define OP_LOADSP               3'b011
`define OP_ADDSP                4'b0001

// ------- microcode memory settings ------
`define UPC_BITS                9   // 512 microcode operations
`define UOP_BITS                36  // microcode opcode width

// ------- microcode labels for opcode execution -------
// based on microcode program
`define UADDR_RESET             0
`define UADDR_FETCH             3
`define UADDR_FETCH_NEXT        5
`define UADDR_INTERRUPT         7
`define UADDR_EMULATE           9

// ---------- microcode settings --------------------
`define P_IMM                   0   // microcode address (9 bits) or constant to be used at microcode level
`define P_SEL_READ              9   // alu-A multiplexor between data-in and addr-out (1 bit)
`define P_SEL_ALU               10  // alu-B multiplexor between a, b, mc_const or opcode (2 bits)
`define P_SEL_ADDR              12  // addr-out multiplexor between sp, pc, a, b (2 bits)
`define P_ALU                   14  // alu operation (4 bits)
`define P_W_RM                  18  // write M[i] (from alu-out)
`define P_W_PC                  19  // write pc (from alu-out)
`define P_W_A                   20  // write a (from alu-out)
`define P_FETCH                 21  // request instruction fetch
`define P_SET_IDIM              22  // unused
`define P_CLEAR_IDIM            23  // unused
`define P_W_OPCODE              24  // write opcode  (from alu-out) : check if can be written directly from data-in
`define P_DECODE                25  // jump to microcode entry point based on current opcode
`define P_MEM_R                 26  // request memory read
`define P_MEM_W                 27  // request memory write
`define P_BRANCH                28  // microcode inconditional branch to address
`define P_OP_NOT_CACHED         29  // microcode branch if byte[pc] is not cached at opcode
`define P_A_ZERO                30  // microcode branch if a is zero
`define P_A_NEG                 31  // microcode branch if a is negative a[31]=1
`define P_W_A_MEM               32  // write a directly from data-in (alu datapath is free to perform any other operation in parallel)
`define P_EXIT_INT              34  // clear interrupt flag (exit from interrupt)
`define P_ENTER_INT             35  // set interrupt flag (enter interrupt)
