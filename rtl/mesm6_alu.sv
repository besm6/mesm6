`timescale 1ns / 1ps
`include "mesm6_defines.sv"

module mesm6_alu(
    input  wire [47:0]              a,      // parameter A
    input  wire [47:0]              b,      // parameter B
    output reg  [47:0]              result, // computed result
    output reg  [47:0]              y,      // least significant bits
    input  wire [`ALU_OP_WIDTH-1:0] op,     // ALU operation
    input  wire                     clk,    // clock for syncronous multicycle operations
    output reg                      done    // flag: alu operation finished
);

// Internal cycle count.
reg [3:0] count;

// Count leading zeroes (plus 1).
wire [5:0] clz =
    a[47] ? 1  : a[46] ? 2  : a[45] ? 3  : a[44] ? 4 :
    a[43] ? 5  : a[42] ? 6  : a[41] ? 7  : a[40] ? 8 :
    a[39] ? 9  : a[38] ? 10 : a[37] ? 11 : a[36] ? 12 :
    a[35] ? 13 : a[34] ? 14 : a[33] ? 15 : a[32] ? 16 :
    a[31] ? 17 : a[30] ? 18 : a[29] ? 19 : a[28] ? 20 :
    a[27] ? 21 : a[26] ? 22 : a[25] ? 23 : a[24] ? 24 :
    a[23] ? 25 : a[22] ? 26 : a[21] ? 27 : a[20] ? 28 :
    a[19] ? 29 : a[18] ? 30 : a[17] ? 31 : a[16] ? 32 :
    a[15] ? 33 : a[14] ? 34 : a[13] ? 35 : a[12] ? 36 :
    a[11] ? 37 : a[10] ? 38 : a[9]  ? 39 : a[8]  ? 40 :
    a[7]  ? 41 : a[6]  ? 42 : a[5]  ? 43 : a[4]  ? 44 :
    a[3]  ? 45 : a[2]  ? 46 : a[1]  ? 47 : a[0]  ? 48 : 0;

// 49-bit adder
wire [47:0] adder_a =                       // mux at A input of adder
    (op == `ALU_COUNT) ? $countones(a) :    // number of 1s in accumulator
    (op == `ALU_CLZ)   ? clz :              // count leading zeroes + 1
                         a;                 // accumulator
wire [48:0] sum = adder_a + b;              // adder
reg carry;                                  // carry bit, latched

// ----- alu operation selection -----
always @(posedge clk) begin
    if (op == `ALU_NOP) begin
        // No operation: reset count and done flag.
        done <= 0;
        count <= 0;

    end else if (~done) begin
        // Perform the operation.
        count <= count + 1;

        case (op)
        `ALU_AND: begin
                // AAX: one cycle.
                result <= a & b;
                y <= '0;
                done <= 1;
            end

        `ALU_OR: begin
                // AOX: one cycle.
                result <= a | b;
                y <= '0;
                done <= 1;
            end

        `ALU_XOR: begin
                // AEX: one cycle.
                result <= a ^ b;
                y <= a;
                done <= 1;
            end

        `ALU_ADD_CARRY_AROUND,
        `ALU_COUNT: begin
                // ARX, ACX: two cycles.
                case (count)
                 0: begin
                        {carry, result} <= sum;
                        y <= '0;
                    end
                 1: begin
                        result <= result + carry;
                        done <= 1;
                    end
                endcase
            end

        `ALU_CLZ: begin
                // ANX: two cycles.
                case (count)
                 0: begin
                        {carry, result} <= sum;
                        y <= a << clz;
                    end
                 1: begin
                        result <= result + carry;
                        done <= 1;
                    end
                endcase
            end

        `ALU_SHIFT: begin
                // ASX, ASN: one cycle.
                if (b[47]) begin
                    // shift right
                    {result, y} <= {a, 48'b0} >> b[46:41];
                end else begin
                    // shift left
                    {y, result} <= {48'b0, a} << (6'd64 - b[46:41]);
                end
                done <= 1;
            end

        //TODO:`ALU_PACK    y <= '0;
        //TODO:`ALU_UNPACK  y <= '0;
        //TODO:`ALU_FADD
        //TODO:`ALU_FSUB
        //TODO:`ALU_FREVSUB
        //TODO:`ALU_FSUBABS
        //TODO:`ALU_FSIGN
        //TODO:`ALU_ADDEXP
        //TODO:`ALU_SUBEXP
        //TODO:`ALU_FMUL
        //TODO:`ALU_FDIV
        endcase
    end
end

endmodule
