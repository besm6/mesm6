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

assign y = 0;   // TODO

// Internal cycle count.
reg [3:0] count;

// 49-bit sum
wire [48:0] sum = a + b;

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
                done <= 1;
            end

        `ALU_OR: begin
                // AOX: one cycle.
                result <= a | b;
                done <= 1;
            end

        `ALU_XOR: begin
                // AEX: one cycle.
                result <= a ^ b;
                done <= 1;
            end

        `ALU_ADD_CARRY_AROUND: begin
                // ARX: one cycle.
                result <= sum[47:0] + sum[48];
                done <= 1;
            end

        //TODO:`ALU_SHIFT
        //TODO:`ALU_PACK
        //TODO:`ALU_UNPACK
        //TODO:`ALU_COUNT
        //TODO:`ALU_CLZ
        //TODO:`ALU_FADD
        //TODO:`ALU_FSUB
        //TODO:`ALU_FREVSUB
        //TODO:`ALU_FSUBABS
        //TODO:`ALU_FSIGN
        //TODO:`ALU_ADDEXP
        //TODO:`ALU_SUBEXP
        //TODO:`ALU_FMUL
        //TODO:`ALU_FDIV
        default:
            result <= a;
        endcase
    end
end

endmodule
