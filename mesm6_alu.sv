`timescale 1ns / 1ps
`include "mesm6_defines.sv"

module mesm6_alu(
    input  wire [47:0]              alu_a,      // parameter A
    input  wire [47:0]              alu_b,      // parameter B
    output reg  [47:0]              alu_r,      // computed result
    input  wire [`ALU_OP_WIDTH-1:0] alu_op,     // ALU operation
    input  wire                     clk,        // clock for syncronous multicycle operations
    output reg                      done        // done signal for alu operation
);

// ALU_B is an offset if ALU_PLUS_OFFSET operation
wire [47:0] b_offset = { {38{1'b0}}, ~alu_b[4], alu_b[3:0], 2'b0 };

wire [47:0] b_value =
    (alu_op == `ALU_PLUS_OFFSET) ? b_offset     // B offset
                                 : alu_b;       // by default, ALU_B as is

// ----- alu operation selection -----
always @(alu_a or alu_b or alu_op or b_value)
begin
    done <= 1;        // default alu operations are 1 cycle
    case (alu_op)
    `ALU_NOP        : alu_r <= alu_a;
    `ALU_NOP_B      : alu_r <= alu_b;
    `ALU_AND        : alu_r <= alu_a & alu_b;
    `ALU_OR         : alu_r <= alu_a | alu_b;
    `ALU_NOT        : alu_r <= ~alu_a;
    default         : alu_r <= alu_a + b_value;
    endcase
end

endmodule
