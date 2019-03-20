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

        `ALU_ADD_CARRY_AROUND: begin
                // ARX: two cycles.
                case (count)
                 0: begin
                        result <= sum[47:0];
                        y <= '0;
                    end
                 1: begin
                        if (sum[48])
                            result <= result + 1;
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

        `ALU_COUNT: begin
                // The "popcount" part of the ACX instruction: one cycle.
                // To implement the ACX instruction, the ALU_ARX uop must follow.
                result <= $countones(a);
                done <= 1;
        end

        //TODO:`ALU_PACK    y <= '0;
        //TODO:`ALU_UNPACK  y <= '0;
        //TODO:`ALU_CLZ     y <= acc << (n+1);
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
