//
// MESM-6 processor: arithmetic unit.
//
// Copyright (c) 2019 Serge Vakulenko
// Copyright (c) 2019 Leo Broukhis
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
`timescale 1ns / 1ps
`include "mesm6_defines.sv"

module mesm6_alu(
    input  wire                     clk,        // clock for syncronous multicycle operations
    input  wire [`ALU_OP_WIDTH-1:0] op,         // ALU operation
    input  wire                     wy,         // write Y := A
    input  wire                     grp_log,    // logical group
    input  wire                     do_norm,    // normalization enabled
    input  wire                     do_round,   // rounding enabled
    input  wire [47:0]              a,          // operand A
    input  wire [47:0]              b,          // operand B
    output reg  [47:0]              acc,        // computed result
    output reg                      done        // flag: alu operation finished
);

// Internal progress indicator.
enum reg [3:0] {
    STATE_IDLE,
    STATE_SHIFTING,
    STATE_PACKING,
    STATE_UNPACKING,
    STATE_ADD_CARRY,
    STATE_ADD_B,
    STATE_NORM_BEFORE,
    STATE_ADDING,
    STATE_DIVIDING,
    STATE_NORM_AFTER,
    STATE_ROUND
} state;

// Y register.
reg [47:0] rmr;                             // least significant bits of mantissa

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

reg carry;                              // carry bit, latched
reg ovfl, expsign;                      // overflow bits, latched

// Mux at A input of adder,
wire [47:0] a_mux = (state == STATE_IDLE) ?
                        a :             // A input
                        acc;            // accumulator

// Mux at B input of adder
wire [47:0] b_mux = (state == STATE_ADD_CARRY) ?
                        carry :         // carry bit
                        b;              // B input

// 49-bit adder
wire [48:0] sum = a_mux + b_mux;        // A (or accumulator) + B (or carry)

reg accsign2, inc1;
reg [41:0] rail;
reg [40:0] inc2;
reg [6:0] railexp;
reg [5:0] railtail;
reg rounded, sticky;

wire [8:0] expincr = (accsign2 != acc[40]) ? 9'o001 :
                     (acc[40] == acc[39])  ? 9'o777 :
                                             9'o000;

`define FULLRAIL    {railtail, rail}
`define FULLMANT    {accsign2, acc[40:0]}
`define FULLEXP     {expsign, ovfl, acc[47:41]}
`define ABS(x)      (x[40] ? ~x + 1 : x)

wire need_neg1 = (op == `ALU_FREVSUB) ||
                 (op == `ALU_FSUBABS && a[40]) ||
                 (op == `ALU_FSIGN && b[40]);
wire need_neg2 = (op == `ALU_FSUB) ||
                 (op == `ALU_FSUBABS && !b[40]);

wire [41:0] add_val1 = {a[40], a[40:0]} ^ {42{need_neg1}};
wire [41:0] add_val2 = {b[40], b[40:0]} ^ {42{need_neg2}};

wire signed [40:0] amant = a[40:0];
wire signed [40:0] bmant = b[40:0];

// ALU operation selection.
always @(posedge clk) begin
    if (op == `ALU_NOP) begin
        // No operation: reset state and done flag.
        done <= 0;
        state <= STATE_IDLE;
        ovfl <= 0;
        expsign <= 0;
        sticky <= 0;
        if (wy)
            rmr <= a;                       // update Y for UZA and U1A

    end else if (~done) begin
        case (state)
        STATE_IDLE:
            // Start new operation.
            case (op)
            `ALU_YTA: begin
                    // YTA: one cycle.
                    if (grp_log) begin
                        // Logical mode.
                        acc <= rmr;
                        done <= 1;
                    end else begin
                        `FULLMANT <= rmr[39:0];
                        `FULLEXP <= (a[47:41] + b[47:41] - 64);
                        rounded <= 1'b1;    // Suppress rounding
                        if (do_norm)
                            state <= STATE_NORM_AFTER;
                        else
                            done <= 1;
                    end
                end

            `ALU_AND: begin
                    // AAX: one cycle.
                    acc <= a & b;
                    rmr <= '0;
                    done <= 1;
                end

            `ALU_OR: begin
                    // AOX: one cycle.
                    acc <= a | b;
                    rmr <= '0;
                    done <= 1;
                end

            `ALU_XOR: begin
                    // AEX: one cycle.
                    acc <= a ^ b;
                    rmr <= a;
                    done <= 1;
                end

            `ALU_ADD_CARRY_AROUND: begin
                    // ARX: two cycles.
                    {carry, acc} <= sum;
                    rmr <= '0;
                    state <= STATE_ADD_CARRY;
                end

            `ALU_COUNT: begin
                    // ACX: three cycles.
                    acc <= countones(a);        // number of 1s in accumulator
                    rmr <= '0;
                    state <= STATE_ADD_B;
                end

            `ALU_CLZ: begin
                    // ANX: three cycles.
                    acc <= clz;                 // count leading zeroes + 1
                    rmr <= a << clz;
                    state <= STATE_ADD_B;
                end

            `ALU_SHIFT: begin
                    // ASN, ASX: 1 bit or 4 bits per cycle,
                    // depending on SLOW_SHIFT define.
                    railexp <= b[47:41];
                    {acc, rmr} <= {a, 48'b0};
                    state <= STATE_SHIFTING;
                end

            `ALU_PACK: begin
                    // APX: 1 + as many cycles as 1 bits in the mask.
                    acc <= '0;
                    `FULLRAIL <= a;
                    rmr <= b;
                    state <= STATE_PACKING;
                end

            `ALU_UNPACK: begin
                    // AUX: 49 cycles.
                    acc <= '0;
                    `FULLRAIL <= a;
                    rmr <= b;
                    railexp <= 48;
                    state <= STATE_UNPACKING;
                end

            `ALU_FADD, `ALU_FSUB, `ALU_FREVSUB, `ALU_FSUBABS: begin
                    // A+X, A-X, X-A or AMX instructions.
                    // acc gets the operand with the larger exponent, rail gets the other
                    if (a[47:41] > b[47:41]) begin
                        `FULLMANT <= add_val1;
                        inc1 <= need_neg1;
                        rail <= add_val2;
                        inc2 <= {need_neg2, 40'b0};
                        `FULLEXP <= a[47:41];
                        railexp <= b[47:41];
                    end else begin
                        `FULLMANT <= add_val2;
                        inc1 <= need_neg2;
                        rail <= add_val1;
                        inc2 <= {need_neg1, 40'b0};
                        `FULLEXP <= b[47:41];
                        railexp <= a[47:41];
                    end
                    rmr[39:0] <= 40'b0;
                    state <= STATE_NORM_BEFORE;
                    sticky <= 1'b0;
                end

            `ALU_FSIGN: begin
                    // AVX: change sign and normalize.
                    `FULLMANT <= add_val1;
                    inc1 <= need_neg1;
                    rail <= 42'b0;
                    inc2 <= 41'b0;
                    `FULLEXP <= a[47:41];
                    rmr <= 48'b0;
                    state <= STATE_ADDING;
                end

            `ALU_FADDEXP: begin
                    // E+N: add to exponent, then normalize.
                    `FULLMANT <= amant;
                    `FULLEXP <= (a[47:41] + b[47:41] - 64);
                    rmr <= 48'b0;
                    if (do_norm)
                        state <= STATE_NORM_AFTER;
                    else
                        done <= 1;
                end

            `ALU_FSUBEXP: begin
                    // E-N: subtract from exponent, then normalize.
                    `FULLMANT <= amant;
                    `FULLEXP <= (a[47:41] - b[47:41] + 64);
                    rmr <= 48'b0;
                    if (do_norm)
                        state <= STATE_NORM_AFTER;
                    else
                        done <= 1;
                end

            `ALU_FMUL: begin
                    // A*X: multiply in one cycle, then normalize.
                    {`FULLMANT, rmr[39:0]} <= amant * bmant;
                    `FULLEXP <= (a[47:41] + b[47:41] - 64);
                    rounded <= 1'b0;
                    if (do_norm)
                        state <= STATE_NORM_AFTER;
                    else if (do_round)
                        state <= STATE_ROUND;
                    else
                        done <= 1;
                end

            `ALU_FDIV: begin
                    // A/X: divide by subtraction.
                    // Dividing amant (rail) by bmant, result in acc, counter in inc2
                    inc2 <= 1'b1 << 39;
                    `FULLMANT <= '0;
                    if (`ABS(amant) >= `ABS(bmant)) begin
                        `FULLEXP <= a[47:41] - b[47:41] + 65;
                        rail <= amant;
                    end else begin
                        `FULLEXP <= a[47:41] - b[47:41] + 64;
                        rail <= amant << 1;
                    end
                    state <= STATE_DIVIDING;
                end
            endcase

        STATE_SHIFTING: begin
                // Next cycle of shift operation.
                if (railexp == 7'd64)
                    done <= 1;
                else begin
                    if (railexp[6]) begin
`ifdef SLOW_SHIFT
                        // Shift right by one bit.
                        {acc, rmr} <= {acc, rmr} >> 1;
                        railexp <= railexp - 1'b1;
`else
                        // Shift right by up to four bits.
                        case (railexp[1:0])
                         1: begin
                                {acc, rmr} <= {acc, rmr} >> 1;
                                railexp <= railexp - 1'd1;
                            end
                         2: begin
                                {acc, rmr} <= {acc, rmr} >> 2;
                                railexp <= railexp - 2'd2;
                            end
                         3: begin
                                {acc, rmr} <= {acc, rmr} >> 3;
                                railexp <= railexp - 2'd3;
                            end
                         0: begin
                                {acc, rmr} <= {acc, rmr} >> 4;
                                railexp <= railexp - 3'd4;
                            end
                        endcase
`endif
                    end else begin
`ifdef SLOW_SHIFT
                        // Shift left by one bit.
                        {rmr, acc} <= {rmr, acc} << 1;
                        railexp <= railexp + 1'b1;
`else
                        // Shift left by up to four bits.
                        case (railexp[1:0])
                         3: begin
                                {rmr, acc} <= {rmr, acc} << 1;
                                railexp <= railexp + 1'd1;
                            end
                         2: begin
                                {rmr, acc} <= {rmr, acc} << 2;
                                railexp <= railexp + 2'd2;
                            end
                         1: begin
                                {rmr, acc} <= {rmr, acc} << 3;
                                railexp <= railexp + 2'd3;
                            end
                         0: begin
                                {rmr, acc} <= {rmr, acc} << 4;
                                railexp <= railexp + 3'd4;
                            end
                        endcase
`endif
                    end
                end
            end

        STATE_DIVIDING: begin
                // Next cycle of divide operation.
                if (rail == '0 || inc2 == '0) begin
                    if (do_norm) begin
                        rounded <= 1'b1;        // Suppressing rounding
                        state <= STATE_NORM_AFTER;
                    end else
                        done <= 1;
                end else begin
                    // ABS(rail) < 1'b1 << 39
                    if (rail[41:39] == 3'b0 ||
                        (rail[41:39] == 3'b111 && rail[38:0] != 39'b0))
                        rail <= rail << 1;
                    else if (rail[41] ^ add_val2[41]) begin
                        `FULLMANT <= `FULLMANT - inc2;
                        rail <= (rail + add_val2) << 1;
                    end else begin
                        `FULLMANT <= `FULLMANT + inc2;
                        rail <= (rail - add_val2) << 1;
                    end
                end
                inc2 <= inc2 >> 1;
            end

        STATE_ADD_B: begin
                // Add B to the accumulator.
                // Second cycle of ACX or ANX.
                {carry, acc} <= sum;
                state <= STATE_ADD_CARRY;
            end

        STATE_ADD_CARRY: begin
                // Add carry bit.
                // Second cycle of ARX and third cycle of ACX or ANX.
                acc <= sum;
                done <= 1;
            end

        STATE_NORM_BEFORE: begin
                // Normalize to the right, before addition.
                if (acc[47:41] != railexp) begin
                    railexp <= railexp + 1;
                    rail <= $signed(rail) >>> 1;
                    rmr[39:0] <= {rail[0] ^ inc2[40], rmr[39:1]};
                    inc2[40] <= rail[0] & inc2[40];
                    sticky <= sticky | (rail[0] ^ inc2[40]);
                end else begin
                    state <= STATE_ADDING;
                end
            end

        STATE_ADDING: begin
                // Addition in one cycle.
                `FULLMANT <= `FULLMANT + rail + inc1 + inc2[40];
                rounded <= 1'b0;
                state <= STATE_NORM_AFTER;
            end

        STATE_NORM_AFTER: begin
                // Normalize after addition and other operations.
                if (expsign) begin
                    // Exponent underflow during normalization to the left; flush everything to 0.
                    acc[40:0] <= 41'b0;
                    rmr[39:0] <= 40'b0;
                    `FULLEXP <= 0;
                    done <= 1;
                end else if (accsign2 != acc[40]) begin
                    {`FULLMANT, rmr[39:0]} <= $signed({`FULLMANT, rmr[39:0]}) >>> 1;
                    sticky <= sticky | acc[0];
                    `FULLEXP <= `FULLEXP + expincr;
                    if (do_round)
                        state <= STATE_ROUND;
                    else
                        done <= 1;
                end else if (do_norm && acc[40] == acc[39]) begin
                    // A 1 bit is about to move from RMR to ACC, this makes additional rounding not needed.
                    rounded <= rounded | rmr[39];
                    {`FULLMANT, rmr[39:0]} <= {`FULLMANT, rmr[39:0]} << 1;
                    `FULLEXP <= `FULLEXP + expincr;
                end else begin
                    if (do_round)
                        state <= STATE_ROUND;
                    else
                        done <= 1;
                end
            end

        STATE_ROUND: begin
                // Rounding, usually after normalization.
                if ((rmr[39:0] != 40'b0 || sticky) && !rounded)
                    acc[0] <= 1'b1;
                done <= 1;
            end

        STATE_PACKING: begin
                // Next cycle of APX operation.
                if (rmr) begin
                    if (rmr[0])
                        acc <= {rail[0], acc[47:1]};
                    rmr <= rmr >> 1;
                    `FULLRAIL <= `FULLRAIL >> 1;
                end else
                    done <= 1;
            end

        STATE_UNPACKING: begin
                // Next cycle of AUX operation.
                if (railexp) begin
                    railexp <= railexp - 1'b1;
                    acc <= {acc, railtail[5] & rmr[47]};
                    rmr <= rmr << 1;
                    if (rmr[47])
                        `FULLRAIL <= `FULLRAIL << 1;
                end else
                    done <= 1;
            end
        endcase
    end
end

//
// Count ones in the input 48-bit word.
//
function [5:0] countones(input [47:0] a);
    countones = '0;
    for (int i = 0; i < 48; ++i)
        countones += a[i];
endfunction

endmodule
