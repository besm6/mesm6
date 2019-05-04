//
// Instruction memory: 32k of 48-bit words.
//
// Copyright (c) 2019 Serge Vakulenko
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
`default_nettype none
// Global time parameters.
`timescale 1ns / 1ps


module imemory(
    input  wire         clk,            // clock
    input  wire  [14:0] i_addr,         // address input
    input  wire         i_read,         // read op
    output wire  [47:0] o_data,         // data from memory
    output logic        o_done          // write op
);

reg delay;

mesm6_irom irom(.address(i_addr), .clock(clk), .rden(i_read), .q(o_data));

always @(posedge clk) begin
	delay <= i_read;
    o_done <= delay & i_read;
end

endmodule
