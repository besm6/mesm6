//
// Data memory: 32k of 48-bit words.
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


module dmemory(
    input  wire         clk,            // clock
    input  wire  [14:0] i_addr,         // address input
    input  wire         i_read,         // read op
    input  wire         i_write,        // write op
    input  wire  [47:0] i_data,         // data to memory
    output wire [47:0] o_data,         // data from memory
    output logic        o_done          // write op
);


wire [47:0] q;
reg delay;

mesm6_dram dram(
	.clock(clk),
	.data(i_data),
	.rdaddress(i_addr[11:0]),
	.rden(i_read),
	.wraddress(i_addr[11:0]),
	.wren(i_write),
	.q(q));


assign o_data = (i_addr == 0) ? '0 : q;
	
always @(posedge clk) begin
	//if (i_read) o_data <= (i_addr == 0) ? '0 : q;
	
	delay  <= i_read | i_write;
	o_done <= delay & (i_read | i_write);
end

endmodule
