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

module dmemory(
    input  wire         clk,            // clock
    input  wire  [14:0] i_addr,         // address input
    input  wire         i_read,         // read op
    input  wire         i_write,        // write op
    input  wire  [47:0] i_data,         // data to memory
    output logic [47:0] o_data,         // data from memory
    output logic        o_done          // write op
);

// Global time parameters.
timeunit 1ns / 1ps;

logic [47:0] mem[32*1024];              // main RAM 32k words

always @(posedge clk) begin
    if (i_read) begin
        o_data <= mem[i_addr];          // memory load
        if (testbench.tracefd)
            $fdisplay(testbench.tracefd, "--- read %h -> %h", i_addr, mem[i_addr]);
    end

    if (i_write) begin
        mem[i_addr] <= i_data;          // memory store
        if (testbench.tracefd)
            $fdisplay(testbench.tracefd, "--- write %h := %h", i_addr, i_data);
    end

    o_done <= i_read | i_write;
end

endmodule
