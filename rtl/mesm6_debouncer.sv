//
// MESM-6: Switch debouncer
//
// Copyright (c) 2019 Evgeniy Khaluev
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
`default_nettype none
`include "mesm6_defines.sv"

module mesm6_debouncer(
    input   wire        clk,
    
    input   wire [9:0]  data_in,
    output  reg  [9:0]  data_out
);

reg [31:0] cnt;

reg [9:0] delay0;
reg [9:0] delay1;
reg [9:0] delay2;
reg [9:0] delay3;

always @(posedge clk) begin
    if (cnt != 65000) begin
        cnt <= cnt + 1;
    end else begin
        cnt <= 0;
        delay0 <= data_in;
        delay1 <= delay0;
        delay2 <= delay1;
        delay3 <= delay2;
    end

    if (delay0 ^ delay1 ^ delay2 ^ delay3 == 0) data_out <= delay2;
end

endmodule
