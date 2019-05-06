//
// MESM-6: Timer.
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

// register addresses
`define TIM_PRS    'o7
`define TIM_PRD    'o6
`define TIM_EN     'o5
`define TIM_IEN    'o4
`define TIM_CNT    'o0

module mesm6_timer(
    input   wire        clk,
    input   wire        reset,
    output  wire        interrupt,

    input  wire [14:0]  tim_addr,
    input  wire         tim_read,
    input  wire         tim_write,
    output wire [47:0]  tim_rdata,
    input  wire [47:0]  tim_wdata,
    output reg          tim_done
);

reg [2:0]  PRS;          // prescaler mode
reg [6:0]  PRS_CNT;      // prescaler counter

reg [31:0] PRD;          // timer period
reg        EN;           // timer eanble
reg        IEN;          // interrupt enable
reg [31:0] CNT;          // timer counter

reg delay;               // interrupt delay



// Readout logic
assign tim_rdata = (tim_addr [2:0] == `TIM_PRS) ? { 45'b0, PRS } :
                   (tim_addr [2:0] == `TIM_PRD) ? { 16'b0, PRD } :
                   (tim_addr [2:0] == `TIM_EN)  ? { 47'b0,  EN } :
                   (tim_addr [2:0] == `TIM_IEN) ? { 47'b0, IEN } :
                   /* TIM_CNT */                           CNT ;

always @(posedge clk) begin
    tim_done <= tim_read | tim_write;
end

// hold 2 cycles
assign interrupt =  delay | ((CNT == 0) & IEN);

always @(posedge clk) begin
    delay <= (CNT == 0) & IEN;
end

// Interrupt enable
always @(posedge clk) begin
    if (reset)
        IEN <= 0;
    else if (tim_write)
        if (tim_addr[2:0] == `TIM_IEN) IEN <= tim_wdata[0];
end

// Prescaler
always @(posedge clk) begin
    if (tim_write) begin
        if (tim_addr[2:0] == `TIM_PRS) PRS <= tim_wdata[2:0];
    end
end

always @(posedge clk) begin
    if (tim_write) begin
        if (tim_addr[2:0] == `TIM_PRS) PRS_CNT <= 0;
    end else
        PRS_CNT <= PRS_CNT + 1;
end

// Period register
always @(posedge clk) begin
    if (tim_write) begin
        if (tim_addr[2:0] == `TIM_PRD) PRD <= tim_wdata;
    end
end

// Enable count register
always @(posedge clk) begin
    if (tim_write) begin
        if (tim_addr[2:0] == `TIM_EN) begin
            EN  <= tim_wdata[0];
        end
    end else if (CNT == 0) begin
        EN <= 0;
    end
end

// Counting
wire tick = PRS == 0 ? 1 :
            PRS == 1 ? PRS_CNT[0]   == 0 :
            PRS == 2 ? PRS_CNT[1:0] == 0 :
            PRS == 3 ? PRS_CNT[2:0] == 0 :
            PRS == 4 ? PRS_CNT[3:0] == 0 :
            PRS == 5 ? PRS_CNT[4:0] == 0 :
            PRS == 6 ? PRS_CNT[5:0] == 0 :
                       PRS_CNT      == 0;

always @(posedge clk) begin
    if (tim_write & (tim_addr[2:0] == `TIM_EN))
        CNT <= PRD;
    else
        if (EN & tick) CNT <= CNT - 1;
end

endmodule
