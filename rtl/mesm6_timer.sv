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
`define TIM_MODE   'o3
`define TIM_PWM    'o2
`define TIM_CNT    'o0

module mesm6_timer(
    input   wire        clk,
    input   wire        reset,
    output  wire        interrupt,

    input  wire [14:0]  i_addr,
    input  wire         i_rd,
    input  wire         i_wr,
    output wire [47:0]  o_rdata,
    input  wire [47:0]  i_wdata,
    output reg          o_done,

    output wire         o_pwm
);

reg [2:0]  PRS;          // prescaler mode
reg [6:0]  PRS_CNT;      // prescaler counter
reg [31:0] PRD;          // timer period
reg        EN;           // timer eanble
reg        IEN;          // interrupt enable
reg [7:0]  PWM;          // PWM comparator value
reg [31:0] CNT;          // timer counter

// status & mode register
// bit 0:  =0 - single period mode
//         =1 - continuous counting
// bit 1:  =1 - period ended latch 
reg [1:0] MODE;           
wire md_periodic = MODE[0];

reg delay;               // interrupt delay

// selectors
wire sel_prs  = i_addr [2:0] == `TIM_PRS;
wire sel_prd  = i_addr [2:0] == `TIM_PRD;
wire sel_en   = i_addr [2:0] == `TIM_EN;
wire sel_ien  = i_addr [2:0] == `TIM_IEN;
wire sel_mode = i_addr [2:0] == `TIM_MODE;
wire sel_pwm  = i_addr [2:0] == `TIM_PWM;


wire prs_stb = PRS == 0 ? 1 :
               PRS == 1 ? PRS_CNT[0]   == 0 :
               PRS == 2 ? PRS_CNT[1:0] == 0 :
               PRS == 3 ? PRS_CNT[2:0] == 0 :
               PRS == 4 ? PRS_CNT[3:0] == 0 :
               PRS == 5 ? PRS_CNT[4:0] == 0 :
               PRS == 6 ? PRS_CNT[5:0] == 0 :
                          PRS_CNT      == 0;


wire prd_stb = (CNT == PRD) & prs_stb;

// hold IRQ signal for 2 cycles
assign interrupt =  delay | (prd_stb & IEN);

always @(posedge clk) begin
    delay <= prd_stb & IEN;
end

// Interrupt enable
always @(posedge clk) begin
    if (reset)
        IEN <= 0;
    else if (i_wr & sel_ien)
        IEN <= i_wdata[0];
end

// Prescaler
always @(posedge clk) begin
    if (i_wr & sel_prs) begin
        PRS <= i_wdata[2:0];
    end
end

always @(posedge clk) begin
    if (i_wr & sel_prs) begin
        PRS_CNT <= 0;
    end else
        PRS_CNT <= PRS_CNT + 1;
end

// Period register
always @(posedge clk) begin
    if (i_wr & sel_prd) begin
        PRD <= i_wdata;
    end
end

// Enable count register
always @(posedge clk) begin
    if (i_wr & sel_en) begin
        EN  <= i_wdata[0];
    end else if (prd_stb) begin
        EN <= md_periodic ? 1 : 0;
    end
end

// Counting
always @(posedge clk) begin
    if (i_wr & (sel_en | sel_prd))
        CNT <= 0;
    else if (EN & prs_stb)
        CNT <= prd_stb ? 0 : CNT + 1;
end

always @(posedge clk) begin
    if (i_wr & sel_mode)
        MODE <= i_wdata[1:0];
    else if (prd_stb)
        MODE <= MODE | 2'b10;
end


always @(posedge clk) begin
    if (i_wr & sel_pwm)
        PWM <= i_wdata;
end

reg [6:0] pwm_cnt;
always @(posedge clk) begin
    if (i_wr & sel_en)
        pwm_cnt <= 0;
    else if (prd_stb)
        pwm_cnt <= pwm_cnt + 1;
end

assign o_pwm = !EN            ? 0 :
               pwm_cnt <  PWM ? 1 : 0;


// Readout logic
assign o_rdata   = sel_prs ? PRS  :
                   sel_prd ? PRD  :
                   sel_en  ? EN   :
                   sel_ien ? IEN  :
                   sel_mode? MODE :
                   sel_pwm ? PWM  :
               /* TIM_CNT */ CNT  ;

always @(posedge clk) begin
    o_done <= i_rd | i_wr;
end

endmodule
