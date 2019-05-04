//
// MESM-6: GPIO stub.
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
`define REG_TRIS   'o7
`define REG_PORT   'o6
`define REG_VAL	 'o5
`define REG_CNIE   'o4
`define REG_CNEN   'o3


// -----
// GPIO stub
// -----
module mesm6_gpio(
    input   wire        clk,
    input   wire        reset,
    output  wire        interrupt,      // interrupt request to CPU

    input  wire [14:0]  gpio_addr,      // register address
    input  wire         gpio_read,      // request data read
    input  wire         gpio_write,     // request data write
    output wire [47:0]  gpio_rdata,
    input  wire [47:0]  gpio_wdata,
    output reg          gpio_done,       // operation completed
	 
	 input  wire [47:0]  gpio_inputs,    // inputs from board
    output wire [47:0]  gpio_outputs    // output to board
);

reg  [47:0] TRIS;
reg  [47:0] PORT; // sets outputs
reg  [47:0] VAL;  // data from input pins
reg         CNIE;
reg  [47:0] CNEN;

assign interrupt = 0; // CNIE & ((old_data ^ PORT & CNEN) != '0);

assign gpio_rdata = gpio_addr [2:0] == `REG_TRIS ? TRIS :
						  gpio_addr [2:0] == `REG_VAL  ? VAL :
                    gpio_addr [2:0] == `REG_CNEN ? CNEN :
                    gpio_addr [2:0] == `REG_CNIE ? {47'b0, CNIE} :
                   /* REG_PORT */                  PORT;

// read/write logic
always @(posedge clk) begin
	gpio_done <= gpio_read | gpio_write;
end

always @(posedge clk) begin
	VAL <= gpio_inputs;
end

// PORT reg
assign gpio_outputs = PORT;

always @(posedge clk) begin
    if (reset) begin
        PORT     <= '0;
    end else if (gpio_write) begin
        case (gpio_addr [2:0]) 
            `REG_PORT  : PORT <= gpio_wdata;
        endcase
    end
end

endmodule
