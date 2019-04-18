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
`define REG_CNIE   'o5
`define REG_CNEN   'o4


// -----
// read-only GPIO stub
// -----

module mesm6_gpio(
    input   wire        clk,
    input   wire        reset,
    output  wire        interrupt,      // interrupt request to CPU

    input  wire [47:0]  gpio_inputs,    // inputs from board

    input  wire [14:0]  gpio_addr,      // register address
    input  wire         gpio_read,      // request data read
    input  wire         gpio_write,     // request data write
    output wire [47:0]  gpio_rdata,
    input  wire [47:0]  gpio_wdata,
    output reg          gpio_done       // operation completed
);

reg  [47:0] TRIS;
reg  [47:0] PORT;
reg         CNIE;
reg  [47:0] CNEN;

reg  [47:0] old_data;

assign interrupt = CNIE & ((old_data ^ PORT & CNEN) != '0);

assign gpio_rdata = gpio_addr [2:0] == `REG_TRIS ? TRIS :
                    gpio_addr [2:0] == `REG_CNEN ? CNEN :
                    gpio_addr [2:0] == `REG_CNIE ? {47'b0, CNIE} :
                   /* REG_PORT */                  PORT;

always @(posedge clk) begin
    if (reset) begin
        TRIS     <= '0;
        PORT     <= '0;
        CNIE     <=  0;
        CNEN     <= '0;
        old_data <= '0;
    end else if (gpio_write) begin
        case (gpio_addr [2:0]) 
            `REG_TRIS  : TRIS <= gpio_wdata;
            //`REG_PORT  : PORT <= gpio_wdata;
            `REG_CNIE  : CNIE <= gpio_wdata != '0;
            `REG_CNEN  : CNEN <= gpio_wdata;
        endcase
    end

    old_data <= PORT;
    PORT     <= gpio_inputs;

    gpio_done <= gpio_read | gpio_write;
end

endmodule