//
// MESM-6: MESM bridge to AVALON JTAG UART.
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

// register addresses
`define REG_CTRL   'o4
`define REG_DATA   'o0

module mesm6_uart(
    input   wire        clk,
    input   wire        reset,

    // MESM-6 side
    input  wire [14:0]  mesm_addr,
    input  wire         mesm_read,
    input  wire         mesm_write,
    output wire [47:0]  mesm_rdata,
    input  wire [47:0]  mesm_wdata,
    output reg          mesm_done,

    // Avalon side
    output  wire        uart_chipselect,  //  uart.chipselect
    output  wire        uart_address,     //      .address
    output  wire        uart_read_n,      //      .read_n
    input   wire [31:0] uart_readdata,    //      .readdata
    output  wire        uart_write_n,     //      .write_n
    output  wire [31:0] uart_writedata,   //      .writedata
    input   wire        uart_waitrequest  //      .waitrequest;
);

assign mesm_rdata = uart_readdata;

assign uart_chipselect = mesm_read | mesm_write;

assign uart_read_n  = ~mesm_read;
assign uart_write_n = ~mesm_write;

assign uart_address = mesm_addr[3];

assign uart_writedata = mesm_wdata;

always @(posedge clk) begin
    mesm_done <= mesm_read | mesm_write;
end

endmodule
