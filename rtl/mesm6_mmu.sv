//
// MESM-6: Memory-mapper & bus controller unit.
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

`define MMU_ADDR_PIC  12'o7777
`define MMU_ADDR_TIM  12'o7776
`define MMU_ADDR_GPIO 12'o7775

// all signals are polled on clk rising edge
// all signals positive

module mesm6_mmu(
    // Signals from CPU.
    input  wire [14:0]  cpu_addr,      // memory address
    input  wire         cpu_read,      // request data read
    input  wire         cpu_write,     // request data write
    input  wire [47:0]  cpu_wdata,     // write data bus
    output wire [47:0]  cpu_rdata,     // data word read
    output wire         cpu_done,      // operation complete

    // Signals to DATA RAM.
    output wire [14:0]  mem_addr,      // memory address
    output wire         mem_read,      // request data read
    output wire         mem_write,     // request data write
    input  wire [47:0]  mem_rdata,     // data word read
    output wire [47:0]  mem_wdata,     // data word written
    input  wire         mem_done,      // memory operation completed

    // Signals to PIC.
    output wire [14:0]  pic_addr,      // register address
    output wire         pic_read,      // request data read
    output wire         pic_write,     // request data write
    input  wire [47:0]  pic_rdata,     // data word written
    output wire [47:0]  pic_wdata,     // data word read
    input  wire         pic_done,      // pic operation completed
    output wire [1:0]   pic_irq,       // interrupt req's from devices

    // Signals to GPIO
    output wire [14:0]  gpio_addr,      // register address
    output wire         gpio_read,      // request data read
    output wire         gpio_write,     // request data write
    input  wire [47:0]  gpio_rdata,     // data word written
    output wire [47:0]  gpio_wdata,     // data word read
    input  wire         gpio_done,      // operation completed
    input  wire         gpio_int       // request CPU interrupt

    // Signals to Timer 0, 1
    /*output wire [14:0]  tim_addr,      // register address
    output wire         tim_read,      // request data read
    output wire         tim_write,     // request data write
    input  wire [47:0]  tim_rdata,     // data word written
    output wire [47:0]  tim_wdata,     // data word read
    input  wire         tim_done,      // operation completed
    input  wire         tim_int*/        // request CPU interrupt
);

// Connect CPU address bus to all devices
assign mem_addr  = cpu_addr;
assign pic_addr  = cpu_addr;
assign gpio_addr = cpu_addr;
//assign tim_addr = cpu_addr;

// Connect CPU write bus to all devices
assign mem_wdata  = cpu_wdata;
assign pic_wdata  = cpu_wdata;
assign gpio_wdata = cpu_wdata;
//assign tim_wdata  = cpu_addr;

// Connect PIC interrupt signals
assign pic_irq[0] = gpio_int;
assign pic_irq[1] = 0; //tim_int;


// Chip select mux
assign pic_read  = cpu_addr[14:3] == `MMU_ADDR_PIC ? cpu_read  : 0;
assign pic_write = cpu_addr[14:3] == `MMU_ADDR_PIC ? cpu_write : 0;

assign mem_read  = cpu_addr[14:9] != 6'o77 ? cpu_read  : 0;
assign mem_write = cpu_addr[14:9] != 6'o77 ? cpu_write : 0;

assign gpio_read  = cpu_addr[14:3] == `MMU_ADDR_GPIO ? cpu_read  : 0;
assign gpio_write = cpu_addr[14:3] == `MMU_ADDR_GPIO ? cpu_write : 0;

assign cpu_rdata  = cpu_addr[14:3] == `MMU_ADDR_PIC  ? pic_rdata  :
                    cpu_addr[14:3] == `MMU_ADDR_GPIO ? gpio_rdata :
                                                      mem_rdata;

assign cpu_done  = cpu_addr[14:3] == `MMU_ADDR_PIC  ? pic_done  :
                   cpu_addr[14:3] == `MMU_ADDR_GPIO ? gpio_done :
                                              mem_done;

/*always @* begin
    case (cpu_addr[14:3])
        `MMU_ADDR_PIC: begin
            pic_read  = cpu_read;
            pic_write = cpu_write;
            cpu_rdata = pic_rdata;
            cpu_done  = pic_done;
        end
       `MMU_ADDR_TIM: begin
            tim_read  = cpu_read;
            tim_write = cpu_write;
            cpu_rdata = tim_rdata;
            cpu_done  = tim_done;
        end
        `MMU_ADDR_GPIO: begin
            gpio_read  = cpu_read;
            gpio_write = cpu_write;
            cpu_rdata  = gpio_rdata;
            cpu_done   = gpio_done;
        end
        default: begin  // Memory
            mem_read  = cpu_read;
            mem_write = cpu_write;
            cpu_rdata = mem_rdata;
            cpu_done  = mem_done;
        end
    endcase
end*/

endmodule