//
// MESM-6: Interrupt controller.
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
`define PIC_IFS    'o7
`define PIC_IFSSET 'o6
`define PIC_IFSCLR 'o5
`define PIC_IEC    'o4
`define PIC_IECSET 'o3
`define PIC_IECCLR 'o2
`define PIC_ISR    'o0

module mesm6_pic(
    input   wire        clk,
    input   wire        reset,
    output  wire        interrupt,  // interrupt request to CPU

    input  wire [47:0]  pic_irq,    // sync'ed interrupt req's from devices

    input  wire [14:0]  pic_addr,   // register address
    input  wire         pic_read,   // request data read
    input  wire         pic_write,  // request data write
    output wire [47:0]  pic_rdata,
    input  wire [47:0]  pic_wdata,
    output reg          pic_done    // pic operation completed
);

reg  [47:0] IFS;                    // incoming interrupt requests, latched
reg  [47:0] IEC;                    // mask of enabled interrupts

wire [47:0] active = IFS & IEC;     // mask of active interrupts

assign interrupt = |active;

wire [5:0]  ISR =
    active[47] ? 1  : active[46] ? 2  : active[45] ? 3  : active[44] ? 4 :
    active[43] ? 5  : active[42] ? 6  : active[41] ? 7  : active[40] ? 8 :
    active[39] ? 9  : active[38] ? 10 : active[37] ? 11 : active[36] ? 12 :
    active[35] ? 13 : active[34] ? 14 : active[33] ? 15 : active[32] ? 16 :
    active[31] ? 17 : active[30] ? 18 : active[29] ? 19 : active[28] ? 20 :
    active[27] ? 21 : active[26] ? 22 : active[25] ? 23 : active[24] ? 24 :
    active[23] ? 25 : active[22] ? 26 : active[21] ? 27 : active[20] ? 28 :
    active[19] ? 29 : active[18] ? 30 : active[17] ? 31 : active[16] ? 32 :
    active[15] ? 33 : active[14] ? 34 : active[13] ? 35 : active[12] ? 36 :
    active[11] ? 37 : active[10] ? 38 : active[9]  ? 39 : active[8]  ? 40 :
    active[7]  ? 41 : active[6]  ? 42 : active[5]  ? 43 : active[4]  ? 44 :
    active[3]  ? 45 : active[2]  ? 46 : active[1]  ? 47 : active[0]  ? 48 : 0;


// Readout logic
assign pic_rdata = pic_addr [2:0] == `PIC_ISR ? {42'b0, ISR} :
                   pic_addr [2:0] == `PIC_IEC ? IEC :
                   /* PIC_IFS */                IFS;

always @(posedge clk) begin
    pic_done <= pic_read | pic_write;
end

// Interrupt status register
always @(posedge clk) begin
    if (pic_write) begin
        case (pic_addr [2:0])
            `PIC_IFS   : IFS <= pic_wdata        | pic_irq;
            `PIC_IFSSET: IFS <= IFS | pic_wdata  | pic_irq;
            `PIC_IFSCLR: IFS <= IFS & ~pic_wdata | pic_irq;
            default:     IFS <= IFS              | pic_irq;
        endcase
    end else begin
        IFS <= IFS | pic_irq;
    end
end

// Interrupt enable register
always @(posedge clk) begin
    if (reset) begin
        IEC <= '0;
    end else if (pic_write) begin
        case (pic_addr [2:0])
            `PIC_IEC:    IEC <= pic_wdata;
            `PIC_IECSET: IEC <= IEC | pic_wdata;
            `PIC_IECCLR: IEC <= IEC & ~pic_wdata;
        endcase
    end
end

endmodule
