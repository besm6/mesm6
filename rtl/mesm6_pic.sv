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
`define PIC_OFF    'o0

module mesm6_pic(
    input   wire        clk,
    input   wire        reset,
    output  wire        interrupt,     // interrupt request to CPU

    input  wire [1:0]   pic_irq,       // sync'ed interrupt req's from devices

    input  wire [14:0]  pic_addr,      // register address
    input  wire         pic_read,      // request data read
    input  wire         pic_write,     // request data write
    output wire [47:0]  pic_rdata,
    input  wire [47:0]  pic_wdata,
    output reg          pic_done      // pic operation completed
);

reg  [47:0] IFS;  // main interrupt register
reg  [47:0] IEC;  // interrupt register mask

assign interrupt = (IFS & IEC) != '0;

wire [5:0]  OFF =
    IFS[47] ? 1  : IFS[46] ? 2  : IFS[45] ? 3  : IFS[44] ? 4 :
    IFS[43] ? 5  : IFS[42] ? 6  : IFS[41] ? 7  : IFS[40] ? 8 :
    IFS[39] ? 9  : IFS[38] ? 10 : IFS[37] ? 11 : IFS[36] ? 12 :
    IFS[35] ? 13 : IFS[34] ? 14 : IFS[33] ? 15 : IFS[32] ? 16 :
    IFS[31] ? 17 : IFS[30] ? 18 : IFS[29] ? 19 : IFS[28] ? 20 :
    IFS[27] ? 21 : IFS[26] ? 22 : IFS[25] ? 23 : IFS[24] ? 24 :
    IFS[23] ? 25 : IFS[22] ? 26 : IFS[21] ? 27 : IFS[20] ? 28 :
    IFS[19] ? 29 : IFS[18] ? 30 : IFS[17] ? 31 : IFS[16] ? 32 :
    IFS[15] ? 33 : IFS[14] ? 34 : IFS[13] ? 35 : IFS[12] ? 36 :
    IFS[11] ? 37 : IFS[10] ? 38 : IFS[9]  ? 39 : IFS[8]  ? 40 :
    IFS[7]  ? 41 : IFS[6]  ? 42 : IFS[5]  ? 43 : IFS[4]  ? 44 :
    IFS[3]  ? 45 : IFS[2]  ? 46 : IFS[1]  ? 47 : IFS[0]  ? 48 : 0;

assign pic_rdata = pic_addr [2:0] == `PIC_OFF ? {42'b0, OFF} :
                   pic_addr [2:0] == `PIC_IEC ? IEC :
                   /* PIC_IFS */                IFS;

always @(posedge clk) begin
    if (reset) begin
        IFS <= 0;
        IEC <= 0;
    end else if (pic_write) begin
        case (pic_addr [2:0]) 
            `PIC_IFS   : IFS <= pic_wdata        | {46'b0, pic_irq};
            `PIC_IFSSET: IFS <= IFS | pic_wdata  | {46'b0, pic_irq};
            `PIC_IFSCLR: IFS <= IFS & ~pic_wdata | {46'b0, pic_irq};
            `PIC_IEC:    IEC <= pic_wdata;
            `PIC_IECSET: IEC <= IEC | pic_wdata;
            `PIC_IECCLR: IEC <= IEC & ~pic_wdata;
            default:     IFS <= IFS | {46'b0, pic_irq};
        endcase
    end else begin
        IFS <= IFS | {46'b0, pic_irq};
    end

    pic_done <= pic_read | pic_write;
end

endmodule