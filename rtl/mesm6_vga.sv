//
// MESM-6: 4-plane (16 color) 320x240 VGA videoadapter.
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
`define REG_ADDRL    3'o7
`define REG_PLANE    3'o6
`define REG_DATA     3'o0

module mesm6_vga(
    input   wire        clk,
    input   wire        reset,
    output  wire        interrupt,

    // MESM-6 side
    input  wire [14:0]  i_addr,
    input  wire         i_rd,
    input  wire         i_wr,
    output reg  [47:0]  o_rdata,
    input  wire [47:0]  i_wdata,
    output reg          o_done,

    // VGA pins
    output wire         vga_hsync,
    output wire         vga_vsync,
    output wire [3:0]   vga_r,
    output wire [3:0]   vga_g,
    output wire [3:0]   vga_b
);


// MESM-6 BUS operation strobes
wire wr_stb = i_wr & ~o_done;
wire rd_stb = i_rd & ~o_done;

// VGA register selectors
wire sel_addr_latch = i_addr[2:0] == `REG_ADDRL;
wire sel_plane      = i_addr[2:0] == `REG_PLANE;
wire sel_data       = i_addr[2:0] == `REG_DATA;

// divide 50MHz board clk to 25MHz pixel clock
reg pix_clk_div;
always @(posedge clk) begin
    pix_clk_div <= reset ? 0 : pix_clk_div + 1;
end

wire pix_clk = pix_clk_div;

// MESM-6 video mode 320x240 is built from 640x480 with pixels and scanlines doubled
// video mode timings for 640x480 60Hz
localparam HS_STA = 16;              // horizontal sync start
localparam HS_END = 16 + 96;         // horizontal sync end
localparam HA_STA = 16 + 96 + 48;    // horizontal active pixel start
localparam VS_STA = 480 + 10;        // vertical sync start
localparam VS_END = 480 + 10 + 2;    // vertical sync end
localparam VA_END = 480;             // vertical active pixel end
localparam LINE   = 800;             // complete line (pixels)
localparam SCREEN = 525;             // complete screen (lines)

reg [9:0] h_count;  // line position
reg [9:0] v_count;  // screen position

always @(posedge clk) begin
    if (reset)
        h_count <= 0;
    else if (pix_clk)
        h_count <= (h_count == LINE) ? 0 : h_count + 1;
end

always @(posedge clk) begin
    if (reset)
        v_count <= 0;
    else if (pix_clk & h_count == LINE) begin
            v_count <= (v_count == SCREEN) ? 0 : v_count + 1;
    end 
end

wire o_active = ~((h_count < HA_STA) | (v_count > VA_END - 1)); 

wire [9:0] x = (h_count < HA_STA) ? 0 : (h_count - HA_STA);
wire [9:0] y = (v_count >= VA_END) ? (VA_END - 1) : (v_count);

reg [7:0]  plane_r[320*240/8];
reg [7:0]  plane_g[320*240/8];
reg [7:0]  plane_b[320*240/8];
reg [7:0]  plane_y[320*240/8];

reg [1:0]  plane_sel;
always @(posedge clk) begin
    if (wr_stb & sel_plane)
        plane_sel <= i_wdata;
end

reg [13:0] addr_latch;
always @(posedge clk) begin
    if (wr_stb) begin
        if (sel_addr_latch) addr_latch <= i_wdata;
        else if (sel_data)  addr_latch <= addr_latch + 1; 
    end else if (rd_stb & sel_data)
        addr_latch <= addr_latch + 1; 
end

always @(posedge clk) begin
    if (wr_stb & sel_data)
        case (plane_sel)
            0: plane_y[addr_latch] <= i_wdata;
            1: plane_r[addr_latch] <= i_wdata;
            2: plane_g[addr_latch] <= i_wdata;
            3: plane_b[addr_latch] <= i_wdata;
        endcase
end

always @(posedge clk) begin
    if (rd_stb & sel_data)
        case (plane_sel)
            0: o_rdata <= plane_y[addr_latch];
            1: o_rdata <= plane_r[addr_latch];
            2: o_rdata <= plane_g[addr_latch];
            3: o_rdata <= plane_b[addr_latch];
        endcase
end


reg [13:0] pix_cnt;
reg [13:0] pix_cnt_old;

// draw scanline twice
always @(posedge clk) begin
    if (~v_count[0] & h_count == 112 & pix_clk)
        pix_cnt_old <= pix_cnt;
end

always @(posedge clk) begin
    if (~|v_count & ~|h_count) begin
        pix_cnt <= 0;
    end else if (v_count[0] & h_count == 112 & pix_clk) begin
        pix_cnt <= pix_cnt_old;
    end else if (x[3:0] == 15 & pix_clk)
        pix_cnt <= pix_cnt + 1;
end

// pixel shift registers
reg [7:0] pix_r;
reg [7:0] pix_g;
reg [7:0] pix_b;
reg [7:0] pix_y;

always @(posedge clk) begin
    if (x[3:0] == 0) begin
        pix_r <= plane_r[pix_cnt];
        pix_g <= plane_g[pix_cnt];
        pix_b <= plane_b[pix_cnt];
        pix_y <= plane_y[pix_cnt];
    end else if (~x[0] & pix_clk) begin
        pix_r <= {pix_r[6:0], pix_r[7]};
        pix_g <= {pix_g[6:0], pix_g[7]};
        pix_b <= {pix_b[6:0], pix_b[7]};
        pix_y <= {pix_y[6:0], pix_y[7]};
    end
end

always @(posedge clk) begin
    o_done <= i_wr | i_rd;
end

// form VGA signals
assign vga_r = o_active ? {pix_y[7], pix_r[7], pix_r[7], pix_r[7]} : 0;
assign vga_g = o_active ? {pix_y[7], pix_g[7], pix_g[7], pix_g[7]} : 0;
assign vga_b = o_active ? {pix_y[7], pix_b[7], pix_b[7], pix_b[7]} : 0;

assign vga_hsync = ~((h_count >= HS_STA) & (h_count < HS_END));
assign vga_vsync = ~((v_count >= VS_STA) & (v_count < VS_END));

endmodule
