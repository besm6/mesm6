//
// MESM-6: Simple UART unit.
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
`define REG_CTRL    3'o7
`define REG_CTRLSET 3'o6
`define REG_CTRLCLR 3'o5

`define REG_DATA    3'o0

module mesm6_uart(
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

    // RX, TX pins
    output wire         tx_pin,
    input  wire         rx_pin
);

/*

UART control & status register

| field       | mode | offset |
|-------------+------+--------|
| divider     | RW   |    8:0 |
| rxtx_enable | RW   |      9 |
| tx_empty    | RO   |     10 |
| rx_empty    | RO   |     11 |
| tx_finish   | RO   |     12 |
| rx_frm_err  | RW   |     13 |
| tx_irq      | RW   |     14 |
| rx_irq      | RW   |     15 |
| tx_irq_mode | RW   |  17:16 |
| rx_irq_mode | RW   |  19:18 |
| parity_bit  | RW   |     20 |
| stop_bits   | RW   |     21 |
| data_bits   | RW   |  23:22 |
| loopback    | RW   |     24 |

baud_rate = F_CPU / (16 * divider)

rx_frm_err - sets to HIGH if bad stop bit received

*/

`define BITS_DIVIDER   8:0
`define BIT_RXTX_ENABLE 9
`define BIT_RX_FRM_ERR 13
`define BIT_LOOPBACK   24

reg     [47:0] UART_CTRL;

wire        rxtx_en            = UART_CTRL[`BIT_RXTX_ENABLE];
wire [12:0] rxtx_divider       = {UART_CTRL[`BITS_DIVIDER], 4'b1111};
wire        loopback           = UART_CTRL[`BIT_LOOPBACK];
wire        rx_frm_err_latched = UART_CTRL[`BIT_RX_FRM_ERR];

wire        rx_frm_err;

always @(posedge clk) begin
    if (reset)
        UART_CTRL <= '0;
    else if (i_wr)
        case (i_addr[2:0])
            `REG_CTRL:    UART_CTRL <= i_wdata;
            `REG_CTRLCLR: UART_CTRL <= UART_CTRL & ~i_wdata;
            `REG_CTRLSET: UART_CTRL <= UART_CTRL |  i_wdata;
        endcase
    else if (rx_frm_err)
        UART_CTRL[`BIT_RX_FRM_ERR] <= UART_CTRL[`BIT_RX_FRM_ERR] | rx_frm_err;
end

// transmit clock divisor
reg [12:0] tx_cnt;
reg        tx_active;

always @(posedge clk) begin
    tx_cnt <= (~rxtx_en | ~tx_active) ? rxtx_divider :
                      (tx_cnt == 0)   ? rxtx_divider :
                                        tx_cnt - 1'b1;
end

// pulse generator for tx shift reg
wire tx_pulse = ~tx_active ? 0 : tx_cnt == '0;

// bit counter
reg [3:0] tx_bit_count;
always @(posedge clk) begin
    tx_bit_count <= ~tx_active ? 9 :
                     tx_pulse  ? tx_bit_count - 1 :
                                 tx_bit_count;
end

// tx fifo buffer
reg [7:0]  tx_fifo[32];
reg [4:0]  tx_w_ptr;     // points to free cell
reg [4:0]  tx_r_ptr;

// tx fifo status
wire tx_empty =  tx_w_ptr         == tx_r_ptr;
wire tx_full  = (tx_w_ptr + 1'b1) == tx_r_ptr;

// tx_fifo is writing by CPU
wire tx_fifo_wr = i_wr & ~tx_full & ~o_done & (i_addr[2:0] == `REG_DATA);

// tx_w_ptr logic
always @(posedge clk) begin
    tx_w_ptr <= reset      ? 0 :
                tx_fifo_wr ? tx_w_ptr + 1'b1
                           : tx_w_ptr;
end

// tx shift register is ready to send
wire tx_fifo_rd = ~tx_empty & ~tx_active & rxtx_en;

// tx_r_ptr logic
always @(posedge clk) begin
    tx_r_ptr <= reset      ? 0 :
                tx_fifo_rd ? tx_r_ptr + 1'b1 :
                             tx_r_ptr;
end

// tx fifo is selected by CPU
wire tx_sel = i_addr[2:0] == `REG_DATA;

// put data to tx_fifo
always @(posedge clk) begin
    if (i_wr & tx_sel) begin
        tx_fifo[tx_w_ptr] <= i_wdata;
    end
end

// tx_active when fifo has pending data
always @(posedge clk) begin
    if (reset) begin
        tx_active <= 0;
    end else if (~tx_active & ~tx_empty & rxtx_en) begin
        tx_active <= 1;
    end else if (tx_active & ~|tx_bit_count & tx_pulse) begin
        tx_active <= 0;
    end
end

// Output shift register
// | stop | parity | data        | start |
// |------+--------+-------------+-------| ==> tx_pin
// |  1-2 |    0-1 | msb 5-9 lsb |     1 |
reg [11:0] tx_out;
always @(posedge clk) begin
    if (~tx_active) begin
        tx_out <= {2'b11, tx_fifo[tx_r_ptr], 1'b0};
    end else if (tx_active & tx_pulse) begin
        tx_out <= {2'b11, tx_out[10:1]};
    end
end

// when idle set tx_pin to HIGH, to shifting data otherwise
assign tx_pin    = ~tx_active ? 1'b1 : tx_out[0];

// when all data is send and no pending bytes in tx_fifo
wire   tx_finish = ~tx_active & tx_empty;


// -----------------------
// receive logic
// -----------------------
// rx fifo buffer
reg [7:0]  rx_fifo[32];
reg [4:0]  rx_w_ptr;
reg [4:0]  rx_r_ptr;

// rx fifo status
wire       rx_empty =  rx_w_ptr         == rx_r_ptr;
wire       rx_full  = (rx_w_ptr + 1'b1) == rx_r_ptr;


wire rx_in = loopback ? tx_pin : rx_pin;

reg  rx_active;
// rx clock
reg [12:0] rx_cnt;


// start bit edge detection
reg [3:0] rx_sync;

// using majority detection function
wire rx_start_bit   = (rx_sync[0] &  rx_sync[1] &  rx_sync[2]) |
                (rx_sync[0] &  rx_sync[1] &  rx_sync[3]) |
                (rx_sync[0] &  rx_sync[2] &  rx_sync[3]) |
                (rx_sync[1] &  rx_sync[2] &  rx_sync[3]) ;

always @(posedge clk) begin
    if (reset)
        rx_sync <= '0;
    else if (rxtx_en & ~|rx_cnt[1:0] == '0) rx_sync <= { rx_sync[2:0], ~rx_in & ~rx_active};
end

always @(posedge clk) begin
    rx_cnt <= ~rxtx_en                   ? rxtx_divider :
               ~rx_active & rx_start_bit ? rxtx_divider[12:1] : // shift rx_pulse to middle of rx_bit
               rx_cnt == 0               ? rxtx_divider :
                                           rx_cnt - 1'b1;
end

reg [3:0] rx_bit_count;
always @(posedge clk) begin
    rx_bit_count <= ~rx_active      ? 9 :
                     rx_cnt   == '0 ? rx_bit_count - 1 :
                                      rx_bit_count;
end

wire rx_pulse = ~rx_active ? 0 : rx_cnt == '0;
wire stop_bit_valid = rx_in & ~|rx_bit_count & rx_pulse;

// after start bit received start receiving process
always @(posedge clk) begin
    if (reset)
        rx_active <= 0;
    else if (rxtx_en & ~rx_active & rx_start_bit) begin
        rx_active <= 1;
    end else if (rx_active & ~|rx_bit_count & rx_pulse) begin
        rx_active <= 0;
    end
end

// shifts on rx_pulse 
reg [7:0] rx_data;
always @(posedge clk) begin
    if (rx_pulse)
        rx_data <= {rx_in, rx_data[7:1]};
end

// rx_r_ptr logic
wire rx_fifo_rd = ~rx_empty & i_rd & ~o_done & (i_addr[2:0] == `REG_DATA);

always @(posedge clk) begin
    rx_r_ptr <= reset      ? 0 :
                rx_fifo_rd ? rx_r_ptr + 1'b1 :
                             rx_r_ptr;
end

// rx_w_ptr logic
wire rx_fifo_wr     = ~rx_full & rxtx_en & stop_bit_valid;

always @(posedge clk) begin
    rx_w_ptr <= reset      ? 0 :
                rx_fifo_wr ? rx_w_ptr + 1'b1
                           : rx_w_ptr;
end

// rx framing error detection
assign rx_frm_err = rx_active & ~|rx_bit_count & rx_pulse & ~rx_in;

always @(posedge clk) begin
    if (rx_fifo_wr)
        rx_fifo[rx_w_ptr] <= rx_data;
end

always @(posedge clk) begin
    if (i_rd)
        case (i_addr[2:0])
            `REG_CTRL:
                o_rdata <= {  loopback, // BIT_24
                              /*23 22 21 20 19 18 17 16 15 14*/
                              10'b0, // reserved bits
                              UART_CTRL[`BIT_RX_FRM_ERR], // bit 13
                              tx_finish, // bit 12
                              rx_empty,  // bit 11
                              tx_empty,  // bit 10
                              rxtx_en,   // bit 9
                              rxtx_divider[12:4] };
            `REG_DATA: o_rdata <= {39'b0, rx_empty,  rx_fifo[rx_r_ptr]};
        endcase
end

// debug
wire [7:0] rx_fifo_0 = rx_fifo[0];
wire [7:0] rx_fifo_1 = rx_fifo[1];

always @(posedge clk) begin
    o_done <= i_wr | i_rd;
end

endmodule
