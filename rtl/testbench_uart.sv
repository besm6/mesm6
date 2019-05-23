//
// MESM-6: MESM-6 UART testbench.
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

module testbench_uart();

// register addresses
localparam REG_CTRL    = 3'o7;
localparam REG_CTRLSET = 3'o6;
localparam REG_CTRLCLR = 3'o5;

localparam REG_DATA    = 3'o0;


reg clk, reset;
wire interrupt;


reg [14:0] addr;
reg read, write;
wire done;

wire [47:0] rdata;
reg  [47:0] wdata;

wire tx_pin;
wire rx_pin;

reg tx_generator_enabled;

mesm6_uart uart(
    clk, reset, interrupt,

    addr,
    read, write,
    rdata, wdata,
    done,

    rx_pin, tx_pin
);

initial begin
    $display("Reseting UART...");
    $monitor("    (%0t) rx_w_ptr = %d, CTRL = %o",
        $time, uart.rx_w_ptr, uart.UART_CTRL);
    $dumpfile("output_uart.vcd");
    $dumpvars();
    clk = 0;

    // Hardware reset.
    reset = 1;
    addr = 0;
    read = 0;
    write = 0;
    wdata = '0;
    #10 $display("Reset done.");
    reset = 0;

    // Write zero to UART Control reg.
    #10 $display("Reseting Control Reg...");
    addr  = REG_CTRL;
    wdata = '0;
    write = 1;
    wait(done);
    write = 0;
    if (uart.UART_CTRL != '0) begin
        $display("FATAL: CTRL is not reset.");
        $finish;
    end

    #10 $display("Enabling RX TX.");
    addr  = REG_CTRLSET;
    wdata = (1<<9);
    write = 1;
    wait(done);
    write = 0;
    if (uart.UART_CTRL[9] != 1'b1) begin
        $display("FATAL: RX TX is not set.");
        $finish;
    end

    #10 $display("Enabling TX generator.");
    tx_generator_enabled = 1;
    $display("Sending some garbage.");

    wait(uart.rx_frm_err_latched)

    $display("\n----- Test PASS -----");

        
    $finish;
end

initial begin
    #40000 $display("\n------- SIM TIMEOUT --------");
    $display("Test FAIL");
    $finish;
end

always begin
    #2 clk = ~clk;
end

// TX generator
//                                  STOP *   DATA   START  IDLE
reg [31:0] tx_data = 32'b11111111_11111110_01010101___0___1111111;
reg [3:0] tx_divider;
reg [4:0] tx_bit_count;

assign tx_pin = tx_data[0];

always @(posedge clk) begin
    if (reset) begin
        tx_divider <= '0;
        tx_bit_count <= '0;
        tx_generator_enabled <= '0;
    end else if (tx_generator_enabled) begin
        tx_divider <= tx_divider + 1'b1;
        if (tx_divider == '0) begin
            tx_bit_count <= tx_bit_count + 1'b1;
            tx_data <= {tx_data[0], tx_data[31:1]};
        end
    end
end

endmodule
