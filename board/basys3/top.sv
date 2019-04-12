//
// Demo of LED control using slide switches and push buttons.
//
//
`timescale 1ns / 1ps

module top (
    input             clk,      // 100MHz clock input

    input             btnC,     // push button inputs
    input             btnU,
    input             btnD,
    input             btnR,
    input             btnL,

    input      [15:0] sw,       // slide switch inputs

    output reg [15:0] led,      // LED outputs

    output reg  [6:0] seg,      // 7-segment display outputs, active low
    output reg        dp,       // decimal point, active low
    output reg  [3:0] an        // common anodes, active low
);
    reg  [3:0]  cnt;
    reg  [47:0] acc;
    reg  [47:0] mask;
    wire [47:0] result = pack(acc, mask);

    initial seg = 7'b1111111;   // clear 7-segment display

    initial an = 4'b0000;       // all digits enabled
    initial cnt = 4'b0000;

    initial dp = 0;             // enable decimal point

    always @(clk) begin         // slide switches enable LEDs
        if (btnU)
            acc[15:0] <= sw;
        else if (btnD)
            acc[31:16] <= sw;
        else
            acc[47:32] <= sw;

        if (btnR)
            mask[15:0] <= sw;
        else if (btnL)
            mask[31:16] <= sw;
        else
            mask[47:32] <= sw;

        if (cnt[3])
            led <= result[15:0];
        else if (cnt[2])
            led <= result[31:16];
        else if (cnt[1])
            led <= result[47:32];

        cnt <= cnt + 1;
    end

    function [47:0] pack(input [47:0] val, mask);
//`ifdef notdef
        reg [47:0] result;
        int i;

        begin
            result = 48'b0;
            for (i=16; i<48; i++) begin
                if (mask[i])
                    result = { val[i], result[47:1] };
            end
            pack = result;
        end
//`endif
//        pack = 48'o5252_5252_5252_5252;
    endfunction

endmodule
