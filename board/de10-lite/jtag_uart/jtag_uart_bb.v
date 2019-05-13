
module jtag_uart (
	clk_clk,
	irq_irq,
	reset_reset_n,
	uart_chipselect,
	uart_address,
	uart_read_n,
	uart_readdata,
	uart_write_n,
	uart_writedata,
	uart_waitrequest);	

	input		clk_clk;
	output		irq_irq;
	input		reset_reset_n;
	input		uart_chipselect;
	input		uart_address;
	input		uart_read_n;
	output	[31:0]	uart_readdata;
	input		uart_write_n;
	input	[31:0]	uart_writedata;
	output		uart_waitrequest;
endmodule
