	jtag_uart u0 (
		.clk_clk          (<connected-to-clk_clk>),          //   clk.clk
		.irq_irq          (<connected-to-irq_irq>),          //   irq.irq
		.reset_reset_n    (<connected-to-reset_reset_n>),    // reset.reset_n
		.uart_chipselect  (<connected-to-uart_chipselect>),  //  uart.chipselect
		.uart_address     (<connected-to-uart_address>),     //      .address
		.uart_read_n      (<connected-to-uart_read_n>),      //      .read_n
		.uart_readdata    (<connected-to-uart_readdata>),    //      .readdata
		.uart_write_n     (<connected-to-uart_write_n>),     //      .write_n
		.uart_writedata   (<connected-to-uart_writedata>),   //      .writedata
		.uart_waitrequest (<connected-to-uart_waitrequest>)  //      .waitrequest
	);

