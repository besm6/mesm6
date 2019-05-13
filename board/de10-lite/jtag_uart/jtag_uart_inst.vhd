	component jtag_uart is
		port (
			clk_clk          : in  std_logic                     := 'X';             -- clk
			irq_irq          : out std_logic;                                        -- irq
			reset_reset_n    : in  std_logic                     := 'X';             -- reset_n
			uart_chipselect  : in  std_logic                     := 'X';             -- chipselect
			uart_address     : in  std_logic                     := 'X';             -- address
			uart_read_n      : in  std_logic                     := 'X';             -- read_n
			uart_readdata    : out std_logic_vector(31 downto 0);                    -- readdata
			uart_write_n     : in  std_logic                     := 'X';             -- write_n
			uart_writedata   : in  std_logic_vector(31 downto 0) := (others => 'X'); -- writedata
			uart_waitrequest : out std_logic                                         -- waitrequest
		);
	end component jtag_uart;

	u0 : component jtag_uart
		port map (
			clk_clk          => CONNECTED_TO_clk_clk,          --   clk.clk
			irq_irq          => CONNECTED_TO_irq_irq,          --   irq.irq
			reset_reset_n    => CONNECTED_TO_reset_reset_n,    -- reset.reset_n
			uart_chipselect  => CONNECTED_TO_uart_chipselect,  --  uart.chipselect
			uart_address     => CONNECTED_TO_uart_address,     --      .address
			uart_read_n      => CONNECTED_TO_uart_read_n,      --      .read_n
			uart_readdata    => CONNECTED_TO_uart_readdata,    --      .readdata
			uart_write_n     => CONNECTED_TO_uart_write_n,     --      .write_n
			uart_writedata   => CONNECTED_TO_uart_writedata,   --      .writedata
			uart_waitrequest => CONNECTED_TO_uart_waitrequest  --      .waitrequest
		);

