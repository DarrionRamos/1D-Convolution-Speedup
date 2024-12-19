-- Darrion Ramos
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.user_pkg.all;
use work.config_pkg.all;
use work.math_custom.all;

entity dram_rd_custom is
  port(	-- user dma control signals
		dram_clk             : in  std_logic;
		user_clk             : in  std_logic;
		dram_rst             : in  std_logic;
		user_rst             : in  std_logic;
		go                   : in  std_logic;
		rd_en	             : in  std_logic;
		stall		         : in  std_logic;
		start_addr           : in  std_logic_vector(RAM0_ADDR_RANGE);
		size		         : in  std_logic_vector(RAM0_RD_SIZE_RANGE);
		valid 		         : out std_logic;
		data		         : out std_logic_vector(RAM0_RD_DATA_RANGE);
		done		         : out std_logic;
		user_interim_rst     : out std_logic;
		
		-- debugging signals
		debug_count          : out std_logic_vector(16 downto 0);
		debug_dma_size       : out std_logic_vector(15 downto 0);
		debug_dma_start_addr : out std_logic_vector(RAM0_ADDR_RANGE);
		debug_dma_addr		 : out std_logic_vector(RAM0_ADDR_RANGE);
		debug_dma_prog_full	 : out std_logic;
		debug_dma_empty		 : out std_logic;
		
		-- dram control signals
		dram_ready	         : in  std_logic;
		dram_rd_en           : out  std_logic;
		dram_rd_addr         : out std_logic_vector(RAM0_ADDR_RANGE);
		dram_rd_data         : in  std_logic_vector(DRAM0_DATA_RANGE);
		dram_rd_valid        : in  std_logic);
end dram_rd_custom;


architecture default of dram_rd_custom is
    COMPONENT dram_rd_asym_fifo
        PORT (
        rst         : IN STD_LOGIC;
        wr_clk      : IN STD_LOGIC;
        rd_clk      : IN STD_LOGIC;
        din         : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        wr_en       : IN STD_LOGIC;
        rd_en       : IN STD_LOGIC;
        dout        : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
        full        : OUT STD_LOGIC;
        empty       : OUT STD_LOGIC;
        prog_full   : OUT STD_LOGIC;
        wr_rst_busy : OUT STD_LOGIC;
        rd_rst_busy : OUT STD_LOGIC 
        );
    END COMPONENT;
    
	-- user signals
	signal combined_user_rst      : std_logic;
	signal user_done_rst          : std_logic;
	
	-- dram fifo signals
	signal dram_fifo_full         : std_logic;
	signal dram_fifo_empty        : std_logic;
	signal dram_fifo_rd_en        : std_logic;
	signal dram_fifo_wr_rst_busy     : std_logic;
	signal dram_fifo_rd_rst_busy     : std_logic;
	signal dram_fifo_rst_busy     : std_logic;
	signal dram_fifo_rst          : std_logic;
	signal dram_fifo_combined_rst : std_logic;
	
	-- dram done signals
	signal dram_done_sync_0       : std_logic;
	signal dram_done_sync_1       : std_logic;
	signal dram_not_ready         : std_logic;
	signal done_r                 : std_logic;
	signal valid_read_count       : std_logic_vector(RAM0_RD_SIZE_RANGE);
	signal done_count             : std_logic_vector(5 downto 0);
	
	-- Misc signals
	signal size_int               : integer;
	signal addr_gen_stall         : std_logic;
	signal addr_gen_start         : std_logic;
	signal dram_rst_pending       : std_logic;
	signal dram_rd_data_converted : std_logic_vector(DRAM0_DATA_RANGE);
	signal size_converted         : std_logic_vector(DRAM0_SIZE_RANGE);
  
begin
	-- flip read dram data to go into fifo
	dram_rd_data_converted(C_DRAM0_DATA_WIDTH-1 downto C_DRAM0_DATA_WIDTH/2) <= dram_rd_data(C_DRAM0_DATA_WIDTH/2-1 downto 0);
	dram_rd_data_converted(C_DRAM0_DATA_WIDTH/2-1 downto 0) <= dram_rd_data(C_DRAM0_DATA_WIDTH-1 downto C_DRAM0_DATA_WIDTH/2);
	
	-- 16 to 32 bit word size conversion
	size_int           <= to_integer(unsigned(size));
	size_converted <= std_logic_vector(to_unsigned(size_int, size_converted'length)/2) when size_int mod 2 = 0 else
	                  std_logic_vector(to_unsigned(size_int, size_converted'length)/2 + 1) when size_int mod 2 /= 0 else
	                  (others => '0');

	-- Synchronize go signal from user frequency to dram frequency to begin FIFO operations.
	ADDR_GEN_SYNC : entity work.handshake
        port map (
            clk_src   => user_clk,
            clk_dest  => dram_clk,
            rst       => combined_user_rst,
            go        => go,
            delay_ack => '0',
			--ack       => nc
            rcv       => addr_gen_start);
			
	-- Generate addresses to read from RAM.
	U_ADDR_GEN : entity work.dram_addr_gen
	   generic map (
	       addr_width    => C_RAM0_ADDR_WIDTH)
	   port map (
	       clk         => dram_clk,
	       rst         => dram_fifo_combined_rst,
	       go          => addr_gen_start,
	       size        => size_converted,
		   stall       => addr_gen_stall,
		   stop        => '0',
		   addr_start  => start_addr,
	       addr_valid  => dram_rd_en,
	       addr_out    => dram_rd_addr);
	       
	-- FIFO entity to store RAM data and provide to user app pipeline.
	DRAM_RD_FIFO : dram_rd_asym_fifo
		PORT MAP (
		rst           => dram_rst,
		wr_clk        => dram_clk,
		rd_clk        => user_clk,
		din           => dram_rd_data_converted,
		wr_en         => dram_rd_valid,
		rd_en         => dram_fifo_rd_en,
		dout          => data,
		--full        => nc,
		empty         => dram_fifo_empty,
		prog_full     => dram_fifo_full,
		wr_rst_busy   => dram_fifo_wr_rst_busy,
		rd_rst_busy => dram_fifo_rd_rst_busy
		);
		
	-- Logic to determine when to assert done.
	process(user_clk, combined_user_rst) begin
		if (combined_user_rst = '1') then
			valid_read_count <= (others => '0');
			done_r           <= '0';
		elsif (rising_edge(user_clk)) then
			-- Count the amount of valid reads to determine when to assert done.
			if (dram_fifo_rd_en = '1') then
				valid_read_count <= std_logic_vector(unsigned(valid_read_count) + 1);
			end if;
			if (unsigned(valid_read_count) >= unsigned(size) AND unsigned(size) > 0) then
				done_r <= '1';
			else
				done_r <= '0';
			end if;
			
			-- If go is asserted, reset signals to not interfere with new computation.
			if (go = '1') then
				valid_read_count <= (others => '0');
				done_r <= '0';
			end if;
		end if;
	end process;
	
	-- Logic for user space reset between runs.
	process(user_clk, user_rst) begin
		if (user_rst = '1') then
			user_done_rst <= '0';
			done_count    <= (others => '0');
		elsif (rising_edge(user_clk)) then
			-- Determine how long done is asserted.
			if (done_r = '1') then
				done_count <= std_logic_vector(unsigned(done_count) + 1);
			else
				done_count <= (others => '0');
			end if;
			
			-- Assert user reset once done has been asserted for 12 cycles.
			if (unsigned(done_count) >= 12) then
				user_done_rst <= '1';
				done_count    <= (others => '0');
			else
				user_done_rst <= '0';
			end if;
		end if;
	end process;
	
	-- Dual flop synchronize the done signal to reset the dram domain fifo.
	process(dram_clk, dram_rst) begin
		if (dram_rst = '1') then
			dram_done_sync_0 <= '0';
			dram_done_sync_1 <= '0';
			dram_fifo_rst    <= '0';
			dram_rst_pending <= '0';
		elsif (rising_edge(dram_clk)) then
			dram_done_sync_0 <= user_done_rst;
			dram_done_sync_1 <= dram_done_sync_0;
		end if;
	end process;
	
	--dram_fifo_rst_busy     <= dram_fifo_wr_rst_busy OR dram_fifo_rd_rst_busy;
	addr_gen_stall         <= dram_not_ready OR dram_fifo_full;
	dram_fifo_rd_en        <= rd_en AND (NOT dram_fifo_empty);
	valid                  <= NOT dram_fifo_empty;
	dram_not_ready         <= NOT dram_ready;
	combined_user_rst      <= user_rst OR user_done_rst;
	dram_fifo_combined_rst <= dram_rst OR dram_done_sync_1;
	done                   <= done_r;
	user_interim_rst       <= combined_user_rst;
end default;
