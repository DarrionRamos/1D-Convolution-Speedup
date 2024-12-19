-- Greg Stitt
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;

use work.config_pkg.all;
use work.user_pkg.all;

use ieee.numeric_std.all;
use ieee.math_real.all;
use work.math_custom.all;

entity user_app is
    port (
        clk 			 : in std_logic;
        rst 			 : in std_logic;
		user_interim_rst : in std_logic;

        -- Memory-map interface
        mmap_wr_en   : in  std_logic;
        mmap_wr_addr : in  std_logic_vector(MMAP_ADDR_RANGE);
        mmap_wr_data : in  std_logic_vector(MMAP_DATA_RANGE);
        mmap_rd_en   : in  std_logic;
        mmap_rd_addr : in  std_logic_vector(MMAP_ADDR_RANGE);
        mmap_rd_data : out std_logic_vector(MMAP_DATA_RANGE);

        -- DMA read interface for RAM 0
        ram0_rd_rd_en : out std_logic;
        ram0_rd_go    : out std_logic;
        ram0_rd_valid : in  std_logic;
        ram0_rd_data  : in  std_logic_vector(RAM0_RD_DATA_RANGE);
        ram0_rd_addr  : out std_logic_vector(RAM0_ADDR_RANGE);
        ram0_rd_size  : out std_logic_vector(RAM0_RD_SIZE_RANGE);
        ram0_rd_done  : in  std_logic;

        -- DMA write interface for RAM 1 
        ram1_wr_ready : in  std_logic;
        ram1_wr_go    : out std_logic;
        ram1_wr_valid : out std_logic;
        ram1_wr_data  : out std_logic_vector(RAM1_WR_DATA_RANGE);
        ram1_wr_addr  : out std_logic_vector(RAM1_ADDR_RANGE);
        ram1_wr_size  : out std_logic_vector(RAM1_WR_SIZE_RANGE);
        ram1_wr_done  : in  std_logic
        );
end user_app;

architecture default of user_app is

	-- global wires/control
    signal go                   : std_logic;
	signal combined_rst         : std_logic;
	signal sw_rst               : std_logic;
    signal size                 : std_logic_vector(RAM0_RD_SIZE_RANGE);
    signal done                 : std_logic;
	signal pipeline_output_temp : std_logic_vector(C_KERNEL_WIDTH+C_SIGNAL_WIDTH+clog2(C_KERNEL_SIZE)-1 downto 0);
	
	-- kernel buffer wires/control
	signal kernel_pipe_data     : window;
	signal kernel_pipe_data_sv  : std_logic_vector(C_KERNEL_SIZE*C_KERNEL_WIDTH-1 downto 0);
	signal kernel_buffer_full   : std_logic;
	signal kernel_sw_data       : std_logic_vector(KERNEL_WIDTH_RANGE);
	signal kernel_sw_load       : std_logic;
	
	-- signal buffer wires/control
	signal signal_pipe_data     : window;
	signal signal_pipe_data_sv  : std_logic_vector(C_KERNEL_SIZE*C_SIGNAL_WIDTH-1 downto 0);
	signal signal_buffer_full   : std_logic;
	signal signal_buf_rd_en     : std_logic;
	
begin
	-- window to std_logic_vector conversions (and swap directions for kernel data)
	process (kernel_pipe_data) begin
		for i in 0 to C_KERNEL_SIZE-1 loop
			kernel_pipe_data_sv((i+1)*C_KERNEL_WIDTH-1 downto i*C_KERNEL_WIDTH) <= kernel_pipe_data((C_KERNEL_SIZE-1)-i);
		end loop;
	end process;
	
	process (signal_pipe_data) begin
	for i in 0 to C_KERNEL_SIZE-1 loop
		signal_pipe_data_sv((i+1)*C_SIGNAL_WIDTH-1 downto i*C_SIGNAL_WIDTH) <= signal_pipe_data(i);
	end loop;
	end process;
	
    U_MMAP : entity work.memory_map
        port map (
            clk => clk,
            rst => rst,

            wr_en   => mmap_wr_en,
            wr_addr => mmap_wr_addr,
            wr_data => mmap_wr_data,
            rd_en   => mmap_rd_en,
            rd_addr => mmap_rd_addr,
            rd_data => mmap_rd_data,

            -- circuit interface from software
            go           => go,
			clear        => sw_rst,
			--sw_rst       => nc,
			signal_size  => size,
			kernel_data  => kernel_sw_data,
			kernel_load  => kernel_sw_load,
			kernel_ready => kernel_buffer_full,
			done         => done
            );

	U_SIGNAL_BUF : entity work.shift_buffer
		port map(
			clk          => clk,
			rst          => combined_rst,
			r_en         => signal_buf_rd_en,
			w_en	     => ram0_rd_valid,
			data_input   => ram0_rd_data,
			full		 => signal_buffer_full,
			--empty        => nc,
			--window_valid => nc,
			data_output  => signal_pipe_data
			);
			
	U_KERNEL_BUF : entity work.shift_buffer
		port map(
			clk         => clk,
			rst         => combined_rst,
			r_en        => '0',
			w_en	    => kernel_sw_load,
			data_input  => kernel_sw_data,
			full        => kernel_buffer_full,
			--empty,
			data_output => kernel_pipe_data
			);
			
	U_PIPELINE : entity work.mult_add_tree(unsigned_arch)
		generic map (
			num_inputs   => C_KERNEL_SIZE,
			input1_width => C_KERNEL_WIDTH,
			input2_width => C_SIGNAL_WIDTH)
		port map(
			clk    => clk,
			rst    => combined_rst,
			en     => ram1_wr_ready,
			input1 => kernel_pipe_data_sv,
			input2 => signal_pipe_data_sv,
			output => pipeline_output_temp
			);
			
	-- pipeline output clipping logic
	process (pipeline_output_temp) begin
		if (unsigned(pipeline_output_temp(C_KERNEL_WIDTH+C_SIGNAL_WIDTH+clog2(C_KERNEL_SIZE)-1 downto 16)) = 0) then
			ram1_wr_data <= pipeline_output_temp(15 downto 0);
		else
			ram1_wr_data <= (others => '1');
		end if;
	end process;
	
	-- Delay (valid) logic
	U_PIPELINE_VALID : entity work.delay
		generic map (
			CYCLES => clog2(C_KERNEL_SIZE)+1,
			WIDTH  => 1)
		port map(
			clk    => clk,
			rst    => combined_rst,
			en     => ram1_wr_ready,
			input(0)  => signal_buf_rd_en,
			output(0) => ram1_wr_valid
			);
	
    -- RAM0 control.
    ram0_rd_go    <= go;
    ram0_rd_size  <= std_logic_vector(unsigned(size) + (2*(C_KERNEL_SIZE-1)));
	ram0_rd_addr  <= (others => '0'); 
	ram0_rd_rd_en <= ram1_wr_ready AND ram0_rd_valid;

    -- RAM1 control.
    ram1_wr_go    <= go;
    ram1_wr_size  <= std_logic_vector(unsigned(size) + (C_KERNEL_SIZE-1));
	ram1_wr_addr  <= (others => '0');

	-- Misc signal control
    done <= ram1_wr_done;
	signal_buf_rd_en <= ram1_wr_ready AND signal_buffer_full;
	combined_rst <= rst OR user_interim_rst OR sw_rst;

end default;
