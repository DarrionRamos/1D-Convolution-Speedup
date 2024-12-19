-- Darrion Ramos
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.user_pkg.all;
use work.math_custom.all;

entity shift_buffer is
	generic(num_regs :   natural  := C_KERNEL_SIZE;
		    width    :   positive := C_KERNEL_WIDTH);
	port(clk          : in  std_logic;
		 rst          : in  std_logic;
		 r_en         : in  std_logic;
		 w_en	      : in  std_logic;
		 data_input   : in  std_logic_vector(width-1 downto 0);
		 full	      : out std_logic := '0';
		 empty        : out std_logic := '1';
         data_output  : out window);
end shift_buffer;


architecture default of shift_buffer is

	type reg_array is array (0 to num_regs-1) of std_logic_vector(width-1 downto 0);
	
	signal regs : reg_array;
	signal full_local, empty_local : std_logic;
	signal count : std_logic_vector(clog2(num_regs) downto 0);
begin
	-- read/write logic
    process(clk, rst)
    begin
		if (rst = '1') then
			count <= (others => '0');
			for i in 0 to num_regs-1 loop
				regs(i) <= (others => '0');
			end loop;
		elsif (clk'event and clk = '1') then
			if (w_en = '1' AND (full_local = '0' OR r_en = '1')) then
				regs(0) <= data_input;
				
				-- Shift all data in registers.
				for i in 0 to num_regs-2 loop
					regs(i+1) <= regs(i);
				end loop;
				
				-- Update the count as long as we are not reading at the same time.
				if (r_en = '0') then
					count <= std_logic_vector(unsigned(count) + 1);
				end if;
			end if;
        
			if (r_en = '1' AND empty_local = '0' AND w_en = '0') then
				regs(num_regs-1) <= (others => '0');
				count <= std_logic_vector(unsigned(count) - 1);
			end if;
		end if;
    end process;
	
	-- output mapping
	process(regs)
	begin
		for i in 0 to num_regs-1 loop
			data_output(i) <= regs(i);
        end loop;
	end process;
	
	-- full_local/empty_local control
	process(count)
	begin
		if (unsigned(count) < num_regs) then
			full_local  <= '0';
		elsif (r_en = '0') then
			full_local  <= '1';
		end if;
		
		if (unsigned(count) = 0) then
			empty_local <= '1';
		else
			empty_local <= '0';
		end if;
	end process;
	full <= full_local;
	empty <= empty_local;
end default;
