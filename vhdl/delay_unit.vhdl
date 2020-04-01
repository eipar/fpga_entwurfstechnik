-----------------------------------------------------------
--      Institute of Microelectronic Systems
--      Architectures and Systems
--      Leibniz Universitaet Hannover
-----------------------------------------------------------
--      lab :         Design Methods for FPGAs
--      file :        delay_unit.vhdl
--      authors :
--      last update :
--      description :
-----------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.fpga_audiofx_pkg.all;

entity delay_unit is
  generic (
    BASE_ADDR_0        : natural := 16#00000000#;
    BASE_ADDR_1        : natural := 16#01000000#;
    BUFFER_SIZE        : natural := 16#01000000#;
    DELAY_SHIFT_FACTOR : natural;							-- should be 10 => 2^10 = 1024 -> 5.92s
    OUTPORTS           : natural								-- should be 7 maximum
    );
  port (
    clock               : in  std_ulogic;
    reset               : in  std_ulogic;
    -- audio signals
    ain_sync_0          : in  std_ulogic;
    ain_data_0          : in  std_ulogic;
    ain_sync_1          : in  std_ulogic;
    ain_data_1          : in  std_ulogic;
    aout_sync_0         : out std_ulogic_vector(OUTPORTS-1 downto 0);
    aout_data_0         : out std_ulogic_vector(OUTPORTS-1 downto 0);
    aout_sync_1         : out std_ulogic_vector(OUTPORTS-1 downto 0);
    aout_data_1         : out std_ulogic_vector(OUTPORTS-1 downto 0);
    -- register interface
    regif_cs            : in  std_ulogic;
    regif_wen           : in  std_ulogic;
    regif_addr          : in  std_ulogic_vector(REGIF_ADDR_WIDTH-1 downto 0);
    regif_data_in       : in  std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);
    regif_data_out      : out std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);
    -- sdram interface
    sdram_select        : out std_ulogic;
    sdram_write_en      : out std_ulogic;
    sdram_address       : out std_ulogic_vector(25 downto 0);
    sdram_data_in       : in  std_ulogic_vector(15 downto 0);
    sdram_data_out      : out std_ulogic_vector(15 downto 0);
    sdram_request_en    : out std_ulogic;
    sdram_req_slot_free : in  std_ulogic;
    sdram_data_avail    : in  std_ulogic
    );
end entity delay_unit;

architecture rtl of delay_unit is

	-- Signal Declaration
	-- control registers
	signal control_0, control_0_nxt : std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);	
	signal control_1, control_1_nxt : std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);
	
	-- output registers
	type delay_array is array (0 to OUTPORTS-1) of std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);
	signal output_delay_0, output_delay_0_nxt : delay_array;
	signal output_delay_1, output_delay_1_nxt : delay_array;
	
	-- pointers
	-- -- write
	signal write_pointer_0, write_pointer_0_nxt : std_ulogic_vector(25 downto 0);
	signal write_pointer_1, write_pointer_1_nxt : std_ulogic_vector(25 downto 0);
	-- -- read
	type read_pointer_array is array (0 to OUTPORTS-1) of std_ulogic_vector(25 downto 0);
	signal read_pointer_0, read_pointer_1 : read_pointer_array;
	-- -- increment write pointers
	signal inc_write_pointer_0, inc_write_pointer_1 : std_ulogic;
	-- -- max pointers
	signal max_write_pointer_0, max_write_pointer_0_nxt : std_ulogic_vector(25 downto 0);
	signal max_write_pointer_1, max_write_pointer_1_nxt : std_ulogic_vector(25 downto 0);
	-- -- enough samples
	signal enough_samples_0, enough_samples_1 : std_ulogic_vector(OUTPORTS-1 downto 0);
	
	-- Component Declaration	
	component s2p_unit is
		port(
			clock     : in  std_ulogic;
			reset     : in  std_ulogic;
			smp_ack   : in  std_ulogic;
			smp_valid : out std_ulogic;
			smp_data  : out std_ulogic_vector(SAMPLE_WIDTH-1 downto 0);
			ain_sync  : in std_ulogic;
			ain_data  : in std_ulogic
		);
	end component s2p_unit;
	
	component p2s_unit is
		port(
			clock     : in  std_ulogic;
			reset     : in  std_ulogic;
			smp_valid : in  std_ulogic;
			smp_ack   : out std_ulogic;
			smp_data  : in  std_ulogic_vector(SAMPLE_WIDTH-1 downto 0);
			aout_sync : out std_ulogic;
			aout_data : out std_ulogic
		);
	end component p2s_unit;
	
	-- Signal declaration for P2S and S2P units
	signal s2p_ack_0, s2p_ack_1 : std_ulogic;
	signal s2p_valid_0, s2p_valid_1 : std_ulogic;
	signal s2p_data_0, s2p_data_1 : std_ulogic_vector(SAMPLE_WIDTH-1 downto 0);
	
	signal p2s_ack_0, p2s_ack_1 : std_ulogic_vector(OUTPORTS-1 downto 0);
	signal p2s_valid_0, p2s_valid_1 : std_ulogic_vector(OUTPORTS-1 downto 0);
	type p2s_data_array is array (0 to OUTPORTS-1) of std_ulogic_vector(SAMPLE_WIDTH-1 downto 0);
	signal p2s_data_0, p2s_data_1 : p2s_data_array;
	
	-- Signal declaration for Counter 
	constant WIDTH_COUNTER : integer := integer(ceil(log2(real(OUTPORTS))));
	signal outport_cnt, outport_cnt_nxt : unsigned(WIDTH_COUNTER downto 0);
	signal inc_outport_cnt : std_ulogic;
	signal rst_outport_cnt : std_ulogic;
	
	-- Signal declaration for FSM
	type STATE_t is (ST_IDLE, ST_WRITE_DATA_0, ST_REQUEST_DATA_0, ST_WAIT_DATA_0, ST_WRITE_DATA_1, ST_REQUEST_DATA_1, ST_WAIT_DATA_1);
	signal state, state_nxt : STATE_t;
	
begin

	-- Aufgabe II
	-- Sequential process for registers
	register_process: process(clock, reset)
	begin
		if reset='1' then
			control_0 <= "00000011";								-- chosen for default that
			control_1 <= "00000011";								--	the input is active and has at least one active output
			output_delay_0 <= (others => (others => '0'));	-- default 0, so read pointer will follow write pointer
			output_delay_1 <= (others => (others => '0'));
		elsif rising_edge(clock) then
			control_0 <= control_0_nxt;
			control_1 <= control_1_nxt;
			output_delay_0 <= output_delay_0_nxt;
			output_delay_1 <= output_delay_1_nxt;
		end if;
	end process register_process;
	-- end Aufgabe II
	
	-- Aufgabe III
	-- register interface
	register_interface: process(regif_cs, regif_wen, regif_addr, regif_data_in, control_0, control_1, output_delay_0, output_delay_1)
	begin
		-- default conditions
		control_0_nxt <= control_0;
		control_1_nxt <= control_1;
		output_delay_0_nxt <= output_delay_0;
		output_delay_1_nxt <= output_delay_1;
		
		regif_data_out <= (others => '0');
		
		if regif_cs = '1' then
			if regif_wen = '1' then
				-- write in register
				
				if regif_addr = "00000000" then
					-- write in control_0
					control_0_nxt <= regif_data_in;
				elsif regif_addr = "00000001" then
					-- write in control_1
					control_1_nxt <= regif_data_in;
				elsif to_integer(unsigned(regif_addr)) >= (2 + OUTPORTS) then
					-- write in output_delay_1
					output_delay_1_nxt(to_integer(unsigned(regif_addr)) - (2 + OUTPORTS)) <= regif_data_in;
				elsif to_integer(unsigned(regif_addr)) < (2 + OUTPORTS) then
					-- write in output_delay_0
					output_delay_0_nxt(to_integer(unsigned(regif_addr)) - 2) <= regif_data_in;
				end if;
				-- if it is an invalid register, it does not do anything
			
			else
				-- read register
				
				if regif_addr = "00000000" then
					-- read control_0
					regif_data_out <= control_0;
				elsif regif_addr = "00000001" then
					-- read control_1
					regif_data_out <= control_1;
				elsif to_integer(unsigned(regif_addr)) >= (2 + OUTPORTS) then
					-- read output_delay_1
					regif_data_out <= output_delay_1(to_integer(unsigned(regif_addr)) - (2 + OUTPORTS));
				elsif to_integer(unsigned(regif_addr)) < (2 + OUTPORTS) then
					-- read output_delay_0
					regif_data_out <= output_delay_0(to_integer(unsigned(regif_addr)) - 2);
				end if;
				-- if it is an invalid register, it does not do anything
			
			end if;
		end if;
	end process register_interface;
	-- end Aufgabe III
	
	-- Aufgabe V
	-- write pointers register
	pointer_process: process(clock, reset)
	begin
		if reset='1' then
			write_pointer_0 <= std_ulogic_vector(to_unsigned(BASE_ADDR_0, 26));	-- the cast is needed, because BASE_ADDR_0 is natural
			write_pointer_1 <= std_ulogic_vector(to_unsigned(BASE_ADDR_1, 26));
		elsif rising_edge(clock) then
			write_pointer_0 <= write_pointer_0_nxt;
			write_pointer_1 <= write_pointer_1_nxt;
		end if;
	end process pointer_process;
	
	-- process to control write and read pointers
	pointer_control: process(write_pointer_0, write_pointer_1, output_delay_0, output_delay_1, inc_write_pointer_0, inc_write_pointer_1)
	begin
		-- default conditions
		write_pointer_0_nxt <= write_pointer_0;
		write_pointer_1_nxt <= write_pointer_1;
		
		-- if it is needed to write in input 0 buffer
		if inc_write_pointer_0 = '1' then
			-- check if it reached the buffer limit
			if write_pointer_0 = std_ulogic_vector(to_unsigned(BASE_ADDR_0+BUFFER_SIZE, 26)) then
				write_pointer_0_nxt <= std_ulogic_vector(to_unsigned(BASE_ADDR_0, 26));
			else
				-- otherwise add 1
				write_pointer_0_nxt <= std_ulogic_vector(unsigned(write_pointer_0) + 1);
			end if;
		end if;
		
		-- if it is needed to write in input 1 buffer
		if inc_write_pointer_1 = '1' then
			-- check if it reached the buffer limit
			if write_pointer_1 = std_ulogic_vector(to_unsigned(BASE_ADDR_1+BUFFER_SIZE, 26)) then
				write_pointer_1_nxt <= std_ulogic_vector(to_unsigned(BASE_ADDR_1, 26));
			else
				-- otherwise add 1
				write_pointer_1_nxt <= std_ulogic_vector(unsigned(write_pointer_1) + 1);
			end if;
		end if;
		
		--read pointer 0
		for i in 0 to OUTPORTS-1 loop
			-- if there is enough samples for the output delay needed
			if (unsigned(write_pointer_0) >= shift_left(unsigned(output_delay_0(i)),DELAY_SHIFT_FACTOR)) then
				-- just substract the delay
				read_pointer_0(i) <= std_ulogic_vector(unsigned(write_pointer_0) - shift_left(unsigned(output_delay_0(i)),DELAY_SHIFT_FACTOR));
			else
				-- if not, BUFFER_SIZE - output_delay*DELAY_SHIFT_FACTOR + write_pointer
				read_pointer_0(i) <= std_ulogic_vector(to_unsigned(BUFFER_SIZE, 26) - shift_left(unsigned(output_delay_0(i)),DELAY_SHIFT_FACTOR) + unsigned(write_pointer_0));
			end if;
		end loop;
		
		--read pointer 1
		for j in 0 to OUTPORTS-1 loop
			-- if there is enough samples for the output delay needed
			if (unsigned(write_pointer_1) >= (to_unsigned(BASE_ADDR_1, 26) + shift_left(unsigned(output_delay_1(j)),DELAY_SHIFT_FACTOR))) then
				-- just substract the delay
				read_pointer_1(j) <= std_ulogic_vector(unsigned(write_pointer_1) - shift_left(unsigned(output_delay_1(j)),DELAY_SHIFT_FACTOR));
			else
				-- if not, BUFFER_SIZE - output_delay*DELAY_SHIFT_FACTOR + write_pointer
				read_pointer_1(j) <= std_ulogic_vector(to_unsigned(BUFFER_SIZE, 26) - shift_left(unsigned(output_delay_1(j)),DELAY_SHIFT_FACTOR) + unsigned(write_pointer_1));
			end if;
		end loop;
	end process pointer_control;

	-- just for now, set this on 0 and then change it on the simulation
	--inc_write_pointer_0 <= '0';
	--inc_write_pointer_1 <= '0';
	
	-- end Aufgabe V
	
	-- Aufgabe VII
	-- register process
	max_reg_proc : process(clock, reset)
	begin
		if reset = '1' then
			max_write_pointer_0 <= (others => '0');
			max_write_pointer_1 <= (others => '0');
		elsif rising_edge(clock) then
			max_write_pointer_0 <= max_write_pointer_0_nxt;
			max_write_pointer_1 <= max_write_pointer_1_nxt;
		end if;
	end process max_reg_proc;
	
	-- control process to calculate max pointers and enough samples
	buffer_status : process(write_pointer_0, write_pointer_1, max_write_pointer_0, max_write_pointer_1, output_delay_0, output_delay_1)
	begin
		-- default condition
		max_write_pointer_0_nxt <= max_write_pointer_0;
		max_write_pointer_1_nxt <= max_write_pointer_1;
		
		-- max pointers
		-- if the write pointer is greater than max, change it
		if unsigned(write_pointer_0) > unsigned(max_write_pointer_0) then
			max_write_pointer_0_nxt <= write_pointer_0;
		end if;
		
		if unsigned(write_pointer_1) > unsigned(max_write_pointer_1) then
			max_write_pointer_1_nxt <= write_pointer_1;
		end if;
		
		-- enough samples
		-- check if there is enough samples on the buffer depending on the max write pointer and the delay specified for each output
		for i in 0 to OUTPORTS-1 loop
			if (unsigned(max_write_pointer_0) >= shift_left(unsigned(output_delay_0(i)),DELAY_SHIFT_FACTOR)) then
				enough_samples_0(i) <= '1';
			else
				enough_samples_0(i) <= '0';
			end if;
		end loop;
		
		for j in 0 to OUTPORTS-1 loop
			if (unsigned(max_write_pointer_1) >= (to_unsigned(BASE_ADDR_1, 26) + shift_left(unsigned(output_delay_1(j)),DELAY_SHIFT_FACTOR))) then
				enough_samples_1(j) <= '1';
			else
				enough_samples_1(j) <= '0';
			end if;
		end loop;
		
	end process buffer_status;
	-- end Aufgabe VII
	
	-- Aufgabe IX 
	-- instanciate S2P and P2s
	s2p_unit_0 : s2p_unit
	port map(
		clock     => clock,
		reset     => reset,
		smp_ack   => s2p_ack_0,
		smp_valid => s2p_valid_0,
		smp_data  => s2p_data_0,
		ain_sync  => ain_sync_0,
		ain_data  => ain_data_0
	);
	
	s2p_unit_1 : s2p_unit
	port map(
		clock     => clock,
		reset     => reset,
		smp_ack   => s2p_ack_1,
		smp_valid => s2p_valid_1,
		smp_data  => s2p_data_1,
		ain_sync  => ain_sync_1,
		ain_data  => ain_data_1
	);
	
	p2s_loop_0 : 
	for i in 0 to OUTPORTS-1 generate
		p2s_unit_0 : p2s_unit
		port map(
			clock     => clock,
			reset     => reset,
			smp_valid => p2s_valid_0(i),
			smp_ack   => p2s_ack_0(i),
			smp_data  => p2s_data_0(i),
			aout_sync => aout_sync_0(i),
			aout_data => aout_data_0(i)
		);
	end generate p2s_loop_0;
	
	p2s_loop_1 : 
	for i in 0 to OUTPORTS-1 generate
		p2s_unit_1 : p2s_unit
		port map(
			clock     => clock,
			reset     => reset,
			smp_valid => p2s_valid_1(i),
			smp_ack   => p2s_ack_1(i),
			smp_data  => p2s_data_1(i),
			aout_sync => aout_sync_1(i),
			aout_data => aout_data_1(i)
		);
	end generate p2s_loop_1;
	-- end Aufgabe IX
	
	-- Aufgabe X
	-- register
	outport_counter_register : process(reset, clock)
	begin
		if reset = '1' then
			outport_cnt <= (others => '0');
		elsif rising_edge(clock) then
			outport_cnt <= outport_cnt_nxt;
		end if;	end process outport_counter_register;
	-- counter
	outport_counter : process(rst_outport_cnt, inc_outport_cnt, outport_cnt)
	begin
		outport_cnt_nxt <= outport_cnt;
		if rst_outport_cnt = '1' then
			outport_cnt_nxt <= (others => '0');
		elsif inc_outport_cnt = '1' then
			outport_cnt_nxt <= outport_cnt + 1;
		end if;
	end process outport_counter;
	-- end Aufgabe X
	
	-- Aufgabe XI
	fsm_ff : process(clock, reset)
	begin
		if reset = '1' then
			state <= ST_IDLE;
		elsif rising_edge(clock) then
			state <= state_nxt;
		end if;
	end process fsm_ff;
	
	fsm_control : process(state, s2p_valid_0, control_0, sdram_req_slot_free, write_pointer_0, s2p_data_0, enough_samples_0, outport_cnt, read_pointer_0, sdram_data_avail, p2s_ack_0, sdram_data_in, s2p_valid_1, control_1, write_pointer_1, s2p_data_1, enough_samples_1, read_pointer_1, p2s_ack_1)
	begin
		-- default states 
		state_nxt <= state;
		
		sdram_select <= '0';
		sdram_request_en <= '0';
		sdram_write_en <= '0';
		sdram_address  <= (others => '0');
		sdram_data_out <= (others => '0');
		
		rst_outport_cnt <= '0';
		inc_outport_cnt <= '0';
		
		s2p_ack_0 <= '0';
		p2s_valid_0 <= (others => '0');
		p2s_data_0 <= (others => (others => '0'));
		
		inc_write_pointer_0 <= '0';
		
		s2p_ack_1 <= '0';
		p2s_valid_1 <= (others => '0');
		p2s_data_1 <= (others => (others => '0'));
		
		inc_write_pointer_1 <= '0';
					
		-- cntrl
		case state is 
			when ST_IDLE =>
				-- si hay sample nuevo en 0 e input 0 está activada
				if ((s2p_valid_0 = '1') AND (control_0(0) = '1')) then 
					state_nxt <= ST_WRITE_DATA_0;
				-- si hay sample nuevo en 1 e input 1 está activada
				elsif ((s2p_valid_1 = '1') AND (control_1(0) = '1')) then 
					state_nxt <= ST_WRITE_DATA_1;
				else
					state_nxt <= ST_IDLE;
				end if;
			----------------- INPUT 0
			when ST_WRITE_DATA_0 =>
				--slot request libre
				if (sdram_req_slot_free = '1') then
					state_nxt <= ST_REQUEST_DATA_0;
					sdram_select <= '1';
					sdram_request_en <= '1';
					sdram_write_en <= '1';
					sdram_address <= write_pointer_0;
					sdram_data_out <= s2p_data_0;
					inc_write_pointer_0 <= '1';
					s2p_ack_0 <= '1';
				else 
					state_nxt <= ST_WRITE_DATA_0;
				end if;
			when ST_REQUEST_DATA_0 =>
				-- hay salidad activas y request slot libre y suficientes samples 
				if (outport_cnt = to_unsigned(OUTPORTS, WIDTH_COUNTER+1)) then 	-- no hay más salidas activas
					rst_outport_cnt <= '1';
					state_nxt <= ST_IDLE;
				elsif (control_0(to_integer(outport_cnt + 1)) = '0') then 
					state_nxt <= ST_REQUEST_DATA_0;
					inc_outport_cnt <= '1';
				elsif (enough_samples_0(to_integer(outport_cnt)) = '0') then
					state_nxt <= ST_REQUEST_DATA_0;
					inc_outport_cnt <= '1';
					p2s_valid_0(to_integer(outport_cnt)) <= '1';
					p2s_data_0(to_integer(outport_cnt)) <= (others => '0');
				elsif(sdram_req_slot_free = '1') then 
					state_nxt <= ST_WAIT_DATA_0;
					sdram_select <= '1';
					sdram_request_en <= '1';
					sdram_write_en <= '0';
					sdram_address <= read_pointer_0(to_integer(outport_cnt));
				else -- no hay más salidas activas de las ya vistas y no hay más salidas disponibles y no hay request slot disponible ni suf samples
					state_nxt <= ST_REQUEST_DATA_0;
				end if;
			when ST_WAIT_DATA_0 =>
				-- hay datos de la SDRAM y la P2S está lista
				if ((sdram_data_avail = '1') AND (p2s_ack_0(to_integer(outport_cnt)) = '1')) then
					state_nxt <= ST_REQUEST_DATA_0;
					sdram_select <= '1';
					p2s_valid_0(to_integer(outport_cnt)) <= '1';
					p2s_data_0(to_integer(outport_cnt)) <= sdram_data_in;
					inc_outport_cnt <= '1';
				else
					-- SDRAM no dio datos y/o la P2S no está lista
					state_nxt <= ST_WAIT_DATA_0;
				end if;
			----------------- INPUT 1
			when ST_WRITE_DATA_1 =>
				--slot request libre
				if (sdram_req_slot_free = '1') then
					state_nxt <= ST_REQUEST_DATA_1;
					sdram_select <= '1';
					sdram_request_en <= '1';
					sdram_write_en <= '1';
					sdram_address <= write_pointer_1;
					sdram_data_out <= s2p_data_1;
					inc_write_pointer_1 <= '1';
					s2p_ack_1 <= '1';
				else 
					state_nxt <= ST_WRITE_DATA_1;
				end if;
			when ST_REQUEST_DATA_1 =>
				-- hay salidad activas y request slot libre y suficientes samples 
				if (outport_cnt = to_unsigned(OUTPORTS, WIDTH_COUNTER+1)) then 	-- no hay más salidas activas
					rst_outport_cnt <= '1';
					state_nxt <= ST_IDLE;
				elsif (control_1(to_integer(outport_cnt + 1)) = '0') then 
					state_nxt <= ST_REQUEST_DATA_1;
					inc_outport_cnt <= '1';
				elsif (enough_samples_1(to_integer(outport_cnt)) = '0') then
					state_nxt <= ST_REQUEST_DATA_1;
					inc_outport_cnt <= '1';
					p2s_valid_1(to_integer(outport_cnt)) <= '1';
					p2s_data_1(to_integer(outport_cnt)) <= (others => '0');
				elsif(sdram_req_slot_free = '1') then 
					state_nxt <= ST_WAIT_DATA_1;
					sdram_select <= '1';
					sdram_request_en <= '1';
					sdram_write_en <= '0';
					sdram_address <= read_pointer_1(to_integer(outport_cnt));
				else -- no hay más salidas activas de las ya vistas y no hay más salidas disponibles y no hay request slot disponible ni suf samples
					state_nxt <= ST_REQUEST_DATA_1;
				end if;
			when ST_WAIT_DATA_1 =>
				-- hay datos de la SDRAM y la P2S está lista
				if ((sdram_data_avail = '1') AND (p2s_ack_1(to_integer(outport_cnt)) = '1')) then
					state_nxt <= ST_REQUEST_DATA_1;
					sdram_select <= '1';
					p2s_valid_1(to_integer(outport_cnt)) <= '1';
					p2s_data_1(to_integer(outport_cnt)) <= sdram_data_in;
					inc_outport_cnt <= '1';
				else
					-- SDRAM no dio datos y/o la P2S no está lista
					state_nxt <= ST_WAIT_DATA_1;
				end if;
			when others =>
				state_nxt <= ST_IDLE;
		end case;
	end process fsm_control;
	-- end Aufgabe XI

	-- check valid generic-configuration
	assert ((OUTPORTS >= 1) and (OUTPORTS <= 7)) report "[Delay Unit] Illegal number of outports!" severity failure;
	assert (BASE_ADDR_0 /= BASE_ADDR_1) report "[Delay Unit] Buffer Base Addresses can't be identical!" severity failure;
	assert (BASE_ADDR_0 > BASE_ADDR_1) or ((BASE_ADDR_0 + BUFFER_SIZE - 1) < BASE_ADDR_1) report "[Delay Unit] Buffer Ranges do not match!" severity failure;
	assert (BASE_ADDR_0 < BASE_ADDR_1) or ((BASE_ADDR_1 + BUFFER_SIZE - 1) < BASE_ADDR_0) report "[Delay Unit] Buffer Ranges do not match!" severity failure;

end architecture rtl;
