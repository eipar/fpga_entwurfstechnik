-----------------------------------------------------------------------------
--      Institute of Microelectronic Systems
--      Architectures and Systems
--      Leibniz Universitaet Hannover
-----------------------------------------------------------------------------
--      lab :         Design Methods for FPGAs
--      file :        sdram_ctrl_func_model.vhdl
--      authors :     Jan Duerre
--      last update : 02.09.2014
--      description : This model is only functional. The timing is completely 
--                    different from the real SDRAM! You can only use this
--                    model to test if your communication with the SDRAM-
--                    Controller is correct! (The simulated size of the
--                    SDRAM is only 2**9 32-bit-words, larger addresses are
--                    ignored.)
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity sdram_ctrl is
  port (
    clk_clk                  : in    std_logic;
    reset_reset_n            : in    std_logic;
    wrapper_wire_addr        : out   std_logic_vector(12 downto 0);
    wrapper_wire_ba          : out   std_logic_vector(1 downto 0);
    wrapper_wire_cas_n       : out   std_logic;
    wrapper_wire_cke         : out   std_logic;
    wrapper_wire_cs_n        : out   std_logic;
    wrapper_wire_dq          : inout std_logic_vector(31 downto 0);
    wrapper_wire_dqm         : out   std_logic_vector(3 downto 0);
    wrapper_wire_ras_n       : out   std_logic;
    wrapper_wire_we_n        : out   std_logic;
    wrapper_s1_address       : in    std_logic_vector(24 downto 0);
    wrapper_s1_byteenable_n  : in    std_logic_vector(3 downto 0);
    wrapper_s1_chipselect    : in    std_logic;
    wrapper_s1_writedata     : in    std_logic_vector(31 downto 0);
    wrapper_s1_read_n        : in    std_logic;
    wrapper_s1_write_n       : in    std_logic;
    wrapper_s1_readdata      : out   std_logic_vector(31 downto 0);
    wrapper_s1_readdatavalid : out   std_logic;
    wrapper_s1_waitrequest   : out   std_logic
    );
end sdram_ctrl;

architecture sim of sdram_ctrl is

  constant SDRAM_SIMULATION_SIZE : natural := 2**9;

  -- connection signals to entity
  signal clock                    : std_ulogic;
  signal reset_n                  : std_ulogic;
  signal avalon_address           : std_ulogic_vector(24 downto 0);
  signal avalon_byteenable_n      : std_ulogic_vector(3 downto 0);
  signal avalon_chipselect        : std_ulogic;
  signal avalon_writedata         : std_ulogic_vector(31 downto 0);
  signal avalon_read_n            : std_ulogic;
  signal avalon_write_n           : std_ulogic;
  signal avalon_readdata          : std_ulogic_vector(31 downto 0);
  signal avalon_readdata_nxt      : std_ulogic_vector(31 downto 0);
  signal avalon_readdatavalid     : std_ulogic;
  signal avalon_readdatavalid_nxt : std_ulogic;
  signal avalon_waitrequest       : std_ulogic;

  -- sdram data array
  type sdram_array_t is array (0 to SDRAM_SIMULATION_SIZE-1) of std_ulogic_vector(31 downto 0);
  signal sdram_data, sdram_data_nxt : sdram_array_t;

  -- readdata output pipeline to simulate read delay
  constant READPIPELINE_SIZE : natural := 4;
  type readdata_pipeline_t is array (0 to READPIPELINE_SIZE-1) of std_ulogic_vector(31 downto 0);

  signal readdata_pipeline          : readdata_pipeline_t;
  signal readdata_pipeline_nxt      : readdata_pipeline_t;
  signal readdatavalid_pipeline     : std_ulogic_vector(READPIPELINE_SIZE-1 downto 0);
  signal readdatavalid_pipeline_nxt : std_ulogic_vector(READPIPELINE_SIZE-1 downto 0);

  -- counter to simulate bootup
  constant INIT_CYCLES          : natural := 64;
  signal init_cnt, init_cnt_nxt : natural;
  signal init_busy              : std_ulogic;

  -- counters to simulate refresh cycles
  constant REFRESH_TIME                           : natural := 32;
  constant REFRESH_CYCLES                         : natural := 8;
  signal refresh_cnt, refresh_cnt_nxt             : natural;
  signal refresh_cycle_cnt, refresh_cycle_cnt_nxt : natural;
  signal refresh_busy                             : std_ulogic;

begin
  -- dram signals: not used / connected to anything
  wrapper_wire_addr  <= (others => '0');
  wrapper_wire_ba    <= (others => '0');
  wrapper_wire_cas_n <= '1';
  wrapper_wire_cke   <= '0';
  wrapper_wire_cs_n  <= '1';
  wrapper_wire_dq    <= (others => 'Z');
  wrapper_wire_dqm   <= (others => '0');
  wrapper_wire_ras_n <= '1';
  wrapper_wire_we_n  <= '1';

  -- map ports to signals with better names :)
  clock                    <= std_ulogic(clk_clk);
  reset_n                  <= std_ulogic(reset_reset_n);
  avalon_address           <= std_ulogic_vector(wrapper_s1_address);
  avalon_byteenable_n      <= std_ulogic_vector(wrapper_s1_byteenable_n);
  avalon_chipselect        <= std_ulogic(wrapper_s1_chipselect);
  avalon_writedata         <= std_ulogic_vector(wrapper_s1_writedata);
  avalon_read_n            <= std_ulogic(wrapper_s1_read_n);
  avalon_write_n           <= std_ulogic(wrapper_s1_write_n);
  wrapper_s1_readdata      <= std_logic_vector(avalon_readdata);
  wrapper_s1_readdatavalid <= std_logic(avalon_readdatavalid);
  wrapper_s1_waitrequest   <= std_logic(avalon_waitrequest);

  -- FFs to sync to sdram clock
  process(reset_n, clock)
  begin
    if reset_n = '0' then
      readdata_pipeline      <= (others => (others => '0'));
      readdatavalid_pipeline <= (others => '0');
      avalon_readdata        <= (others => '0');
      avalon_readdatavalid   <= '0';
      sdram_data             <= (others => (others => 'U'));
      init_cnt               <= 0;
      refresh_cnt            <= 0;
      refresh_cycle_cnt      <= REFRESH_CYCLES;
    elsif rising_edge(clock) then
      readdata_pipeline      <= readdata_pipeline_nxt;
      readdatavalid_pipeline <= readdatavalid_pipeline_nxt;
      avalon_readdata        <= avalon_readdata_nxt;
      avalon_readdatavalid   <= avalon_readdatavalid_nxt;
      sdram_data             <= sdram_data_nxt;
      init_cnt               <= init_cnt_nxt;
      refresh_cnt            <= refresh_cnt_nxt;
      refresh_cycle_cnt      <= refresh_cycle_cnt_nxt;
    end if;
  end process;

  -- counter to simulate init-cycles
  init_cnt_nxt <= init_cnt + 1 when init_cnt < INIT_CYCLES else init_cnt;
  init_busy    <= '0'          when init_cnt = INIT_CYCLES else '1';

  -- counter to simulate refresh-cycles
  process(init_busy, refresh_cnt, refresh_cycle_cnt)
  begin
    refresh_cnt_nxt       <= refresh_cnt;
    refresh_cycle_cnt_nxt <= refresh_cycle_cnt;
    refresh_busy          <= '0';

    -- wait until init is done
    if init_busy = '0' then
      -- count until refresh time is reached
      if refresh_cnt = REFRESH_TIME then
        -- reset refresh cycle counter
        refresh_cycle_cnt_nxt <= 0;
      else
        refresh_cnt_nxt <= refresh_cnt + 1;
      end if;
    end if;

    -- count if refresh cycles not reached
    if refresh_cycle_cnt < REFRESH_CYCLES then
      refresh_cycle_cnt_nxt <= refresh_cycle_cnt + 1;
      -- refreshing
      refresh_busy          <= '1';
      -- on last refresh cycle count
      if refresh_cycle_cnt + 1 = REFRESH_CYCLES then
        -- reset refresh counter to count till next refresh cycle
        refresh_cnt_nxt <= 0;
      end if;
    end if;

  end process;

  -- busy when initializing or refreshing
  avalon_waitrequest <= init_busy or refresh_busy;

  -- data access
  process(avalon_chipselect, avalon_read_n, avalon_write_n, avalon_address, avalon_byteenable_n, avalon_writedata, avalon_waitrequest, sdram_data, readdata_pipeline, readdatavalid_pipeline)
  begin
    -- shift readdata pipeline
    readdata_pipeline_nxt(READPIPELINE_SIZE-1) <= (others => '0');
    for i in 0 to READPIPELINE_SIZE-2 loop
      readdata_pipeline_nxt(i) <= readdata_pipeline(i+1);
    end loop;
    readdatavalid_pipeline_nxt <= '0' & readdatavalid_pipeline(READPIPELINE_SIZE-1 downto 1);
    -- emit last line of pipeline
    avalon_readdata_nxt        <= readdata_pipeline(0);
    avalon_readdatavalid_nxt   <= readdatavalid_pipeline(0);

    sdram_data_nxt <= sdram_data;

    -- chipselect
    if avalon_chipselect = '1' and avalon_waitrequest = '0' then
      -- read
      if avalon_read_n = '0' then
        -- read only when address < sdram-size; else return (others => '0')
        if unsigned(avalon_address) < SDRAM_SIMULATION_SIZE then
                                        -- byte enables
          for i in 0 to 3 loop
                                        -- read only selected data
            if avalon_byteenable_n(i) = '0' then
              readdata_pipeline_nxt(READPIPELINE_SIZE-1)(((i+1)*8)-1 downto i*8) <= sdram_data(to_integer(unsigned(avalon_address)))(((i+1)*8)-1 downto i*8);
            end if;

          end loop;
        end if;
        readdatavalid_pipeline_nxt <= '1' & readdatavalid_pipeline(READPIPELINE_SIZE-1 downto 1);
      end if;

      -- write
      if avalon_write_n = '0' then
        -- byte enables
        for i in 0 to 3 loop
                                        -- allow access only when address < sdram-size
          if unsigned(avalon_address) < SDRAM_SIMULATION_SIZE then
                                        -- change only selected data
            if avalon_byteenable_n(i) = '0' then
              sdram_data_nxt(to_integer(unsigned(avalon_address)))(((i+1)*8)-1 downto i*8) <= avalon_writedata(((i+1)*8)-1 downto i*8);
            end if;
          end if;
        end loop;

      end if;

    end if;
  end process;

end sim;

