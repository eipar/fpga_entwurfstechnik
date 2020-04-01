-------------------------------------------------------------------------------------------
--      Institute of Microelectronic Systems
--      Architectures and Systems
--      Leibniz Universitaet Hannover
-------------------------------------------------------------------------------------------
--      lab :         Design Methods for FPGAs
--      file :        sdram_interface.vhdl
--      authors :     Jan Duerre
--      last update : 22.08.2014
--      description : This SDRAM-Interface supports different Bitwidth on a 32-Bit SDRAM.
--                    Different Bitwidth are implemented by virtually resizing the 
--                    address-space using byte-enables. The speed per word (32, 16 or 8 bit)
--                    is nearly identical. Since the word-size scales on identical timing, 
--                    the data-per-time-rate is worse on smaller words.
--                      Supported Modes:
--                        Bitwidth: 32, Memory Size: 2**25
--                        Bitwidth: 16, Memory Size: 2**26
--                        Bitwidth:  8, Memory Size: 2**27
-------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.fpga_audiofx_pkg.all;

entity sdram_interface is
  generic (
    GW_ADDR : natural := 25;
    GW_DATA : natural := 32
    );
  port (
    -- global signals
    clock               : in    std_ulogic;
    clock_sdram_ctrl    : in    std_ulogic;
    clock_sdram_chip    : in    std_ulogic;
    reset_n             : in    std_ulogic;
    -- bus interface
    sdram_select        : in    std_ulogic;
    sdram_write_en      : in    std_ulogic;
    sdram_address       : in    std_ulogic_vector(GW_ADDR-1 downto 0);
    sdram_data_in       : in    std_ulogic_vector(GW_DATA-1 downto 0);
    sdram_data_out      : out   std_ulogic_vector(GW_DATA-1 downto 0);
    sdram_request_en    : in    std_ulogic;
    sdram_req_slot_free : out   std_ulogic;
    sdram_data_avail    : out   std_ulogic;
    -- sdram connections
    dram_clk            : out   std_ulogic;
    dram_cke            : out   std_ulogic;
    dram_dqm            : out   std_ulogic_vector(3 downto 0);
    dram_we_n           : out   std_ulogic;
    dram_cas_n          : out   std_ulogic;
    dram_ras_n          : out   std_ulogic;
    dram_cs_n           : out   std_ulogic;
    dram_ba             : out   std_ulogic_vector(1 downto 0);
    dram_addr           : out   std_ulogic_vector(12 downto 0);
    dram_data           : inout std_logic_vector(31 downto 0)
    );
end sdram_interface;


architecture rtl of sdram_interface is

  constant CV_FIFO_DEPTH        : natural := 32;
  constant CW_REQUEST_FIFO_DATA : natural := 2**to_log2(GW_DATA+GW_ADDR+1);

  component fifo_sync_dc is
    generic (
      GV_DEPTH : natural;
      GW_ADDR  : natural;
      GW_DATA  : natural
      );
    port (
      aclr    : in  std_logic;
      data    : in  std_logic_vector(GW_DATA-1 downto 0);
      rdclk   : in  std_logic;
      rdreq   : in  std_logic;
      wrclk   : in  std_logic;
      wrreq   : in  std_logic;
      q       : out std_logic_vector(GW_DATA-1 downto 0);
      rdempty : out std_logic;
      rdfull  : out std_logic;
      rdusedw : out std_logic_vector(GW_ADDR-1 downto 0);
      wrempty : out std_logic;
      wrfull  : out std_logic;
      wrusedw : out std_logic_vector(GW_ADDR-1 downto 0)
      );
  end component fifo_sync_dc;

  signal dcfifo_wr_q_wire         : std_logic_vector(CW_REQUEST_FIFO_DATA-1 downto 0);
  signal dcfifo_wr_rdempty_wire   : std_logic;
  signal dcfifo_wr_wrfull_wire    : std_logic;
  signal dcfifo_rd_q_wire         : std_logic_vector(GW_DATA-1 downto 0);
  signal dcfifo_rd_rdempty_wire   : std_logic;
  signal dcfifo_rd_wrfillcnt_wire : std_logic_vector(to_log2(CV_FIFO_DEPTH)-1 downto 0);


  signal dcfifo_wr_reset      : std_ulogic;
  signal dcfifo_wr_data_in    : std_ulogic_vector(CW_REQUEST_FIFO_DATA-1 downto 0);
  signal dcfifo_wr_data_out   : std_ulogic_vector(CW_REQUEST_FIFO_DATA-1 downto 0);
  signal dcfifo_wr_read_clk   : std_ulogic;
  signal dcfifo_wr_read_req   : std_ulogic;
  signal dcfifo_wr_read_empty : std_ulogic;
  signal dcfifo_wr_write_clk  : std_ulogic;
  signal dcfifo_wr_write_req  : std_ulogic;
  signal dcfifo_wr_write_full : std_ulogic;


  signal dcfifo_rd_reset         : std_ulogic;
  signal dcfifo_rd_data_in       : std_ulogic_vector(GW_DATA-1 downto 0);
  signal dcfifo_rd_data_out      : std_ulogic_vector(GW_DATA-1 downto 0);
  signal dcfifo_rd_read_clk      : std_ulogic;
  signal dcfifo_rd_read_req      : std_ulogic;
  signal dcfifo_rd_read_empty    : std_ulogic;
  signal dcfifo_rd_write_clk     : std_ulogic;
  signal dcfifo_rd_write_req     : std_ulogic;
  signal dcfifo_rd_write_fillcnt : unsigned(to_log2(CV_FIFO_DEPTH)-1 downto 0);


  component sdram_ctrl is
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
  end component sdram_ctrl;

  signal sdram_addr_wire  : std_logic_vector(12 downto 0);
  signal sdram_ba_wire    : std_logic_vector(1 downto 0);
  signal sdram_cas_n_wire : std_logic;
  signal sdram_cke_wire   : std_logic;
  signal sdram_cs_n_wire  : std_logic;
  signal sdram_dqm_wire   : std_logic_vector(3 downto 0);
  signal sdram_ras_n_wire : std_logic;
  signal sdram_we_n_wire  : std_logic;

  signal sdram_controller_readdata_wire      : std_logic_vector(31 downto 0);
  signal sdram_controller_readdatavalid_wire : std_logic;
  signal sdram_controller_waitrequest_wire   : std_logic;

  signal sdram_controller_address       : std_ulogic_vector(24 downto 0);
  signal sdram_controller_byteenable_n  : std_ulogic_vector(3 downto 0);
  signal sdram_controller_chipselect    : std_ulogic;
  signal sdram_controller_writedata     : std_ulogic_vector(31 downto 0);
  signal sdram_controller_read_n        : std_ulogic;
  signal sdram_controller_write_n       : std_ulogic;
  signal sdram_controller_readdata      : std_ulogic_vector(31 downto 0);
  signal sdram_controller_readdatavalid : std_ulogic;
  signal sdram_controller_waitrequest   : std_ulogic;

  -- fifo to save byte-enable information
  component fifo is
    generic (
      DEPTH     : natural;
      WORDWIDTH : natural
      );
    port (
      clock    : in  std_ulogic;
      reset_n  : in  std_ulogic;
      write_en : in  std_ulogic;
      data_in  : in  std_ulogic_vector(WORDWIDTH-1 downto 0);
      read_en  : in  std_ulogic;
      data_out : out std_ulogic_vector(WORDWIDTH-1 downto 0);
      empty    : out std_ulogic;
      full     : out std_ulogic;
      fill_cnt : out unsigned(to_log2(DEPTH+1)-1 downto 0)
      );
  end component fifo;

  signal fifo_be_write_en   : std_ulogic;
  signal fifo_be16_data_in  : std_ulogic_vector(0 downto 0);
  signal fifo_be8_data_in   : std_ulogic_vector(1 downto 0);
  signal fifo_be_read_en    : std_ulogic;
  signal fifo_be16_data_out : std_ulogic_vector(0 downto 0);
  signal fifo_be8_data_out  : std_ulogic_vector(1 downto 0);

begin
  -- check for allowed bitsize and memory depth
  assert ((GW_DATA = 32 and GW_ADDR = 25) or
          (GW_DATA = 16 and GW_ADDR = 26) or
          (GW_DATA = 8 and GW_ADDR = 27)) report "[SDRAM] Error: Only the following modes are supported: Width 32, Depth 2^25; Width 16, Depth 2^26; Width 8, Depth 2^27" severity failure;

  dram_clk <= clock_sdram_chip;

  dcfifo_wr_reset     <= not reset_n;
  dcfifo_wr_read_clk  <= clock_sdram_ctrl;
  dcfifo_wr_write_clk <= clock;
  dcfifo_rd_reset     <= not reset_n;
  dcfifo_rd_read_clk  <= clock;
  dcfifo_rd_write_clk <= clock_sdram_ctrl;


  dcfifo_wr_data_out   <= std_ulogic_vector(dcfifo_wr_q_wire);
  dcfifo_wr_read_empty <= std_ulogic(dcfifo_wr_rdempty_wire);
  dcfifo_wr_write_full <= std_ulogic(dcfifo_wr_wrfull_wire);

  dcfifo_rd_data_out      <= std_ulogic_vector(dcfifo_rd_q_wire);
  dcfifo_rd_read_empty    <= std_ulogic(dcfifo_rd_rdempty_wire);
  dcfifo_rd_write_fillcnt <= unsigned(dcfifo_rd_wrfillcnt_wire);

  dcfifo_wr_inst : fifo_sync_dc
    generic map (
      GV_DEPTH => CV_FIFO_DEPTH,
      GW_ADDR  => to_log2(CV_FIFO_DEPTH),
      GW_DATA  => CW_REQUEST_FIFO_DATA
      )
    port map (
      aclr    => std_logic(dcfifo_wr_reset),
      data    => std_logic_vector(dcfifo_wr_data_in),
      rdclk   => std_logic(dcfifo_wr_read_clk),
      rdreq   => std_logic(dcfifo_wr_read_req),
      wrclk   => std_logic(dcfifo_wr_write_clk),
      wrreq   => std_logic(dcfifo_wr_write_req),
      q       => dcfifo_wr_q_wire,
      rdempty => dcfifo_wr_rdempty_wire,
      rdfull  => open,
      rdusedw => open,
      wrempty => open,
      wrfull  => dcfifo_wr_wrfull_wire,
      wrusedw => open
      );

  dcfifo_rd_inst : fifo_sync_dc
    generic map (
      GV_DEPTH => CV_FIFO_DEPTH,
      GW_ADDR  => to_log2(CV_FIFO_DEPTH),
      GW_DATA  => GW_DATA
      )
    port map (
      aclr    => std_logic(dcfifo_rd_reset),
      data    => std_logic_vector(dcfifo_rd_data_in),
      rdclk   => std_logic(dcfifo_rd_read_clk),
      rdreq   => std_logic(dcfifo_rd_read_req),
      wrclk   => std_logic(dcfifo_rd_write_clk),
      wrreq   => std_logic(dcfifo_rd_write_req),
      q       => dcfifo_rd_q_wire,
      rdempty => dcfifo_rd_rdempty_wire,
      rdfull  => open,
      rdusedw => open,
      wrempty => open,
      wrfull  => open,
      wrusedw => dcfifo_rd_wrfillcnt_wire
      );


  dram_addr  <= std_ulogic_vector(sdram_addr_wire);
  dram_ba    <= std_ulogic_vector(sdram_ba_wire);
  dram_cas_n <= std_ulogic(sdram_cas_n_wire);
  dram_cke   <= std_ulogic(sdram_cke_wire);
  dram_cs_n  <= std_ulogic(sdram_cs_n_wire);
  dram_dqm   <= std_ulogic_vector(sdram_dqm_wire);
  dram_ras_n <= std_ulogic(sdram_ras_n_wire);
  dram_we_n  <= std_ulogic(sdram_we_n_wire);

  sdram_controller_readdata      <= std_ulogic_vector(sdram_controller_readdata_wire);
  sdram_controller_readdatavalid <= std_ulogic(sdram_controller_readdatavalid_wire);
  sdram_controller_waitrequest   <= std_ulogic(sdram_controller_waitrequest_wire);

  sdram_ctrl_inst : sdram_ctrl
    port map (
      clk_clk                  => std_logic(clock_sdram_ctrl),
      reset_reset_n            => std_logic(reset_n),
      wrapper_wire_addr        => sdram_addr_wire,
      wrapper_wire_ba          => sdram_ba_wire,
      wrapper_wire_cas_n       => sdram_cas_n_wire,
      wrapper_wire_cke         => sdram_cke_wire,
      wrapper_wire_cs_n        => sdram_cs_n_wire,
      wrapper_wire_dq          => dram_data,
      wrapper_wire_dqm         => sdram_dqm_wire,
      wrapper_wire_ras_n       => sdram_ras_n_wire,
      wrapper_wire_we_n        => sdram_we_n_wire,
      wrapper_s1_address       => std_logic_vector(sdram_controller_address),
      wrapper_s1_byteenable_n  => std_logic_vector(sdram_controller_byteenable_n),
      wrapper_s1_chipselect    => std_logic(sdram_controller_chipselect),
      wrapper_s1_writedata     => std_logic_vector(sdram_controller_writedata),
      wrapper_s1_read_n        => std_logic(sdram_controller_read_n),
      wrapper_s1_write_n       => std_logic(sdram_controller_write_n),
      wrapper_s1_readdata      => sdram_controller_readdata_wire,
      wrapper_s1_readdatavalid => sdram_controller_readdatavalid_wire,
      wrapper_s1_waitrequest   => sdram_controller_waitrequest_wire
      );

  -- fifo to save byte-enable information (not required for 32 bit width)
  fifo_be_16_gen : if GW_DATA = 16 generate
    fifo_be : fifo
      generic map (
        DEPTH     => CV_FIFO_DEPTH,
        WORDWIDTH => 1
        )
      port map (
        clock    => clock_sdram_ctrl,
        reset_n  => reset_n,
        write_en => fifo_be_write_en,
        data_in  => fifo_be16_data_in,
        read_en  => fifo_be_read_en,
        data_out => fifo_be16_data_out,
        empty    => open,
        full     => open,
        fill_cnt => open
        );
  end generate fifo_be_16_gen;

  fifo_be_8_gen : if GW_DATA = 8 generate
    fifo_be : fifo
      generic map (
        DEPTH     => CV_FIFO_DEPTH,
        WORDWIDTH => 2
        )
      port map (
        clock    => clock_sdram_ctrl,
        reset_n  => reset_n,
        write_en => fifo_be_write_en,
        data_in  => fifo_be8_data_in,
        read_en  => fifo_be_read_en,
        data_out => fifo_be8_data_out,
        empty    => open,
        full     => open,
        fill_cnt => open
        );
  end generate fifo_be_8_gen;



  -- iobus interface logic: write to dcfifo_wr / read from dcfifo_rd (50 MHz domain)
  process(sdram_select, sdram_write_en, sdram_request_en, sdram_address, sdram_data_in, dcfifo_wr_write_full, dcfifo_rd_read_empty, dcfifo_rd_data_out)
  begin
    -- iobus signals
    sdram_data_out      <= (others => '0');
    sdram_req_slot_free <= not dcfifo_wr_write_full;
    sdram_data_avail    <= not dcfifo_rd_read_empty;
    -- fifo signals
    dcfifo_wr_write_req <= '0';
    dcfifo_wr_data_in   <= (others => '0');
    dcfifo_rd_read_req  <= '0';

    -- data & addr & wr-bit
    dcfifo_wr_data_in <= std_ulogic_vector(resize(unsigned(sdram_data_in & sdram_address(GW_ADDR-1 downto 0) & sdram_write_en), dcfifo_wr_data_in'length));

    -- chipselect
    if sdram_select = '1' then
      -- write
      if sdram_write_en = '1' then
        -- write enable to fifo
        dcfifo_wr_write_req <= '1';
      -- read
      else
        -- read request
        if sdram_request_en = '1' then
                                        -- write enable to fifo
          dcfifo_wr_write_req <= '1';
        -- collect data
        else
                                        -- read enable to fifo
          dcfifo_rd_read_req <= '1';
                                        -- data from fifo to output
          sdram_data_out     <= dcfifo_rd_data_out(sdram_data_out'range);
        end if;
      end if;
    end if;

  end process;


  -- arbiter logic: read from dcfifo_wr / write to dcfifo_rd (SDRAM clock domain)
  process(dcfifo_wr_data_out, dcfifo_wr_read_empty, dcfifo_rd_write_fillcnt, fifo_be16_data_out, fifo_be8_data_out, sdram_controller_readdata, sdram_controller_readdatavalid, sdram_controller_waitrequest)
  begin
    -- dc fifo signals
    dcfifo_wr_read_req            <= '0';
    dcfifo_rd_write_req           <= '0';
    dcfifo_rd_data_in             <= (others => '0');
    -- byte enable fifo signals
    fifo_be_write_en              <= '0';
    fifo_be_read_en               <= '0';
    fifo_be16_data_in             <= (others => '0');
    fifo_be8_data_in              <= (others => '0');
    -- controller signals
    sdram_controller_address      <= (others => '0');
    sdram_controller_chipselect   <= '1';
    sdram_controller_writedata    <= (others => '0');
    sdram_controller_read_n       <= '1';
    sdram_controller_write_n      <= '1';
    sdram_controller_byteenable_n <= "0000";

    -- write valid read data from controller to fifo
    if sdram_controller_readdatavalid = '1' then
      -- get byte enable information from fifo
      fifo_be_read_en     <= '1';
      -- write data to receive fifo
      dcfifo_rd_write_req <= '1';

      -- data fifo
      if GW_DATA = 32 then
        dcfifo_rd_data_in <= sdram_controller_readdata;

      elsif GW_DATA = 16 then

        case fifo_be16_data_out(0) is
          when '0'    => dcfifo_rd_data_in <= sdram_controller_readdata(15 downto 0);
          when '1'    => dcfifo_rd_data_in <= sdram_controller_readdata(31 downto 16);
          when others => dcfifo_rd_data_in <= (others => '0');
        end case;

      elsif GW_DATA = 8 then

        case fifo_be8_data_out(1 downto 0) is
          when "00"   => dcfifo_rd_data_in <= sdram_controller_readdata(7 downto 0);
          when "01"   => dcfifo_rd_data_in <= sdram_controller_readdata(15 downto 8);
          when "10"   => dcfifo_rd_data_in <= sdram_controller_readdata(23 downto 16);
          when "11"   => dcfifo_rd_data_in <= sdram_controller_readdata(31 downto 24);
          when others => dcfifo_rd_data_in <= (others => '0');
        end case;

      end if;

    end if;

    -- data in dcfifo_wr
    if dcfifo_wr_read_empty = '0' then
      -- read request (and enough space in read fifo)
      if ((dcfifo_wr_data_out(0) = '0') and (dcfifo_rd_write_fillcnt < (CV_FIFO_DEPTH-8))) then
        -- enable read
        sdram_controller_read_n <= '0';

        -- set byte enable & address
        if GW_DATA = 32 then

          sdram_controller_byteenable_n <= "0000";
          sdram_controller_address      <= dcfifo_wr_data_out(GW_ADDR downto 1);

        elsif GW_DATA = 16 then

          case dcfifo_wr_data_out(1) is
            when '0'    => sdram_controller_byteenable_n <= "1100";
            when '1'    => sdram_controller_byteenable_n <= "0011";
            when others => sdram_controller_byteenable_n <= "0000";
          end case;

          fifo_be16_data_in(0)     <= dcfifo_wr_data_out(1);
          sdram_controller_address <= dcfifo_wr_data_out(GW_ADDR downto 2);

        elsif GW_DATA = 8 then

          case dcfifo_wr_data_out(2 downto 1) is
            when "00"   => sdram_controller_byteenable_n <= "1110";
            when "01"   => sdram_controller_byteenable_n <= "1101";
            when "10"   => sdram_controller_byteenable_n <= "1011";
            when "11"   => sdram_controller_byteenable_n <= "0111";
            when others => sdram_controller_byteenable_n <= "0000";
          end case;

          fifo_be8_data_in(1 downto 0) <= dcfifo_wr_data_out(2 downto 1);
          sdram_controller_address     <= dcfifo_wr_data_out(GW_ADDR downto 3);

        end if;

        -- check if controller is ready
        if sdram_controller_waitrequest = '0' then
                                        -- enable read from request fifo
          dcfifo_wr_read_req <= '1';
                                        -- write byte enable information to byte-enable fifo
          fifo_be_write_en   <= '1';
        end if;

      -- write request
      elsif dcfifo_wr_data_out(0) = '1' then
        -- write enable
        sdram_controller_write_n <= '0';

        -- set byte enable & address & data
        if GW_DATA = 32 then

          sdram_controller_byteenable_n <= "0000";
          sdram_controller_address      <= dcfifo_wr_data_out(GW_ADDR downto 1);
          sdram_controller_writedata    <= dcfifo_wr_data_out(GW_DATA+GW_ADDR downto GW_ADDR+1);

        elsif GW_DATA = 16 then

          case dcfifo_wr_data_out(1) is
            when '0' =>
              sdram_controller_byteenable_n           <= "1100";
              sdram_controller_writedata(15 downto 0) <= dcfifo_wr_data_out(GW_DATA+GW_ADDR downto GW_ADDR+1);
            when '1' =>
              sdram_controller_byteenable_n            <= "0011";
              sdram_controller_writedata(31 downto 16) <= dcfifo_wr_data_out(GW_DATA+GW_ADDR downto GW_ADDR+1);
            when others =>
              sdram_controller_byteenable_n <= "0000";
              sdram_controller_writedata    <= (others => '0');
          end case;

          sdram_controller_address <= dcfifo_wr_data_out(GW_ADDR downto 2);

        elsif GW_DATA = 8 then

          case dcfifo_wr_data_out(2 downto 1) is
            when "00" =>
              sdram_controller_byteenable_n          <= "1110";
              sdram_controller_writedata(7 downto 0) <= dcfifo_wr_data_out(GW_DATA+GW_ADDR downto GW_ADDR+1);
            when "01" =>
              sdram_controller_byteenable_n           <= "1101";
              sdram_controller_writedata(15 downto 8) <= dcfifo_wr_data_out(GW_DATA+GW_ADDR downto GW_ADDR+1);
            when "10" =>
              sdram_controller_byteenable_n            <= "1011";
              sdram_controller_writedata(23 downto 16) <= dcfifo_wr_data_out(GW_DATA+GW_ADDR downto GW_ADDR+1);
            when "11" =>
              sdram_controller_byteenable_n            <= "0111";
              sdram_controller_writedata(31 downto 24) <= dcfifo_wr_data_out(GW_DATA+GW_ADDR downto GW_ADDR+1);
            when others =>
              sdram_controller_byteenable_n <= "0000";
              sdram_controller_writedata    <= (others => '0');
          end case;

          sdram_controller_address <= dcfifo_wr_data_out(GW_ADDR downto 3);

        end if;

        -- check if controller is ready
        if sdram_controller_waitrequest = '0' then
                                        -- enable read from fifo
          dcfifo_wr_read_req <= '1';
        end if;
      end if;
    end if;

  end process;

end rtl;
