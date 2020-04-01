-----------------------------------------------------------
--      Institute of Microelectronic Systems
--      Architectures and Systems
--      Leibniz Universitaet Hannover
-----------------------------------------------------------
--      lab :         Design Methods for FPGAs
--      file :        fpga_audiofx_Aufgabenteil_XV.vhdl
--      authors :     Christian Leibold
--      last update : 22.09.2015
--      description : Toplevel module for V8.XV
-----------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.fpga_audiofx_pkg.all;

entity fpga_audiofx is
  port (
    -- global
    clock_ext_50   : in    std_ulogic;
    reset_n_extern : in    std_ulogic;
    -- audio codec
    wm8731_clk     : out   std_ulogic;
    i2s_sclk       : in    std_ulogic;
    i2s_adc_ws     : in    std_ulogic;
    i2s_adc_sdat   : in    std_ulogic;
    i2s_dac_ws     : in    std_ulogic;
    i2s_dac_sdat   : out   std_ulogic;
    i2c_sdat       : inout std_logic;
    i2c_sclk       : out   std_logic;
    -- uart
    uart_rxd       : in    std_ulogic;
    uart_txd       : out   std_ulogic;
    -- logic analyzer pins for uart
    la_uart_rxd    : out   std_ulogic;
    la_uart_txd    : out   std_ulogic;
    -- sdram
    dram_clk       : out   std_ulogic;
    dram_cke       : out   std_ulogic;
    dram_dqm       : out   std_ulogic_vector(3 downto 0);
    dram_we_n      : out   std_ulogic;
    dram_cas_n     : out   std_ulogic;
    dram_ras_n     : out   std_ulogic;
    dram_cs_n      : out   std_ulogic;
    dram_ba        : out   std_ulogic_vector(1 downto 0);
    dram_addr      : out   std_ulogic_vector(12 downto 0);
    dram_data      : inout std_logic_vector (31 downto 0)
    );
end fpga_audiofx;

architecture rtl of fpga_audiofx is

  -- PLL Component
  component pll is
    port (
      areset : in  std_logic;
      inclk0 : in  std_logic;
      c0     : out std_logic;
      c1     : out std_logic;
      c2     : out std_logic;
      locked : out std_logic
      );
  end component pll;

  -- global signals
  signal clock_50        : std_ulogic;
  signal clock_audio_12  : std_ulogic;
  signal clock_sdram_100 : std_ulogic;
  signal reset_extern    : std_ulogic;
  signal reset_n         : std_ulogic;
  signal reset           : std_ulogic;

  -- I2S Slave Component
  component i2s_slave is
    port (
      clock           : in  std_ulogic;
      reset_n         : in  std_ulogic;
      i2s_sclk        : in  std_ulogic;
      i2s_adc_ws      : in  std_ulogic;
      i2s_adc_sdat    : in  std_ulogic;
      i2s_dac_ws      : in  std_ulogic;
      i2s_dac_sdat    : out std_ulogic;
      ain_left_sync   : out std_ulogic;
      ain_left_data   : out std_ulogic;
      ain_right_sync  : out std_ulogic;
      ain_right_data  : out std_ulogic;
      aout_left_sync  : in  std_ulogic;
      aout_left_data  : in  std_ulogic;
      aout_right_sync : in  std_ulogic;
      aout_right_data : in  std_ulogic
      );
  end component i2s_slave;

  -- Internal Audio Connection Signals
  signal ain_left_sync : std_ulogic;
  signal ain_left_data : std_ulogic;

  signal ain_right_sync : std_ulogic;
  signal ain_right_data : std_ulogic;

  signal aout_left_sync : std_ulogic;
  signal aout_left_data : std_ulogic;

  signal aout_right_sync : std_ulogic;
  signal aout_right_data : std_ulogic;

  -- I2C Master Component
  component i2c_master is
    port (
      clock         : in    std_ulogic;
      reset_n       : in    std_ulogic;
      i2c_clk       : out   std_logic;
      i2c_dat       : inout std_logic;
      busy          : out   std_ulogic;
      cs            : in    std_ulogic;
      mode          : in    std_ulogic_vector(1 downto 0);
      slave_addr    : in    std_ulogic_vector(6 downto 0);
      bytes_tx      : in    unsigned(4 downto 0);
      bytes_rx      : in    unsigned(4 downto 0);
      tx_data       : in    std_ulogic_vector(7 downto 0);
      tx_data_valid : in    std_ulogic;
      rx_data       : out   std_ulogic_vector(7 downto 0);
      rx_data_valid : out   std_ulogic;
      rx_data_en    : in    std_ulogic;
      error         : out   std_ulogic
      );
  end component i2c_master;

  -- WM8731 Configurator Component
  component wm8731_configurator is
    port(
      clock             : in  std_ulogic;
      reset_n           : in  std_ulogic;
      i2c_busy          : in  std_ulogic;
      i2c_cs            : out std_ulogic;
      i2c_mode          : out std_ulogic_vector(1 downto 0);
      i2c_slave_addr    : out std_ulogic_vector(6 downto 0);
      i2c_bytes_tx      : out unsigned(4 downto 0);
      i2c_bytes_rx      : out unsigned(4 downto 0);
      i2c_tx_data       : out std_ulogic_vector(7 downto 0);
      i2c_tx_data_valid : out std_ulogic;
      i2c_rx_data       : in  std_ulogic_vector(7 downto 0);
      i2c_rx_data_valid : in  std_ulogic;
      i2c_rx_data_en    : out std_ulogic;
      i2c_error         : in  std_ulogic;
      regif_cs          : in  std_ulogic;
      regif_wen         : in  std_ulogic;
      regif_addr        : in  std_ulogic_vector(REGIF_ADDR_WIDTH-1 downto 0);
      regif_data_in     : in  std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);
      regif_data_out    : out std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0)
      );
  end component wm8731_configurator;

  -- Connection Signals between the WM8731-Configurator and I2C-Master
  signal i2c_busy          : std_ulogic;
  signal i2c_cs            : std_ulogic;
  signal i2c_mode          : std_ulogic_vector(1 downto 0);
  signal i2c_slave_addr    : std_ulogic_vector(6 downto 0);
  signal i2c_bytes_tx      : unsigned(4 downto 0);
  signal i2c_bytes_rx      : unsigned(4 downto 0);
  signal i2c_tx_data       : std_ulogic_vector(7 downto 0);
  signal i2c_tx_data_valid : std_ulogic;
  signal i2c_rx_data       : std_ulogic_vector(7 downto 0);
  signal i2c_rx_data_valid : std_ulogic;
  signal i2c_rx_data_en    : std_ulogic;
  signal i2c_error         : std_ulogic;

  -- UART-Interface Component
  component uart_interface is
    port (
      clock          : in  std_ulogic;
      reset          : in  std_ulogic;
      rxd            : in  std_ulogic;
      txd            : out std_ulogic;
      regif_cs       : out std_ulogic_vector(REGIF_MODULE_RANGE-1 downto 0);
      regif_wen      : out std_ulogic;
      regif_addr     : out std_ulogic_vector(REGIF_ADDR_WIDTH-1 downto 0);
      regif_data_in  : out std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);
      regif_data_out : in  std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0)
      );
  end component uart_interface;

  signal regif_cs       : std_ulogic_vector(REGIF_MODULE_RANGE-1 downto 0);
  signal regif_wen      : std_ulogic;
  signal regif_addr     : std_ulogic_vector(REGIF_ADDR_WIDTH-1 downto 0);
  signal regif_data_in  : std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);
  signal regif_data_out : std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);

  signal regif_dout_ac : std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);
  signal regif_dout_du : std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);

  type regif_dout_gain_t is array (0 to 5) of std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);
  signal regif_dout_gain_left  : regif_dout_gain_t;
  signal regif_dout_gain_right : regif_dout_gain_t;

  -- SDRAM Interface Component
  component sdram_interface is
    generic (
      GW_ADDR : natural;
      GW_DATA : natural
      );
    port (
      clock               : in    std_ulogic;
      clock_sdram_ctrl    : in    std_ulogic;
      clock_sdram_chip    : in    std_ulogic;
      reset_n             : in    std_ulogic;
      sdram_select        : in    std_ulogic;
      sdram_write_en      : in    std_ulogic;
      sdram_address       : in    std_ulogic_vector(GW_ADDR-1 downto 0);
      sdram_data_in       : in    std_ulogic_vector(GW_DATA-1 downto 0);
      sdram_data_out      : out   std_ulogic_vector(GW_DATA-1 downto 0);
      sdram_request_en    : in    std_ulogic;
      sdram_req_slot_free : out   std_ulogic;
      sdram_data_avail    : out   std_ulogic;
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
  end component sdram_interface;

  signal sdram_select        : std_ulogic;
  signal sdram_write_en      : std_ulogic;
  signal sdram_address       : std_ulogic_vector(25 downto 0);
  signal sdram_data_in       : std_ulogic_vector(15 downto 0);
  signal sdram_data_out      : std_ulogic_vector(15 downto 0);
  signal sdram_request_en    : std_ulogic;
  signal sdram_req_slot_free : std_ulogic;
  signal sdram_data_avail    : std_ulogic;

  -- Delay-Unit Component
  component delay_unit is
    generic (
      BASE_ADDR_0        : natural := 16#00000000#;
      BASE_ADDR_1        : natural := 16#01000000#;
      BUFFER_SIZE        : natural := 16#01000000#;
      DELAY_SHIFT_FACTOR : natural := 10;
      OUTPORTS           : natural := 5
      );
    port (
      clock               : in  std_ulogic;
      reset               : in  std_ulogic;
      ain_sync_0          : in  std_ulogic;
      ain_data_0          : in  std_ulogic;
      ain_sync_1          : in  std_ulogic;
      ain_data_1          : in  std_ulogic;
      aout_sync_0         : out std_ulogic_vector(OUTPORTS-1 downto 0);
      aout_data_0         : out std_ulogic_vector(OUTPORTS-1 downto 0);
      aout_sync_1         : out std_ulogic_vector(OUTPORTS-1 downto 0);
      aout_data_1         : out std_ulogic_vector(OUTPORTS-1 downto 0);
      regif_cs            : in  std_ulogic;
      regif_wen           : in  std_ulogic;
      regif_addr          : in  std_ulogic_vector(REGIF_ADDR_WIDTH-1 downto 0);
      regif_data_in       : in  std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);
      regif_data_out      : out std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);
      sdram_select        : out std_ulogic;
      sdram_write_en      : out std_ulogic;
      sdram_address       : out std_ulogic_vector(25 downto 0);
      sdram_data_in       : in  std_ulogic_vector(15 downto 0);
      sdram_data_out      : out std_ulogic_vector(15 downto 0);
      sdram_request_en    : out std_ulogic;
      sdram_req_slot_free : in  std_ulogic;
      sdram_data_avail    : in  std_ulogic
      );
  end component delay_unit;

  -- Audio Output Signals Delay-Unit (du)
  signal aout_sync_du_0 : std_ulogic_vector(4 downto 0);
  signal aout_data_du_0 : std_ulogic_vector(4 downto 0);
  signal aout_sync_du_1 : std_ulogic_vector(4 downto 0);
  signal aout_data_du_1 : std_ulogic_vector(4 downto 0);

  -- Gain-Control Component
  component gain_control is
    generic (
      SHIFT_FACTOR : integer
      );
    port (
      clock          : in  std_ulogic;
      reset          : in  std_ulogic;
      ain_sync       : in  std_ulogic;
      ain_data       : in  std_ulogic;
      aout_sync      : out std_ulogic;
      aout_data      : out std_ulogic;
      regif_cs       : in  std_ulogic;
      regif_wen      : in  std_ulogic;
      regif_addr     : in  std_ulogic_vector(REGIF_ADDR_WIDTH-1 downto 0);
      regif_data_in  : in  std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0);
      regif_data_out : out std_ulogic_vector(REGIF_DATA_WIDTH-1 downto 0)
      );
  end component gain_control;

  -- Audio Input Signals Gain-Control (gain)
  signal ain_sync_gain_left  : std_ulogic_vector(5 downto 0);
  signal ain_data_gain_left  : std_ulogic_vector(5 downto 0);
  signal ain_sync_gain_right : std_ulogic_vector(5 downto 0);
  signal ain_data_gain_right : std_ulogic_vector(5 downto 0);

  -- Audio Output Signals Gain-Control (gain)
  signal aout_sync_gain_left  : std_ulogic_vector(5 downto 0);
  signal aout_data_gain_left  : std_ulogic_vector(5 downto 0);
  signal aout_sync_gain_right : std_ulogic_vector(5 downto 0);
  signal aout_data_gain_right : std_ulogic_vector(5 downto 0);

  -- Mixer-Unit Component
  component mixer_unit is
    port(
      clock     : in  std_ulogic;
      reset     : in  std_ulogic;
      ain_sync  : in  std_ulogic_vector(1 downto 0);
      ain_data  : in  std_ulogic_vector(1 downto 0);
      aout_sync : out std_ulogic;
      aout_data : out std_ulogic
      );
  end component mixer_unit;

  -- Mixer-Unit Connection Signals
  signal sync_left_inter  : std_ulogic_vector(7 downto 0);
  signal data_left_inter  : std_ulogic_vector(7 downto 0);
  signal sync_right_inter : std_ulogic_vector(7 downto 0);
  signal data_right_inter : std_ulogic_vector(7 downto 0);

  -- UART TXD Wire (required for logic analysis)
  signal uart_txd_wire : std_ulogic;

begin

  -- invert reset-extern signal
  reset_extern <= not reset_n_extern;

  -- PLL (clock_50, clock_audio_12, clock_sdram_100)
  pll_inst : pll
    port map(
      areset => reset_extern,
      inclk0 => clock_ext_50,
      c0     => clock_50,
      c1     => clock_audio_12,
      c2     => clock_sdram_100,
      locked => reset_n
      );

  -- invert reset-intern signal
  reset      <= not reset_n;
  -- assign 12MHz-clock to WM8731
  wm8731_clk <= clock_audio_12;

  -- WM8731 Configurator
  wm8731_configurator_inst : wm8731_configurator
    port map (
      clock             => clock_50,
      reset_n           => reset_n,
      i2c_busy          => i2c_busy,
      i2c_cs            => i2c_cs,
      i2c_mode          => i2c_mode,
      i2c_slave_addr    => i2c_slave_addr,
      i2c_bytes_tx      => i2c_bytes_tx,
      i2c_bytes_rx      => i2c_bytes_rx,
      i2c_tx_data       => i2c_tx_data,
      i2c_tx_data_valid => i2c_tx_data_valid,
      i2c_rx_data       => i2c_rx_data,
      i2c_rx_data_valid => i2c_rx_data_valid,
      i2c_rx_data_en    => i2c_rx_data_en,
      i2c_error         => i2c_error,
      regif_cs          => regif_cs(0),
      regif_wen         => regif_wen,
      regif_addr        => regif_addr,
      regif_data_in     => regif_data_in,
      regif_data_out    => regif_dout_ac
      );

  -- I2C Master
  i2c_master_inst : i2c_master
    port map (
      clock         => clock_50,
      reset_n       => reset_n,
      i2c_clk       => i2c_sclk,
      i2c_dat       => i2c_sdat,
      busy          => i2c_busy,
      cs            => i2c_cs,
      mode          => i2c_mode,
      slave_addr    => i2c_slave_addr,
      bytes_tx      => i2c_bytes_tx,
      bytes_rx      => i2c_bytes_rx,
      tx_data       => i2c_tx_data,
      tx_data_valid => i2c_tx_data_valid,
      rx_data       => i2c_rx_data,
      rx_data_valid => i2c_rx_data_valid,
      rx_data_en    => i2c_rx_data_en,
      error         => i2c_error
      );

  -- I2S Slave
  i2s_slave_inst : i2s_slave
    port map (
      clock           => clock_50,
      reset_n         => reset_n,
      i2s_sclk        => i2s_sclk,
      i2s_adc_ws      => i2s_adc_ws,
      i2s_adc_sdat    => i2s_adc_sdat,
      i2s_dac_ws      => i2s_dac_ws,
      i2s_dac_sdat    => i2s_dac_sdat,
      ain_left_sync   => ain_left_sync,
      ain_left_data   => ain_left_data,
      ain_right_sync  => ain_right_sync,
      ain_right_data  => ain_right_data,
      aout_left_sync  => aout_left_sync,
      aout_left_data  => aout_left_data,
      aout_right_sync => aout_right_sync,
      aout_right_data => aout_right_data
      );

  -- UART Interface     
  uart_interface_inst : uart_interface
    port map (
      clock          => clock_50,
      reset          => reset,
      rxd            => uart_rxd,
      txd            => uart_txd_wire,
      regif_cs       => regif_cs,
      regif_wen      => regif_wen,
      regif_addr     => regif_addr,
      regif_data_in  => regif_data_in,
      regif_data_out => regif_data_out
      );

  -- Connection of Register Interfaces
  regif_data_out <= regif_dout_ac
                    or regif_dout_du
                    or regif_dout_gain_left(0)
                    or regif_dout_gain_left(1)
                    or regif_dout_gain_left(2)
                    or regif_dout_gain_left(3)
                    or regif_dout_gain_left(4)
                    or regif_dout_gain_left(5)
                    or regif_dout_gain_right(0)
                    or regif_dout_gain_right(1)
                    or regif_dout_gain_right(2)
                    or regif_dout_gain_right(3)
                    or regif_dout_gain_right(4)
                    or regif_dout_gain_right(5);

  -- Logic Analysis Wires
  uart_txd    <= uart_txd_wire;
  la_uart_rxd <= uart_rxd;
  la_uart_txd <= uart_txd_wire;

  -- SDRAM Interface
  sdram_interface_inst : sdram_interface
    generic map (
      GW_ADDR => 26,
      GW_DATA => 16
      )
    port map (
      clock               => clock_50,
      clock_sdram_ctrl    => clock_sdram_100,
      clock_sdram_chip    => clock_sdram_100,
      reset_n             => reset_n,
      sdram_select        => sdram_select,
      sdram_write_en      => sdram_write_en,
      sdram_address       => sdram_address,
      sdram_data_in       => sdram_data_out,
      sdram_data_out      => sdram_data_in,
      sdram_request_en    => sdram_request_en,
      sdram_req_slot_free => sdram_req_slot_free,
      sdram_data_avail    => sdram_data_avail,
      dram_clk            => dram_clk,
      dram_cke            => dram_cke,
      dram_dqm            => dram_dqm,
      dram_we_n           => dram_we_n,
      dram_cas_n          => dram_cas_n,
      dram_ras_n          => dram_ras_n,
      dram_cs_n           => dram_cs_n,
      dram_ba             => dram_ba,
      dram_addr           => dram_addr,
      dram_data           => dram_data
      );

  -- Delay Unit
  delay_unit_inst : delay_unit
	generic map(
		BASE_ADDR_0        => 16#00000000#,
		BASE_ADDR_1        => 16#01000000#,
		BUFFER_SIZE        => 16#01000000#,
		DELAY_SHIFT_FACTOR => 10,							-- should be 10 => 2^10 = 1024 -> 5.92s
		OUTPORTS           => 5
	)
    port map (
      clock               => clock_50,
      reset               => reset,
      ain_sync_0          => ain_left_sync,
      ain_data_0          => ain_left_data,
      ain_sync_1          => ain_right_sync,
      ain_data_1          => ain_right_data,
      aout_sync_0         => aout_sync_du_0,
      aout_data_0         => aout_data_du_0,
      aout_sync_1         => aout_sync_du_1,
      aout_data_1         => aout_data_du_1,
      regif_cs            => regif_cs(1),
      regif_wen           => regif_wen,
      regif_addr          => regif_addr,
      regif_data_in       => regif_data_in,
      regif_data_out      => regif_dout_du,
      sdram_select        => sdram_select,
      sdram_write_en      => sdram_write_en,
      sdram_address       => sdram_address,
      sdram_data_in       => sdram_data_in,
      sdram_data_out      => sdram_data_out,
      sdram_request_en    => sdram_request_en,
      sdram_req_slot_free => sdram_req_slot_free,
      sdram_data_avail    => sdram_data_avail
      );

  -- Connect Delay Unit Outputs and original Audio Signals to Gain Controls
  ain_sync_gain_left  <= aout_sync_du_0 & ain_left_sync;
  ain_data_gain_left  <= aout_data_du_0 & ain_left_data;
  ain_sync_gain_right <= aout_sync_du_1 & ain_right_sync;
  ain_data_gain_right <= aout_data_du_1 & ain_right_data;

  -- Gain Controls
  gain_left_inst : gain_control
    generic map (
      SHIFT_FACTOR => 6
      )
    port map (
      clock          => clock_50,
      reset          => reset,
      ain_sync       => ain_sync_gain_left(0),
      ain_data       => ain_data_gain_left(0),
      aout_sync      => aout_sync_gain_left(0),
      aout_data      => aout_data_gain_left(0),
      regif_cs       => regif_cs(2),
      regif_wen      => regif_wen,
      regif_addr     => regif_addr,
      regif_data_in  => regif_data_in,
      regif_data_out => regif_dout_gain_left(0)
      );

  gain_right_inst : gain_control
    generic map (
      SHIFT_FACTOR => 6
      )
    port map (
      clock          => clock_50,
      reset          => reset,
      ain_sync       => ain_sync_gain_right(0),
      ain_data       => ain_data_gain_right(0),
      aout_sync      => aout_sync_gain_right(0),
      aout_data      => aout_data_gain_right(0),
      regif_cs       => regif_cs(13),
      regif_wen      => regif_wen,
      regif_addr     => regif_addr,
      regif_data_in  => regif_data_in,
      regif_data_out => regif_dout_gain_right(0)
      );


  gain_left_gen : for i in 1 to 5 generate
    gain_left_inst : gain_control
      generic map (
        SHIFT_FACTOR => 6
        )
      port map (
        clock          => clock_50,
        reset          => reset,
        ain_sync       => ain_sync_gain_left(i),
        ain_data       => ain_data_gain_left(i),
        aout_sync      => aout_sync_gain_left(i),
        aout_data      => aout_data_gain_left(i),
        regif_cs       => regif_cs(i+2),
        regif_wen      => regif_wen,
        regif_addr     => regif_addr,
        regif_data_in  => regif_data_in,
        regif_data_out => regif_dout_gain_left(i)
        );
  end generate gain_left_gen;

  gain_right_gen : for i in 1 to 5 generate
    gain_right_inst : gain_control
      generic map (
        SHIFT_FACTOR => 6
        )
      port map (
        clock          => clock_50,
        reset          => reset,
        ain_sync       => ain_sync_gain_right(i),
        ain_data       => ain_data_gain_right(i),
        aout_sync      => aout_sync_gain_right(i),
        aout_data      => aout_data_gain_right(i),
        regif_cs       => regif_cs(i+5+2),
        regif_wen      => regif_wen,
        regif_addr     => regif_addr,
        regif_data_in  => regif_data_in,
        regif_data_out => regif_dout_gain_right(i)
        );
  end generate gain_right_gen;

  -- Mixer Units
  mixer_unit_inst_gc2_3 : mixer_unit
    port map (
      clock     => clock_50,
      reset     => reset,
      ain_sync  => aout_sync_gain_left(1 downto 0),
      ain_data  => aout_data_gain_left(1 downto 0),
      aout_sync => sync_left_inter(0),
      aout_data => data_left_inter(0)
      );

  mixer_unit_inst_gc4_5 : mixer_unit
    port map (
      clock     => clock_50,
      reset     => reset,
      ain_sync  => aout_sync_gain_left(3 downto 2),
      ain_data  => aout_data_gain_left(3 downto 2),
      aout_sync => sync_left_inter(1),
      aout_data => data_left_inter(1)
      );

  mixer_unit_inst_gc6_7 : mixer_unit
    port map (
      clock     => clock_50,
      reset     => reset,
      ain_sync  => aout_sync_gain_left(5 downto 4),
      ain_data  => aout_data_gain_left(5 downto 4),
      aout_sync => sync_left_inter(2),
      aout_data => data_left_inter(2)
      );

  mixer_unit_inst_gc8_9 : mixer_unit
    port map (
      clock     => clock_50,
      reset     => reset,
      ain_sync  => aout_sync_gain_right(1 downto 0),
      ain_data  => aout_data_gain_right(1 downto 0),
      aout_sync => sync_right_inter(0),
      aout_data => data_right_inter(0)
      );

  mixer_unit_inst_gc10_11 : mixer_unit
    port map (
      clock     => clock_50,
      reset     => reset,
      ain_sync  => aout_sync_gain_right(3 downto 2),
      ain_data  => aout_data_gain_right(3 downto 2),
      aout_sync => sync_right_inter(1),
      aout_data => data_right_inter(1)
      );

  mixer_unit_inst_gc12_13 : mixer_unit
    port map (
      clock     => clock_50,
      reset     => reset,
      ain_sync  => aout_sync_gain_right(5 downto 4),
      ain_data  => aout_data_gain_right(5 downto 4),
      aout_sync => sync_right_inter(2),
      aout_data => data_right_inter(2)
      );

  sync_left_inter(5 downto 4) <= sync_left_inter(0) & sync_left_inter(1);
  data_left_inter(5 downto 4) <= data_left_inter(0) & data_left_inter(1);

  mixer_unit_inst_mx_left_0 : mixer_unit
    port map (
      clock     => clock_50,
      reset     => reset,
      ain_sync  => sync_left_inter(5 downto 4),
      ain_data  => data_left_inter(5 downto 4),
      aout_sync => sync_left_inter(3),
      aout_data => data_left_inter(3)
      );

  sync_left_inter(7 downto 6) <= sync_left_inter(3) & sync_left_inter(2);
  data_left_inter(7 downto 6) <= data_left_inter(3) & data_left_inter(2);

  mixer_unit_inst_mx_left_1 : mixer_unit
    port map (
      clock     => clock_50,
      reset     => reset,
      ain_sync  => sync_left_inter(7 downto 6),
      ain_data  => data_left_inter(7 downto 6),
      aout_sync => aout_left_sync,
      aout_data => aout_left_data
      );

  sync_right_inter(5 downto 4) <= sync_right_inter(0) & sync_right_inter(1);
  data_right_inter(5 downto 4) <= data_right_inter(0) & data_right_inter(1);

  mixer_unit_inst_mx_right_0 : mixer_unit
    port map (
      clock     => clock_50,
      reset     => reset,
      ain_sync  => sync_right_inter(5 downto 4),
      ain_data  => data_right_inter(5 downto 4),
      aout_sync => sync_right_inter(3),
      aout_data => data_right_inter(3)
      );

  sync_right_inter(7 downto 6) <= sync_right_inter(3) & sync_right_inter(2);
  data_right_inter(7 downto 6) <= data_right_inter(3) & data_right_inter(2);

  mixer_unit_inst_mx_right_1 : mixer_unit
    port map (
      clock     => clock_50,
      reset     => reset,
      ain_sync  => sync_right_inter(7 downto 6),
      ain_data  => data_right_inter(7 downto 6),
      aout_sync => aout_right_sync,
      aout_data => aout_right_data
      );

end architecture rtl;
