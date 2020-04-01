transcript on
if {[file exists rtl_work]} {
	vdel -lib rtl_work -all
}
vlib rtl_work
vmap work rtl_work

vlog -vlog01compat -work work +incdir+C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/vhdl {C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/vhdl/altera_reset_controller.v}
vlog -vlog01compat -work work +incdir+C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/vhdl {C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/vhdl/altera_reset_synchronizer.v}
vlog -vlog01compat -work work +incdir+C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/vhdl {C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/vhdl/sdram_ctrl_wrapper.v}
vlog -vlog01compat -work work +incdir+C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/quartus/db {C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/quartus/db/pll_altpll.v}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch04/vhdl/pll.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch04/vhdl/fpga_audiofx_pkg.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/vhdl/fifo_sync_dc.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/vhdl/sdram_ctrl.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch06/vhdl/s2p_unit.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/vhdl/delay_unit.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch04/vhdl/wm8731_configurator.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch04/vhdl/i2s_slave.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch04/vhdl/i2c_master.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch04/vhdl/fifo.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch04/vhdl/clock_generator.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch05/vhdl/p2s_unit.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch07/vhdl/uart_interface.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch07/vhdl/uart_regif_converter.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch07/vhdl/uart_transceiver.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/vhdl/sdram_interface.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/vhdl/fpga_audiofx.vhdl}

vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/quartus/../testbench/fpga_audiofx_tb.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/quartus/../testbench/sdram_ctrl_func_model.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/quartus/../../Versuch07/testbench/uart_tester.vhdl}
vcom -93 -work work {C:/Users/Euge/Desktop/FPGA-Labor/Versuch08/quartus/../../Versuch04/testbench/acodec_model.vhdl}

vsim -t 1ps -L altera -L lpm -L sgate -L altera_mf -L altera_lnsim -L cycloneive -L rtl_work -L work -voptargs="+acc"  fpga_audiofx_tb

add wave *
view structure
view signals
run -all
