transcript on
if ![file isdirectory Debug_iputf_libs] {
	file mkdir Debug_iputf_libs
}

if {[file exists rtl_work]} {
	vdel -lib rtl_work -all
}
vlib rtl_work
vmap work rtl_work

###### Libraries for IPUTF cores 
###### End libraries for IPUTF cores 
###### MIF file copy and HDL compilation commands for IPUTF cores 


vlog "C:/Users/alanh/Documents/Github/Project_2/Project1_restored/Pll_sim/Pll.vo"

vlog -vlog01compat -work work +incdir+C:/Users/alanh/Documents/Github/Project_2/Project1_restored {C:/Users/alanh/Documents/Github/Project_2/Project1_restored/Project.v}
vlog -vlog01compat -work work +incdir+C:/Users/alanh/Documents/Github/Project_2/Project1_restored {C:/Users/alanh/Documents/Github/Project_2/Project1_restored/SevenSeg.v}

vlog -vlog01compat -work work +incdir+C:/Users/alanh/Documents/Github/Project_2/Project1_restored {C:/Users/alanh/Documents/Github/Project_2/Project1_restored/Project.v}

vsim -t 1ps -L altera_ver -L lpm_ver -L sgate_ver -L altera_mf_ver -L altera_lnsim_ver -L cyclonev_ver -L cyclonev_hssi_ver -L cyclonev_pcie_hip_ver -L rtl_work -L work -voptargs="+acc"  Project

add wave *
view structure
view signals
run -all
