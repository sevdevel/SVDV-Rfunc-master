##########################################################################################################
#  Examples to use plot functions of netCDF data
#  Written by: Sebastiaan van de velde
#  Contact: sebastiv@ucr.edu                 
##########################################################################################################

# ====================================================
# Source file with all functions
# ====================================================

source("plot.functions_v02.R")

# ====================================================
# EXAMPLE 1
# ====================================================

# ----------------------------------------------------
# call required input (file location etc.)
# ----------------------------------------------------

path.name       <- c("01 step - alternative worlds - only H2S/02 _varying biological carbon pump/cgenie_output")
experiment.name <- c("SVDV.ep_lo.10PALO2.tenthP.200efold")
var             <- "ocn_O2"
name            <- expression(paste("Surface O"['2']*" (",mu,"M)"))
factor          <- 1e+6
depth.slice     <- 1
var.lim         <- c(0,35)

# ----------------------------------------------------
# run plot functions
# ----------------------------------------------------

plot.lon.lat(path.name,experiment.name,var,name=name,factor=factor,depth.slice=depth.slice,save.plot=F,save.path=NULL,var.lim=var.lim)

# ====================================================
# EXAMPLE 2
# ====================================================

# ----------------------------------------------------
# call required input (file location etc.)
# ----------------------------------------------------

path.name       <- c("01 step - alternative worlds - only H2S/02 _varying biological carbon pump/cgenie_output")
experiment.name <- c("SVDV.ww_lo.10PALO2.tenthP.200efold")
var             <- "ocn_O2"
name            <- expression(paste("Zonal averaged O"['2']*" (",mu,"M)"))
factor          <- 1e+6

# ----------------------------------------------------
# run plot functions
# ----------------------------------------------------

plot.lat.depth(path.name,experiment.name,var,name=name,factor=factor,lon.slice=NULL,save.plot=F,save.path=NULL)

