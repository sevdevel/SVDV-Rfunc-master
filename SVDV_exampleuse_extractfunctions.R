##########################################################################################################
#  Examples to use extract functions for cGeNIE output data
#  Written by: Sebastiaan van de velde
#  Contact: sebastiv@ucr.edu                 
##########################################################################################################

# ====================================================
# Source file with all functions
# ====================================================

source("extract.functions.R")

# ====================================================
# EXAMPLE 1
# ====================================================

# ----------------------------------------------------
# call required input (file location etc.)
# ----------------------------------------------------

path.name <- c("01 step - alternative worlds - only H2S/02 _varying biological carbon pump/cgenie_output")
experiment_name <- c("SVDV.ww_lo.10PALO2.tenthP.2000efold")
save.path <- c("01 step - alternative worlds - only H2S/02 summary files/")

# ----------------------------------------------------
# Run functions
# ----------------------------------------------------

# extract the global summary

global.summary <- generate.summary(experiment_name=experiment_name,path.name=path.name)

# extract the time series data

series         <- generate.series(experiment_name=experiment_name,path.name=path.name)

# ----------------------------------------------------
# Combine and save data
# ----------------------------------------------------

# Combine

summary <- list("global.summary"=global.summary,"series"=series)

# save as .Rdata file

save(summary,file=paste(save.path,experiment_name,"_summary.Rdata",sep=""))

# save global summary as Excel workbook

wb <- createWorkbook()
for (i in 1:length(names(global.summary))){
  addWorksheet(wb, names(global.summary)[i])
  writeDataTable(wb, i, global.summary[[i]])
}

saveWorkbook(wb, file = paste(save.path,experiment_name,"_summary.xlsx",sep=""), overwrite = TRUE)

# save series as Excel workbook

wb <- createWorkbook()

  addWorksheet(wb, "series")
  writeDataTable(wb, 1, series)

saveWorkbook(wb, file = paste(save.path,experiment_name,"_series.xlsx",sep=""), overwrite = TRUE)

# ----------------------------------------------------
# Plot selection time series output
# ----------------------------------------------------

win.graph(width=8*0.7,height=10*0.7,pointsize=12*0.7)
par(mfcol=c(5,2),oma=c(2,2,3,2))

#left column 

plot.timeseries(series=summary$series,selection="globalpO2_atm",x.axis=F,name=expression("p"['O2']*" atm"),factor=1)

plot.timeseries(series=summary$series,selection="global_O2_molkg.1",x.axis=F,name=expression("mean [O2]"['ocn']*" uM"),factor=1e+6)

plot.timeseries(series=summary$series,selection="benthic_O2_molkg.1",x.axis=F,name=expression("benthic [O2]"['ocn']*" uM"),factor=1e+6)

plot.timeseries(series=summary$series,selection="global_PO4_molkg.1",x.axis=F,name=expression("mean [PO4]"['ocn']*" uM"),factor=1e+6)

plot.timeseries(series=summary$series,selection="global_ALK_molkg.1",x.axis=T,name=expression("mean [ALK]"['ocn']*" uM"),factor=1e+6)

# right column

plot.timeseries(series=summary$series,selection="global_H2S_molkg.1",x.axis=F,name=expression("mean [H2S]"['ocn']*" uM"),factor=1e+6)

plot.timeseries(series=summary$series,selection="global_SO4_molkg.1",x.axis=F,name=expression("mean [SO4]"['ocn']*" mM"),factor=1e+3)

plot.timeseries(series=summary$series,selection="global_H2StoSO4_dH2S_molyr.1",x.axis=F,name=expression("CSO d"['H2S']*" Pmol yr-1"),factor=1e-15)

plot.timeseries(series=summary$series,selection="global_POCflux_molyr.1",x.axis=F,name=expression("total F"['POC']*" Pg C yr-1"),factor=(1/12*1e-15))

plot.timeseries(series=summary$series,selection="global_reminP_POC_dDIC_molyr.1",x.axis=T,name=expression("POC remin d"['DIC']*" Pg C yr-1"),factor=(1/12*1e-15))
