##########################################################################################################
#  Functions to export data from standard cGeNIE output 
#  Written by: Sebastiaan van de velde
#  Contact: sebastiv@ucr.edu                 
##########################################################################################################

# ====================================================
# load required packages
# ====================================================

require(stringr)
require(openxlsx)

# ====================================================
# Aux functions
# ====================================================

# ----------------------------------------------------
# extract numerical info
# ----------------------------------------------------

extract.function <- function(output,var,new.var.name=NULL,units="",integrate=F,int.units=NA){
  
  # Find line that contains variable of interest
  extr    <- grep(var,output,value=TRUE)
  # Split line to value
  extr    <- strsplit(extr,":")[[1]][2]
  
  # Split line at first text character (either E+ or units)
  # and select part before text
  beforeE <- strsplit(extr,"[a-z|A-Z|o]")[[1]][1]
  beforeE <- as.numeric(beforeE)
  
  # Split line at first text character (either E+ or units)
  # and select part after text (either power value, or nothing)
  afterE  <- strsplit(extr,"[a-z|A-Z|o]")[[1]][2]
  afterE  <- strsplit(afterE, " ")[[1]][1]
  afterE  <- as.numeric(afterE)
  # If no value found -> no power of ten, return 0
  if (is.na(afterE)){afterE <- 0}
  
  # reassemble value
  total.number <- beforeE*10^afterE
  
  # if a globally integrated value has been given, extract that value too
  if (integrate==T){
    
    intval <- strsplit(extr,"<->")[[1]][2]
    beforeE <- strsplit(intval,"[a-z|A-Z|o]")[[1]][1]
    beforeE <- as.numeric(beforeE)
    
    afterE  <- strsplit(intval,"[a-z|A-Z|o]")[[1]][2]
    afterE  <- strsplit(afterE, " ")[[1]][1]
    afterE  <- as.numeric(afterE)
    
    int.number <- beforeE*10^afterE
  } else {int.number=NA}
  
  if (!is.null(new.var.name)){var <- new.var.name}
  # write line for summary dataframe
  summary <- data.frame("variable"=var,"value"=total.number,"units"=units,"int.value"=int.number,"int.units"=int.units)
  
  # return output
  return(summary)
}

# ====================================================
# generate summary list
# ====================================================

generate.summary <- function(experiment_name,path.name){
  
  # read in results file
  
  results <- c("biogem")
  output <- readLines(paste(path.name,experiment_name,results,"biogem_year_09999_500_diag_GLOBAL_AVERAGE.res",sep="/"))
  
  # initialise list
  
  Summary <- list()
  
  # ----------------------------------------------------
  # Read general info
  # ----------------------------------------------------
  
  # read output year
  
  var <- "Year"
  units <- "yr"
  
  summary <- extract.function(output=output,var=var,units=units)
  
  # read Integration interval
  
  var <- "Integration interval"
  units <- "yr"
  
  summary <- rbind(summary,extract.function(output=output,var=var,units=units))
  
  # store in complete list
  
  Summary$General <- summary
  rm(summary)
  
  # ----------------------------------------------------
  # Read miscellaneous properties
  # ----------------------------------------------------
  
  # read global surface area ocean
  
  var <- "Global surface area"
  units <- "m2"
  
  summary <- extract.function(output=output,var=var,units=units)
  
  # read global ocean volume
  
  var <- "Global ocean volume"
  units <- "m3"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  # read Global mean air-sea coefficient
  
  var <- "Global mean air-sea coefficient"
  units <- "mol m-2 yr-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  # store in complete list
  
  Summary$miscellaneous <- summary
  rm(summary)
  
  # ----------------------------------------------------
  # Read atmospheric properties
  # ----------------------------------------------------
  
  # read Atmospheric pCO2
  
  var <- "Atmospheric pCO2"
  units <- "uatm"
  
  summary <- extract.function(output=output,var=var,units=units)
  
  #read Atmospheric pCO2_13C
  
  var <- "Atmospheric pCO2_13C"
  units <- "o/oo"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  #read Atmospheric pO2
  
  var <- "Atmospheric pO2"
  units <- "uatm"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  #read Atmospheric pH2S
  
  var <- "Atmospheric pH2S"
  units <- "uatm"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  # store in complete list
  
  Summary$atmosphere <- summary
  rm(summary)
  
  # ----------------------------------------------------
  # Read bulk ocean properties
  # ----------------------------------------------------
  
  #---
  var <- "Ocean temp"
  units <- "deg C"
  
  summary <- extract.function(output=output,var=var,units=units)
  
  #---
  var <- "Ocean sal"
  units <- "-"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  #---
  var <- "Ocean DIC "
  units <- "umol kg-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,new.var.name="Ocean DIC",units=units,integrate=T,int.units="mol")))
  
  #---
  var <- "Ocean DIC_13C"
  units <- "o/oo"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  #---
  var <- "Ocean PO4"
  units <- "umol kg-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units,integrate=T,int.units="mol")))
  
  #---
  var <- "Ocean O2"
  units <- "umol kg-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units,integrate=T,int.units="mol")))
  
  #---
  var <- "Ocean ALK"
  units <- "umol kg-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units,integrate=T,int.units="mol")))
  
  #---
  var <- "Ocean DOM_C "
  units <- "umol kg-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,new.var.name="Ocean DOM_C",units=units,integrate=T,int.units="mol")))
  
  #---
  var <- "Ocean DOM_C_13C"
  units <- "o/oo"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  #---
  var <- "Ocean DOM_P"
  units <- "umol kg-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units,integrate=T,int.units="mol")))
  
  #---
  var <- "Ocean Ca"
  units <- "umol kg-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units,integrate=T,int.units="mol")))
  
  #---
  var <- "Ocean SO4"
  units <- "umol kg-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units,integrate=T)))
  
  #---
  var <- "Ocean H2S"
  units <- "umol kg-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units,integrate=T)))
  
  #---
  var <- "Ocean Mg"
  units <- "umol kg-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units,integrate=T)))
  
  # store in complete list
  
  Summary$bulk.ocean <- summary
  rm(summary)
  
  # ----------------------------------------------------
  # Read carbonate ocean properties
  # ----------------------------------------------------
  
  #---
  var <- "Carb chem conc_CO2"
  units <- "umol kg-1"
  
  summary <- extract.function(output=output,var=var,units=units,integrate=T,int.units="mol")
  
  #---
  var <- "Carb chem conc_CO3"
  units <- "umol kg-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units,integrate=T,int.units="mol")))
  
  #---
  var <- "Carb chem conc_HCO3"
  units <- "umol kg-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units,integrate=T,int.units="mol")))
  
  # store in complete list
  
  Summary$carb.ocean <- summary
  rm(summary)
  
  # ----------------------------------------------------
  # Read surface export properties
  # ----------------------------------------------------
  
  #---
  var <- "Export flux POC "
  units <- "umol cm-2 yr-1"
  
  summary <- extract.function(output=output,var=var,new.var.name="Export flux POC",units=units,integrate=T,int.units="mol yr-1")
  
  #---
  var <- "Export flux POC_13C"
  units <- "o/oo"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  #---
  var <- "Export flux CaCO3 "
  units <- "umol cm-2 yr-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,new.var.name="Export flux CaCO3",units=units,integrate=T,int.units="mol yr-1")))
  
  #---
  var <- "Export flux CaCO3_13C"
  units <- "o/oo"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  #---
  var <- "Export flux det"
  units <- "o/oo"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  #---
  var <- "Export flux ash"
  units <- "o/oo"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  # store in complete list
  
  Summary$surface.export <- summary
  rm(summary)
  
  # ----------------------------------------------------
  # Read sedimentation properties
  # ----------------------------------------------------
  
  #---
  var <- "Export flux POC "
  units <- "umol cm-2 yr-1"
  
  summary <- extract.function(output=output,var=var,new.var.name="Export flux POC",units=units,integrate=T,int.units="mol yr-1")
  
  #---
  var <- "Export flux POC_13C"
  units <- "o/oo"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  #---
  var <- "Export flux CaCO3 "
  units <- "umol cm-2 yr-1"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,new.var.name="Export flux CaCO3",units=units,integrate=T,int.units="mol yr-1")))
  
  #---
  var <- "Export flux CaCO3_13C"
  units <- "o/oo"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  #---
  var <- "Export flux det"
  units <- "o/oo"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  #---
  var <- "Export flux ash"
  units <- "o/oo"
  
  summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
  # store in complete list
  
  Summary$sedimentation <- summary
  rm(summary)
  
  # ====================================================
  # return list
  # ====================================================
  
  return(Summary)
  
}

# ====================================================
# generate series list
# ====================================================

generate.series <- function(experiment_name,path.name){
  
  results <- c("biogem")
  
  # initialise list
  
  series <- list()
  
  # ----------------------------------------------------
  # Read-in relevant series
  # ----------------------------------------------------
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_atm_pCO2.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","globalpCO2_mol","globalpCO2_atm"),stringsAsFactors = F)
  
  series <- output 
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_atm_pCO2_13C.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","globalpCO2_13C_mol","globalpCO2_13C_ppm"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_atm_pH2S.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","globalpH2S_mol","globalpH2S_atm"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_atm_pO2.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","globalpO2_mol","globalpO2_atm"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_atm_temp.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","globaltemp_degC"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_carb_sur_conc_CO2.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","sur_conc_CO2_molkg-1","sur_conc_CO2_d13C_ppm"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_carb_sur_conc_CO3.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","sur_conc_CO3_molkg-1","sur_conc_CO3_d13C_ppm"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_carb_sur_conc_HCO3.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","sur_conc_HCO3_molkg-1","sur_conc_HCO3_d13C_ppm"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_carb_sur_dCO3_arg.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","sur_dCO3_arg_molkg-1"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_carb_sur_dCO3_cal.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","sur_dCO3_cal_molkg-1"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_carb_sur_fug_CO2.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","sur_fuc_CO2_atm-1"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_carb_sur_H.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","sur_H_molkg-1"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_carb_sur_ohm_arg.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","sat_arg"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_carb_sur_ohm_cal.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","sat_cal"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_carb_sur_pHsws.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","sur_pHsws_molkg-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_carb_sur_RF0.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","sur_RF0_molkg-1"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_bio_CaCO3toPOC_nsp.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","CaCO3toPOC"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_bio_DOMfrac.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","DOMfrac"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_bio_dPO4.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","dPO4_molyr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_bio_dPO4_1.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","dPO4_1_mol yr-1"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_bio_dPO4_2.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","dPO4_2_mol yr-1"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_bio_fspPOC.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","fspPOC"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  #output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_bio_k_Fe.res",sep="/"),sep="",skip=1,header=F,
  #                    col.names=c("time_yr","k_Fe"),stringsAsFactors = F)
  
  #series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_H2StoSO4_dALK.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_H2StoSO4_dALK_molyr-1","mean_H2StoSO4_dALK_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_H2StoSO4_dH2S.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_H2StoSO4_dH2S_molyr-1","mean_H2StoSO4_dH2S_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_H2StoSO4_dO2.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_H2StoSO4_dO2_molyr-1","mean_H2StoSO4_dO2_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_H2StoSO4_dSO4.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_H2StoSO4_dSO4_molyr-1","mean_H2StoSO4_dSO4_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminD_POC_13C_dDIC_13C.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminD_POC_13C_dDIC_13C_molyr-1","mean_reminD_POC_13C_dDIC_13C_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminD_POC_dALK.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminD_POC_dALK_molyr-1","mean_reminD_POC_dALK_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminD_POC_dDIC.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminD_POC_dDIC_molyr-1","mean_reminD_POC_dDIC_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminD_POC_dH2S.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminD_POC_dH2S_molyr-1","mean_reminD_POC_dH2S_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminD_POC_dO2.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminD_POC_dO2_molyr-1","mean_reminD_POC_dO2_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminD_POC_dSO4.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminD_POC_dSO4_molyr-1","mean_reminD_POC_dSO4_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminD_POP_dALK.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminD_POP_dALK_molyr-1","mean_reminD_POP_dALK_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminD_POP_dH2S.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminD_POP_dH2S_molyr-1","mean_reminD_POP_dH2S_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminD_POP_dO2.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminD_POP_dO2_molyr-1","mean_reminD_POP_dO2_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminD_POP_dPO4.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminD_POP_dPO4_molyr-1","mean_reminD_POP_dPO4_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminD_POP_dSO4.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminD_POP_dSO4_molyr-1","mean_reminD_POP_dSO4_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminP_POC_13C_dDIC_13C.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminP_POC_13C_dDIC_13C_molyr-1","mean_reminP_POC_13C_dDIC_13C_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminP_POC_dALK.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminP_POC_dALK_molyr-1","mean_reminP_POC_dALK_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminP_POC_dDIC.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminP_POC_dDIC_molyr-1","mean_reminP_POC_dDIC_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminP_POC_dH2S.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminP_POC_dH2S_molyr-1","mean_reminP_POC_dH2S_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminP_POC_dO2.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminP_POC_dO2_molyr-1","mean_reminP_POC_dO2_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminP_POC_dSO4.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminP_POC_dSO4_molyr-1","mean_reminP_POC_dSO4_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminP_POP_dALK.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminP_POP_dALK_molyr-1","mean_reminP_POP_dALK_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminP_POP_dH2S.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminP_POP_dH2S_molyr-1","mean_reminP_POP_dH2S_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminP_POP_dO2.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminP_POP_dO2_molyr-1","mean_reminP_POP_dO2_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminP_POP_dPO4.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminP_POP_dPO4_molyr-1","mean_reminP_POP_dPO4_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_diag_redox_reminP_POP_dSO4.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_reminP_POP_dSO4_molyr-1","mean_reminP_POP_dSO4_molkg-1yr-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])

  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_fexport_POC.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_POCflux_molyr-1","global_POCdens_molm-2yr-1","global_POC_DOM_fraction"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_fexport_POP.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_POPflux_molyr-1","global_POPdens_molm-2yr-1","global_POP_DOM_fraction"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_ALK.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_ALK_mol","global_ALK_molkg-1","surface_ALK_molkg-1","benthic_ALK_molkg-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_Ca.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_Ca_mol","global_Ca_molkg-1","surface_Ca_molkg-1","benthic_Ca_molkg-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_DIC.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_DIC_mol","global_DIC_molkg-1","surface_DIC_molkg-1","benthic_DIC_molkg-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_DIC_13C.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_DIC_13C_mol","global_DIC_13C_molkg-1","surface_DIC_13C_molkg-1","benthic_DIC_13C_molkg-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_DOM_C.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_DOM_C_mol","global_DOM_C_molkg-1","surface_DOM_C_molkg-1","benthic_DOM_C_molkg-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_DOM_C_13C.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_DOM_C_13C_mol","global_DOM_C_13C_molkg-1","surface_DOM_C_13C_molkg-1","benthic_DOM_C_13C_molkg-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_DOM_P.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_DOM_P_mol","global_DOM_P_molkg-1","surface_DOM_P_molkg-1","benthic_DOM_P_molkg-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_H2S.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_H2S_mol","global_H2S_molkg-1","surface_H2S_molkg-1","benthic_H2S_molkg-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_Mg.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_Mg_mol","global_Mg_molkg-1","surface_Mg_molkg-1","benthic_Mg_molkg-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_O2.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_O2_mol","global_O2_molkg-1","surface_O2_molkg-1","benthic_O2_molkg-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_PO4.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_PO4_mol","global_PO4_molkg-1","surface_PO4_molkg-1","benthic_PO4_molkg-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_sal.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_sal_ppm","surface_sal_ppm","benthic_sal_ppm"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_SO4.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_SO4_mol","global_SO4_molkg-1","surface_SO4_molkg-1","benthic_SO4_molkg-1"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  output <- read.csv2(paste(path.name,experiment_name,results,"biogem_series_ocn_temp.res",sep="/"),sep="",skip=1,header=F,
                      col.names=c("time_yr","global_temp_degC","surface_temp_degC","benthic_temp_degC"),stringsAsFactors = F)
  
  series <- cbind(series,output[2:ncol(output)])
  
  # ====================================================
  # return list
  # ====================================================
  
  return(series)
  
}

# ====================================================
# plot timeseries
# ====================================================

plot.timeseries <- function(series,selection,y.lim=NULL,x.axis=T,lett=NULL,name=NULL,factor=1){
  
  if (is.null(y.lim)){
    y.min <- min(as.numeric(series[,selection]))*factor
    if (y.min<0){y.min <- y.min*1.01}else{y.min <- y.min*0.99}
    
    y.max <- max(as.numeric(series[,selection]))*factor
    if (y.max<0){y.max <- y.max*0.99}else{y.max <- y.max*1.01}
    
    y.lim <- c(y.min,y.max)
  }
  if (is.null(name)){
    name <- selection
  }
  
  par(mar=c(2,3,1,2))
  
  plot(x=series[,"time_yr"], y=as.numeric(series[,selection])*factor,pch=16,col="black",axes=F,xlab="",ylab="",
       xlim=c(0,10000),ylim=y.lim,type="b")
  
  axis(pos=par()$xaxp[1], side=2,lwd=1,cex.axis = 1.3)
  abline(h=0, lwd=1, lty=2)
  mtext(side=2, text=name,line=1.5,adj=0.5,padj=-0.5,cex = 1.1)  
  if (x.axis == TRUE){
    axis(pos=par()$yaxp[1], side=1,lwd=1,cex.axis = 1.3)
    mtext(side=1, text="time (yr)",line=3.5,adj=0.5,padj=-1,cex = 1.2) 
  }
  
  if (!is.null(lett)){
    mtext(text=lett,line=-2,side=3,adj=0.15,padj=-1,cex = 1.3,las=1,outer=F)}
}