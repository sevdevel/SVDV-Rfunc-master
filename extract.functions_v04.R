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
require(dplyr)

#====================================================
# Aux functions
#====================================================
#----------------------------------------------------
# extract series.res file routing
#----------------------------------------------------

extract.results <- function(files,filenameextract=0){
  
  if(length(files>=1)){
    
    extract <- read.delim(paste(path.name,experiment_name,results,files[1],sep="/"),sep = "")
    extract <- extract %>% select_if(function(x){!all(is.na(x))})
    
    extract.name <- read.delim(paste(path.name,experiment_name,results,files[1],sep="/"),nrows=1)
    extract.name <- strsplit(names(extract.name),"....",fixed=T)[[1]]
    
    if(filenameextract==1){
      name <- strsplit(files[1],"_",fixed=T)[[1]]
      extract.name <- paste(name[5],extract.name)
    }
    if(filenameextract==2){
      name <- strsplit(files[1],"_",fixed=T)[[1]]
      extract.name <- paste(name[5],name[6],extract.name)
    }
    
    names(extract) <- c("time",extract.name[2:length(extract.name)])
    
    if(length(files>1)){
      for (i in 2:length(files)){
        
        extract2 <- read.delim(paste(path.name,experiment_name,results,files[i],sep="/"),sep = "")
        extract2 <- extract2 %>% select_if(function(x){!all(is.na(x))})
        extract2 <- extract2[,2:ncol(extract2)]
        
        extract.name2 <- read.delim(paste(path.name,experiment_name,results,files[i],sep="/"),nrows=1)
        extract.name2 <- strsplit(names(extract.name2),"....",fixed=T)[[1]]
        
        if(filenameextract==1){
          name2 <- strsplit(files[i],"_",fixed=T)[[1]]
          extract.name2 <- paste(name2[5],extract.name2)
        }
        if(filenameextract==2){
          name2 <- strsplit(files[i],"_",fixed=T)[[1]]
          extract.name2 <- paste(name2[5],name2[6],extract.name2)
        }
        
        j <- ncol(extract)
        extract <- cbind(extract,extract2)
        names(extract)[(j+1):ncol(extract)] <- extract.name2[2:length(extract.name2)]
        
      }}}
  else{extract <- NULL}
  
  return(extract)
  
}

#----------------------------------------------------
# extract numerical info
#----------------------------------------------------

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

generate.summary <- function(experiment_name,path.name,digits=5){
  
  # read in results file
  
  results <- c("biogem")
  if (digits==5){
    output <- readLines(paste(path.name,experiment_name,results,"biogem_year_09999_500_diag_GLOBAL_AVERAGE.res",sep="/"))
  }
  if (digits==6){
    output <- readLines(paste(path.name,experiment_name,results,"biogem_year_009999_500_diag_GLOBAL_AVERAGE.res",sep="/"))
  }
  if (digits==7){
    output <- readLines(paste(path.name,experiment_name,results,"biogem_year_1999999_500_diag_GLOBAL_AVERAGE.res",sep="/"))
  }
  
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
  
  #var <- "Atmospheric pH2S"
  #units <- "uatm"
  
  #summary <- rbind(summary,data.frame(extract.function(output=output,var=var,units=units)))
  
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

generate.series <- function(experiment_name,path.name,results="biogem"){
  
  # initialise list
  
  series <- list()
  
  #---------------------------------
  # Extract atmosphere time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*_atm_*")
  
  if(length(files>=1)){
    
    extract <- read.delim(paste(path.name,experiment_name,results,files[1],sep="/"),sep = "")
    extract <- extract %>% select_if(function(x){!all(is.na(x))})
    
    #names(extract) <- c("time",extract.name[2:length(extract.name)])
    names(extract) <- c("time","humidity")
    
    if(length(files>1)){
      for (i in 2:length(files)){
        
        extract2 <- read.delim(paste(path.name,experiment_name,results,files[i],sep="/"),sep = "")
        extract2 <- extract2 %>% select_if(function(x){!all(is.na(x))})
        extract2 <- extract2[,2:ncol(extract2)]
        
        extract.name2 <- read.delim(paste(path.name,experiment_name,results,files[i],sep="/"),nrows=1)
        extract.name2 <- strsplit(names(extract.name2),"....",fixed=T)[[1]]
        
        j <- ncol(extract)
        extract <- cbind(extract,extract2)
        names(extract)[(j+1):ncol(extract)] <- extract.name2[2:length(extract.name2)]
        
      }}}
  
  series$atm <- extract
  rm(extract)
  rm(extract2)
  rm(extract.name2)
  rm(files)

  #---------------------------------
  # Extract ocean time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_ocn_*")
  
  series$ocn <- extract.results(files)
  
  #---------------------------------
  # Extract sed time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_sed_*")
  
  series$sed <- extract.results(files)
  
  #---------------------------------
  # Extract sed-water flux time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_focnsed_*")
  
  series$focnsed <- extract.results(files)
  
  #---------------------------------
  # Extract ocn-atm flux time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_focnatm_*")
  
  series$focnatm <- extract.results(files,filenameextract=0)
  
  #---------------------------------
  # Extract sea-air flux time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_fseaair_*")
  
  series$fseaair <- extract.results(files,filenameextract=0)
  
  #---------------------------------
  # Extract sed-ocn flux time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_fsedocn_*")
  
  series$fsedocn <- extract.results(files,filenameextract=0)
  
  #---------------------------------
  # Extract precipitation rate time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_diag_precip_*")
  
  series$precip <- extract.results(files,filenameextract=1)
  
  #---------------------------------
  # Extract reaction rate time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_diag_react_*")
  
  series$react <- extract.results(files,filenameextract=2)
  
  #---------------------------------
  # Extract redox rate time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_diag_redox_*")
  
  series$redox <- extract.results(files,filenameextract=2)
  
  #---------------------------------
  # Extract reminD rate time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_diag_reminD_*")
  
  series$reminD <- extract.results(files,filenameextract=2)
  
  #---------------------------------
  # Extract reminP rate time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_diag_reminP_*")
  
  series$reminP <- extract.results(files,filenameextract=2)
  
  #---------------------------------
  # Extract weathering rate time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_diag_weather_*")
  
  series$weather <- extract.results(files,filenameextract=0)
  
  #---------------------------------
  # Extract export rate time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_fexport_*")
  
  series$fexport <- extract.results(files,filenameextract=0)
  
  #---------------------------------
  # Extract export rate time-series
  #---------------------------------
  
  files <- list.files(paste(path.name,experiment_name,results,sep="/"), pattern = "*series_fexport_*")
  
  series$fexport <- extract.results(files,filenameextract=0)
  
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
       xlim=range(as.numeric(series[,"time_yr"])),ylim=y.lim,type="b")
  
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