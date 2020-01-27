#===========================================
# Parameter change function
#===========================================

CreateUserConfig <- function(temp.file,new.file,parm,newvalue){
  
  template <- as.character(read.delim(temp.file)[[1]])
  
  for (i in 1:length(parm)){
  x <- strsplit(template,parm[i])
  LineToChange <- template[lapply(x, length)==2] 
  LineToChangeTo <- paste(parm[i],"=",newvalue[i],sep="")
  
  LinePosition <- match(LineToChange, template, nomatch = NA_integer_, incomparables = NULL)
  template[LinePosition] <- LineToChangeTo 
  }
  
  write.table(template, new.file, sep="\t",row.names=FALSE,col.names=FALSE,quote=FALSE,na="") 
  
}

#==================================================
# Automated userconfig creation - full output
#==================================================

path.name <- "userconfigs"
files <- list.files(path=path.name,pattern="191209")

for (i in 1:length(files)){
  temp.file <- paste(path.name,files[i],sep="/")
  parm      <- "bg_par_data_save_level" 
  newvalue  <- "15"
  new.file  <- paste(files[i],"_fulloutput",sep="")
  
  CreateUserConfig(temp.file,new.file,parm,newvalue)
}