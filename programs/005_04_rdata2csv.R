# Code for transforming .RDsata files into csv for further animation
# Vitor Costa 
# March 2019

options(scipen=999)

# Loading config file
source("config.R", echo = T)

# Loading packages
library(data.table)

setwd(analysis.dir)

# Turning .RData files of the one per cent sample to .csv
list.onepc = list.files(pattern="onepc.RData")
list.qob = list.onepc[grep("qob",list.onepc)]
list.qob = list.qob[grep("skill",list.qob)]
for(i in list.qob){
  cat(i,"\n \n")
  load(i)
  q["nreg"]=NULL
  setDT(q)
  file.name = gsub(".RData",".csv",i)
  fwrite(q,file.name,row.names=F)
}

# Now, dealing with debug files with SEs estimation
list.debug.se = list.files(pattern="se.RData")
list.debug.se = list.debug.se[grep("skill",list.debug.se)]
for(i in list.debug.se){
  cat(i,"\n \n")
  load(i)
  t = cbind(q$resTE,q$resCE,q$resSE)
  file.name = gsub(".RData",".csv",i)
  fwrite(as.data.table(t),file.name,row.names=F)
}

others.se = list.files(pattern="se.RData")[!(list.files(pattern="se.RData") %in% list.debug.se)]
for(i in others.se){
  cat(i,"\n \n")
  load(i)
  t = cbind(q$resTE,q$resCE,q$resSE)
  file.name = gsub(".RData",".csv",i)
  fwrite(as.data.table(t),file.name,row.names=F)
}
