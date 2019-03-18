# Code for sampling RAIS - analysis III -Quantile Oaxaca Blinder
# Vitor Costa 
# November 2018

options(scipen=999)

# Loading config file
source("config.R", echo = T)

# Loading packages
library(data.table)
library(Counterfactual)
library(lfe)
library(stargazer)
library(ggplot2)
library(oaxaca)
library(doParallel)
library(foreach)

# Reading sample
setwd(output.dir)
input.file = paste("04_toanalysis",suffix,".csv",sep="")
data = fread(input.file,colClasses = "character",na.strings = "")

# Adjusting class of columns
setkey(data,yr,pis)
data = data[,lapply(.SD,as.numeric),.SDcols=names(data)]
data = data[mean_earn!=0&hired_wage!=0]

# Structure of metatime file
meta.time = data.table(file=character(),data=character(),user=numeric(),system=numeric(),elapsed=numeric())
meta.time.file = paste(metadata.dir,"005_elapsed.csv",sep="/")

#########
# 3. Oaxaca-Blinder counterfactual quantile
#########

setwd(analysis.dir)

#Changing coding for group1 and group2 
data[,group1:=ifelse(group1==1,0,1)]
data[,group2:=ifelse(group2==1,0,1)]

#########
# 4 Inputing individual FEs
#########
setwd(analysis.dir)
input.file = paste("000_fe",suffix,".csv",sep="")
fe.pis = fread(input.file)
fe.pis = unique(fe.pis[,c("pis","effect")])
data = merge(data,fe.pis,by="pis")

#####
# 6. QOB on whole sample
#####

## 6.1 QOB on whole sample 

base.qob = function(x){
  counterfactual(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+factor(region)+factor(yr),
                 data=x,group=group1,sepcore=T,treatment=T,decomposition=T,ncore=detectCores()-1,noboot=noboot,nreg=100)
}

t = system.time(qob00 <- base.qob(data))

file.name=paste("002_qob",suffix,out.se,".RData",sep="")
this.meta.time = rbind(meta.time,list(file.name,as.character(Sys.Date()),t[[1]],t[[2]],t[[3]]))
fwrite(this.meta.time,meta.time.file,append=T)

setwd(analysis.dir)
save(qob00,file=file.name)
rm(qob00)

## 6.2 QOB net of FEs on whole sample
base.qob = function(x){
  counterfactual(log(hwage1)-effect~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+factor(region)+factor(yr),
                 data=x,group=group1,sepcore=T,treatment=T,decomposition=T,ncore=detectCores()-1,noboot=noboot,nreg=100)
}

t = system.time(qob00_net <- base.qob(data))

file.name=paste("002_net_qob",suffix,out.se,".RData",sep="")
this.meta.time = rbind(meta.time,list(file.name,as.character(Sys.Date()),t[[1]],t[[2]],t[[3]]))
fwrite(this.meta.time,meta.time.file,append=T)

setwd(analysis.dir)
save(qob00_net,file=file.name)
rm(qob00_net)
