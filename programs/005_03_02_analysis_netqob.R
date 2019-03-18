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

#########
# 5 Recomputing QOB *~without~* fixed effects
#########

# 5.1 QOB by gender and skill
qob.gs = function(x){
  counterfactual(log(hwage1)-effect~nonwhite1+tenure+I(tenure^2)+factor(region)+factor(yr),
                 data=x,group=group1,sepcore=T,treatment=T,decomposition=T,ncore=detectCores()-1,noboot=noboot,nreg=100)
}

for (i in 0:1){
  for (j in 1:3){
    t = system.time((q <- qob.gs(data[sex1==i&skill==j])))
    file.name = paste("002_net_qob_genderskill_",i,j,suffix,out.se,".RData",sep="")
    this.meta.time = rbind(meta.time,list(file.name,as.character(Sys.Date()),t[[1]],t[[2]],t[[3]]))
    fwrite(this.meta.time,meta.time.file,append=T)
    save(q,file = file.name)
  }
}
rm(q)

# 5.2 QOB by race and skill
qob.rs = function(x){
  counterfactual(log(hwage1)-effect~sex1+tenure+I(tenure^2)+factor(region)+factor(yr),
                 data=x,group=group1,sepcore=T,treatment=T,decomposition=T,ncore=detectCores()-1,noboot=noboot,nreg=100)
}

for (i in 0:1){
  for (j in 1:3){
    t = system.time((q <- qob.rs(data[nonwhite1==i&skill==j])))
    file.name = paste("003_net_qob_raceskill_",i,j,suffix,out.se,".RData",sep="")
    this.meta.time = rbind(meta.time,list(file.name,as.character(Sys.Date()),t[[1]],t[[2]],t[[3]]))
    fwrite(this.meta.time,meta.time.file,append=T)
    save(q,file = file.name)
  }
}
rm(q)
