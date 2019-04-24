# Code for sampling RAIS - analysis I - Mincer and Fixed Effects
# Vitor Costa 
# November 2018

options(scipen=999)

# Loading config file
source("config.R", echo = T)

# Loading packages
library(data.table)
library(lfe)
library(stargazer)

# Reading sample
setwd(output.dir)
input.file = paste("04_toanalysis",suffix,".csv",sep="")
data = fread(input.file,colClasses = "character",na.strings = "")

# Adjusting class of columns
setkey(data,yr,pis)
data = data[,lapply(.SD,as.numeric),.SDcols=names(data)]
data = data[mean_earn!=0&hired_wage!=0]

# #########
# # 0. Getting a sense of the data via Mincerian equations
# #########
mincer_00 = felm(log(hwage1)~group1+nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)|size+yr+region|0|region+size,data=data,exactDOF=T)
# mincer_01 = felm(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)|establishment_size+yr+st|0|st+establishment_size,data=data,exactDOF=T)
mincer_02 = felm(log(hwage1)~group1+nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)|yr+region|0|region,data=data,exactDOF=T)
# mincer_03 = felm(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)|yr+st|0|st,data=data,exactDOF=T)
# mincer_04 = felm(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+age1+I(age1^2)|establishment_size+yr+region|0|region+establishment_size,data=data,exactDOF=T)
# mincer_05 = felm(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+age1+I(age1^2)|establishment_size+yr+st|0|st+establishment_size,data=data,exactDOF=T)
mincer_06 = felm(log(hwage1)~group1+nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+age1+I(age1^2)|yr+region|0|region,data=data,exactDOF=T)
# mincer_07 = felm(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+age1+I(age1^2)|yr+st|0|st,data=data,exactDOF=T)

#########
# 1 Computing individual fixed effects
#########
pool1 = felm(log(hwage1)~group1+med_skill+high_skill+tenure+I(tenure^2)|yr+region+pis|0|region,data=data, na.action=na.omit)
fe.pis.1 = as.data.table(getfe(pool1))
fe.pis.1[,pis:=as.numeric(as.character(idx))]
fe.pis.1 = fe.pis.1[fe=="pis",c("pis","effect")]

pool2 = felm(log(hwage1)~group1+age1+med_skill+high_skill+tenure+I(tenure^2)|yr+region+pis|0|region,data=data, na.action=na.omit)
fe.pis.2 = as.data.table(getfe(pool2))
fe.pis.2[,pis:=as.numeric(as.character(idx))]
fe.pis.2 = fe.pis.2[fe=="pis",c("pis","effect")]

stargazer(mincer_00,mincer_02,mincer_06,pool1,pool2,type="text",dep.var.labels = "Log Hourly Wage (2011 US$)",add.lines = list(c("Fixed Effects","Establishment Size,Year and Region","Year and Region","Year and Region","Year and Region"),
                                                                                                                        c("Worker Effects","No","No","No","Yes")))

prov = list(mincer_00,mincer_02,mincer_06,pool1,pool2)
setwd(analysis.dir)
save(prov,file=paste("000_mincer",suffix,".RData",sep=""))
rm(prov)
rm(list=ls(pattern="mincer_"))

#merging fe with data
data = merge(data,fe.pis.1,by="pis")

#saving fixed effects
setwd(analysis.dir)
fe.table = unique(data[,.(pis,effect,group1,age1,region,skill,hwage1,sex1,nonwhite1)])
fe.table[,group1 := as.character(group1)]
fwrite(fe.table,file=paste("000_fe",suffix,".csv",sep=""),nThread = 4)
##########################################################################################
