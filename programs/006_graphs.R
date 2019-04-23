# Code for sampling RAIS - Graphs and Tables 
# Vitor Costa 
# November 2018

options(scipen=999)

# Loading config file
source("config.R", echo = T)

# Loading packages
library(data.table)
library(stargazer)
library(ggplot2)
library(ggpubr)
library(dplyr)

setwd(analysis.dir)

########
## 1. FEs distribution
########
obj_000 = fread(paste("000_fe",suffix,".csv",sep=""))
obj_000[,group1:=as.character(group1)]
obj_000[,group1:="All"]

obj_001 = fread(paste("000_fe",suffix,".csv",sep=""))
obj_001[,group1:=as.character(group1)]
obj_001[group1=="1",group1:="Private"]
obj_001[group1=="0",group1:="Public"]

obj_000 = rbindlist(list(obj_000,obj_001))
obj_000[,effect.mean:=mean(effect),group1]
obj_001[,effect.mean:=mean(effect),group1]

graph_000 = ggplot(obj_000,aes(x=effect))+xlab("Worker Effect")+geom_histogram(aes(y=..density..),color="black",fill="white",binwidth = 0.2)+facet_grid(group1~.)+geom_vline(xintercept=0,linetype="dotted",size=0.5)+geom_vline(data=obj_000,aes(xintercept=effect.mean,color=group1),linetype="dashed",size=1,color="red")+theme(strip.text.y = element_text(size="14",face="bold"))
ggsave(paste("001_fe_all",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")
graph_001 = ggplot(obj_001,aes(x=effect,fill=group1))+xlab("Worker Effect")+geom_histogram(aes(y=..density..),binwidth = 0.2,position="dodge")+geom_vline(xintercept=unique(obj_001[group1=="Private",effect.mean]),colour="red",linetype="dashed",size=1)+geom_vline(xintercept=unique(obj_001[group1=="Public",effect.mean]),colour="blue",linetype="dashed",size=1)+geom_vline(xintercept=0,linetype="dotted",size=0.5)+theme(legend.title = element_blank(), legend.text = element_text(size="13"), legend.position = c(0.9,0.9))+scale_fill_manual(values=c("red","blue"))
ggsave(paste("001_fe_sector",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

########
## 2. Oaxaca Blinder 
########

# 2.1 Whole Sample OB 
load(paste("001_ob_whole_sample",suffix,".RData",sep=""),verbose=T)
prov[[1]][1,2]
prov[[1]][1,4]
prov[[2]]$y.diff

load(paste("001_net_ob_whole_sample",suffix,".RData",sep=""),verbose=T)
prov[[1]][1,2]
prov[[1]][1,4]
prov[[2]]$y.diff

# 2.2 Yearly OB
obj_002 = fread(paste("001_ob_yearly",suffix,".csv",sep=""))
setnames(obj_002,c("year","total","Characteristics","se.comp","Structural","se.coef"))
obj_002[,year:=as.character(year)]
obj_002 = melt(obj_002,id.vars = c("year","se.comp","se.coef"),measure.vars = c("Characteristics","Structural"))
obj_002[variable=="Characteristics",sd:=se.comp]
obj_002[variable=="Structural",sd:=se.coef]
graph_002 = ggplot(data=obj_002,aes(x=year,y=value,fill=variable))+ylab("")+xlab("")+geom_hline(yintercept=0,linetype="dashed")+ylim(-0.1,0.6)+scale_fill_brewer(palette="Paired")+geom_bar(stat="identity",color="black",position=position_dodge())#+geom_errorbar(width=.4,position=position_dodge(0.9),aes(ymin=value-1.96*sd,ymax=value+1.96*sd))#+ylim(-0.1,0.65)+geom_bar(stat="identity")+geom_hline(yintercept=0,linetype="dashed")+scale_fill_brewer(palette="Paired")+theme_minimal()+theme(legend.title = element_blank())+ggtitle("Yearly O-B")+xlab("") + ylab("Log of hourly wage")

obj_003 = fread(paste("001_net_ob_yearly",suffix,".csv",sep=""))
setnames(obj_003,c("year","total","Characteristics","se.comp","Structural","se.coef"))
obj_003[,year:=as.character(year)]
obj_003 = melt(obj_003,id.vars = c("year","se.comp","se.coef"),measure.vars = c("Characteristics","Structural"))
obj_003[variable=="Characteristics",sd:=se.comp]
obj_003[variable=="Structural",sd:=se.coef]
graph_003 = ggplot(data=obj_003,aes(x=year,y=value,fill=variable))+ylab("")+geom_hline(yintercept=0,linetype="dashed")+ylim(-0.1,0.6)+scale_fill_brewer(palette="Paired")+geom_bar(stat="identity",color="black",position=position_dodge())+annotate("text", x = "2005", y = 0.55, label = "Corrected for Worker Effects")#+geom_errorbar(width=.4,position=position_dodge(0.9),aes(ymin=value-1.96*sd,ymax=value+1.96*sd))#+ylim(-0.1,0.65)+geom_bar(stat="identity")+geom_hline(yintercept=0,linetype="dashed")+scale_fill_brewer(palette="Paired")+theme_minimal()+theme(legend.title = element_blank())+ggtitle("Yearly O-B")+xlab("") + ylab("Log of hourly wage")
#graph_002 = ggplot(data=obj_003,aes(x=year,y=value,fill=variable))+ylim(-0.1,0.65)+geom_bar(stat="identity")+geom_hline(yintercept=0,linetype="dashed")+scale_fill_brewer(palette="Paired")+theme_minimal()+theme(legend.title = element_blank())+ggtitle("Yearly O-B net of worker FE's")+xlab("") + ylab("Log of hourly wage")

figure_001 = ggarrange(graph_002,graph_003,ncol=1,nrow=2,common.legend=T,legend="bottom")#+theme(legend.title=element_blank())
annotate_figure(figure_001,left=text_grob("log(wage/hour)",rot=90))
ggsave(paste("002_yearly_ob",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

########
## 3. Quantile Oaxaca Blinder 
########
setwd(analysis.dir)

# 3.1 QOB on whole sample
load(paste("002_qob",suffix,".RData",sep=""),verbose=T)
Sample = qob00$marginal_counterfactual+ qob00$total_effect
obj_004 = data.table(Quantiles = qob00$quantiles,Sample = Sample,Counterfactual = qob00$marginal_counterfactual, Total = qob00$total_effect, Structural = qob00$structral_effect, Characteristics = qob00$composition_effect)
obj_004 = melt(obj_004,id.vars = "Quantiles")
load(paste("002_qob_debug_se.RData"),verbose=T)
obj_004.1 = data.table(Quantiles = qob00$quantiles, Total = qob00$resTE[10:18], Structural = qob00$resSE[10:18], Characteristics = qob00$resCE[10:18], Counterfactual = qob00$model_quantile_counter[[2]])
obj_004.1 = melt(obj_004.1,id.vars = "Quantiles")
obj_004 = merge(obj_004,obj_004.1,by=c("Quantiles","variable"),all = T)
obj_004[,ylb:= value.x-qnorm(0.995)*value.y]
obj_004[,yub:=value.x+qnorm(0.995)*value.y]
obj_004.graph=obj_004[!(variable=="Sample")][!(variable=="Counterfactual")]
graph_004 = ggplot(obj_004.graph,aes(x=Quantiles,y=value.x,group=variable))+ylim(-0.2,1.0)+ylab("")+xlab("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=qob00$quantiles)+theme(legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash")) + geom_hline(aes(yintercept = 0))+geom_ribbon(aes(ymin=obj_004.graph$ylb,ymax=obj_004.graph$yub),alpha=0.3)

load(paste("002_net_qob",suffix,".RData",sep=""),verbose=T)
Sample = qob00_net$marginal_counterfactual+ qob00_net$total_effect
obj_005 = data.table(Quantiles = qob00_net$quantiles,Sample = Sample,Counterfactual = qob00_net$marginal_counterfactual, Total = qob00_net$total_effect, Structural = qob00_net$structral_effect, Characteristics = qob00_net$composition_effect)
obj_005 = melt(obj_005,id.vars = "Quantiles")
load(paste("002_net_qob_debug_se.RData"),verbose=T)
obj_005.1 = data.table(Quantiles = qob00_net$quantiles, Total = qob00_net$resTE[10:18], Structural = qob00_net$resSE[10:18], Characteristics = qob00_net$resCE[10:18], Counterfactual = qob00_net$model_quantile_counter[[2]])
obj_005.1 = melt(obj_005.1,id.vars = "Quantiles")
obj_005 = merge(obj_005,obj_005.1,by=c("Quantiles","variable"),all = T)
obj_005[,ylb:= value.x-qnorm(0.995)*value.y]
obj_005[,yub:=value.x+qnorm(0.995)*value.y]
obj_005.graph=obj_005[!(variable=="Sample")][!(variable=="Counterfactual")]
graph_005 = ggplot(obj_005.graph,aes(x=Quantiles,y=value.x,group=variable))+ylim(-0.2,1.0)+ylab("")+xlab("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=qob00$quantiles)+theme(legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash")) + geom_hline(aes(yintercept = 0))+geom_ribbon(aes(ymin=obj_005.graph$ylb,ymax=obj_005.graph$yub),alpha=0.3)+annotate("text", x = 0.3, y = 0.95, label = "Corrected for Worker Effects")

figure_002 = ggarrange(graph_004,graph_005,ncol=1,nrow=2,common.legend=T,legend="bottom")
annotate_figure(figure_002,left=text_grob("log(wage/hour)",rot=90))
ggsave(paste("003_qob",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

# 3.2 QOB by Gender and Skill
qob.gs = data.table(quantiles=numeric(),female=character(),skill=character(),variable=factor(),value=numeric())
for (i in 0:1){
  for (j in 1:3){
    load(paste("002_qob_genderskill_",i,j,suffix,".RData",sep=""),verbose=T)
    obj = data.table(quantiles = q$quantiles, Total = q$total_effect, Structural = q$structral_effect, Characteristics = q$composition_effect)
    if(i==0){obj[,female:="Male"]}else{obj[,female:="Female"]} 
    obj[,skill:=j] 
    obj = melt(obj,id.vars = c("quantiles","female","skill"))
    qob.gs = rbind(qob.gs,obj) 
  }
}
qob.gs[,female:=as.factor(female)]
qob.gs[,skill:=factor(skill,levels=c(1,2,3),labels=c("Low Education","Medium Education","High Education"))]

fig_003 = ggplot(qob.gs,aes(x=quantiles,y=value,group=variable))+theme(strip.text.x = element_text(size=14,face="bold"),strip.text.y=element_text(size=14,face="bold"))+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=q$quantiles)+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title=element_blank(),legend.position="bottom")+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_hline(aes(yintercept=0))+facet_grid(female~skill)
ggsave(paste("002_qob_gs",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

# Net of FE's
qob.net.gs = data.table(quantiles=numeric(),female=character(),skill=character(),variable=factor(),value=numeric())
for (i in 0:1){
  for (j in 1:3){
    load(paste("002_net_qob_genderskill_",i,j,suffix,".RData",sep=""),verbose=T)
    obj = data.table(quantiles = q$quantiles, Total = q$total_effect, Structural = q$structral_effect, Characteristics = q$composition_effect)
    if(i==0){obj[,female:="Male"]}else{obj[,female:="Female"]} 
    obj[,skill:=j]
    #if(j==1){obj[,skill:="Low Skill"]}else{if(j==2){obj[,skill:="Medium Skill"]}else{obj[,skill:="High Skill"]}} 
    obj = melt(obj,id.vars = c("quantiles","female","skill"))
    qob.net.gs = rbind(qob.net.gs,obj) 
  }
}
qob.net.gs[,female:=as.factor(female)]
qob.net.gs[,skill:=factor(skill,levels=c(1,2,3),labels=c("Low Education","Medium Education","High Education"))]

fig_004 = ggplot(qob.net.gs,aes(x=quantiles,y=value,group=variable))+theme(strip.text.x = element_text(size=14,face="bold"),strip.text.y=element_text(size=14,face="bold"))+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=q$quantiles)+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title=element_blank(),legend.position="bottom")+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_hline(aes(yintercept=0))+facet_grid(female~skill)
ggsave(paste("002_net_qob_gs",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

# 3.3 QOB by Race and Skill
qob.rs = data.table(quantiles=numeric(),nonwhite=character(),skill=character(),variable=factor(),value=numeric())
for (i in 0:1){
  for (j in 1:3){
    load(paste("003_qob_raceskill_",i,j,suffix,".RData",sep=""),verbose=T)
    obj = data.table(quantiles = q$quantiles, Total = q$total_effect, Structural = q$structral_effect, Characteristics = q$composition_effect)
    if(i==0){obj[,nonwhite:="White"]}else{obj[,nonwhite:="Nonwhite"]} 
    obj[,skill:=j]
    #if(j==1){obj[,skill:="Low Skill"]}else{if(j==2){obj[,skill:="Medium Skill"]}else{obj[,skill:="High Skill"]}} 
    obj = melt(obj,id.vars = c("quantiles","nonwhite","skill"))
    qob.rs = rbind(qob.rs,obj) 
  }
}
qob.rs[,nonwhite:=as.factor(nonwhite)]
qob.rs[,skill:=factor(skill,levels=c(1,2,3),labels=c("Low Education","Medium Education","High Education"))]

fig_005 = ggplot(qob.rs,aes(x=quantiles,y=value,group=variable))+theme(strip.text.x = element_text(size=14,face="bold"),strip.text.y=element_text(size=14,face="bold"))+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=q$quantiles)+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title=element_blank(),legend.position="bottom")+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_hline(aes(yintercept=0))+facet_grid(nonwhite~skill)
ggsave(paste("003_qob_rs",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

# Net of FE's
qob.net.rs = data.table(quantiles=numeric(),nonwhite=character(),skill=character(),variable=factor(),value=numeric())
for (i in 0:1){
  for (j in 1:3){
    load(paste("003_net_qob_raceskill_",i,j,suffix,".RData",sep=""),verbose=T)
    obj = data.table(quantiles = q$quantiles, Total = q$total_effect, Structural = q$structral_effect, Characteristics = q$composition_effect)
    if(i==0){obj[,nonwhite:="White"]}else{obj[,nonwhite:="Nonwhite"]} 
    obj[,skill:=j]
    #if(j==1){obj[,skill:="Low Skill"]}else{if(j==2){obj[,skill:="Medium Skill"]}else{obj[,skill:="High Skill"]}} 
    obj = melt(obj,id.vars = c("quantiles","nonwhite","skill"))
    qob.net.rs = rbind(qob.net.rs,obj) 
  }
}
qob.net.rs[,nonwhite:=as.factor(nonwhite)]
qob.net.rs[,skill:=factor(skill,levels=c(1,2,3),labels=c("Low Education","Medium Education","High Education"))]

fig_006 = ggplot(qob.net.rs,aes(x=quantiles,y=value,group=variable))+theme(strip.text.x = element_text(size=14,face="bold"),strip.text.y=element_text(size=14,face="bold"))+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=q$quantiles)+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title=element_blank(),legend.position="bottom")+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_hline(aes(yintercept=0))+facet_grid(nonwhite~skill)
ggsave(paste("003_net_qob_rs",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

##############
# 3. FEs by characteristics
##############
obj_000[,skill:=factor(skill,c(1,2,3),c("Low","Middle","High"))]
obj_000[,nonwhite1:=factor(nonwhite1,c(0,1),c("White","Nonwhite"))]
obj_000[,group1:=factor(group1,c(0,1),c("Public","Private"))]
obj_000[,region:=factor(region,c(1,2,3,4,5),c("North","Northeast","Southeast","South","Midwest"))]
obj_000[,sex1:=factor(sex1,c(0,1),c("Male","Female"))]
box001 = ggplot(obj_000,aes(x=skill,y=effect))+geom_boxplot()
ggsave(paste("001_fe_skill",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")
box002 = ggplot(obj_000,aes(x=nonwhite1,y=effect))+geom_boxplot()
ggsave(paste("001_fe_race",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")
box003 = ggplot(obj_000,aes(x=region,y=effect))+geom_boxplot()
ggsave(paste("001_fe_region",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")
box004 = ggplot(obj_000,aes(x=sex1,y=effect))+geom_boxplot()
ggsave(paste("001_fe_sex",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

t = unique(obj_001[,ind_mean_wage:=log(mean(hwage1)),.(pis)][,.(effect,ind_mean_wage)])
graph_006 = ggplot(unique(obj_001[,.(effect,ind_mean_wage)]),aes(x=effect,y=ind_mean_wage))+geom_point(shape='.',col="white")+geom_hex()#stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE)#+scale_fill_viridis_c()
ggsave(paste("001_fe_wage",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

##############
# 4. Getting the SEs for estimations
##############
setwd(analysis.dir)

# 4.1 Yearly OB table 
obj_002 = fread(paste("001_ob_yearly",suffix,".csv",sep=""))
obj_002[,coef.CI.99:=paste("[",coef-qnorm(0.995)*se.coef,",",coef+qnorm(0.995)*se.coef,"]",sep="")]
obj_002[,comp.CI.99:=paste("[",comp-qnorm(0.995)*se.comp,",",comp+qnorm(0.995)*se.comp,"]",sep="")]
setcolorder(obj_002,c("year","total","comp","se.comp","comp.CI.99","coef","se.coef","coef.CI.99"))
setnames(obj_002,c("Year","Total","Characteristics","Char.SE","Char.CI.99","Structural","Str.SE","Str.CI.99"))
fwrite(obj_002,paste(graphs.dir,"/001_yearly_ob_se",suffix,".csv",sep=""))

obj_003 = fread(paste("001_net_ob_yearly",suffix,".csv",sep=""))
obj_003[,coef.CI.99:=paste("[",coef-qnorm(0.995)*se.coef,",",coef+qnorm(0.995)*se.coef,"]",sep="")]
obj_003[,comp.CI.99:=paste("[",comp-qnorm(0.995)*se.comp,",",comp+qnorm(0.995)*se.comp,"]",sep="")]
setcolorder(obj_003,c("year","total","comp","se.comp","comp.CI.99","coef","se.coef","coef.CI.99"))
setnames(obj_003,c("Year","Total","Characteristics","Char.SE","Char.CI.99","Structural","Str.SE","Str.CI.99"))
fwrite(obj_003,paste(graphs.dir,"/001_net_yearly_ob_se",suffix,".csv",sep=""))

# QOB on Whole Sample
names(obj_004)=c("Quantiles","Component","Value","Std. Error","Value lower bound","Value upper bound")
fwrite(obj_004,paste(graphs.dir,"/002_qob",suffix,"_se.csv",sep=""),row.names = F)
names(obj_005)=c("Quantiles","Component","Value","Std. Error","Value lower bound","Value upper bound")
fwrite(obj_005,paste(graphs.dir,"/002_net_qob",suffix,"_se.csv",sep=""),row.names = F)

# 4.3 QOB gender and skill
qob.gs = data.table(Quantiles=numeric(),Sex=character(),Education=character(),Component=factor(),Value=numeric(),'Std Error' = numeric(), 'Value lower bound'=numeric(), 'Value upper bound'=numeric())
for (i in 0:1){
  for (j in 1:3){
    cat(i,j,"\n \n")
    
    load(paste("002_qob_genderskill_",i,j,suffix,".RData",sep=""),verbose=T)
    Sample = q$marginal_counterfactual+q$total_effect
    obj0 = data.table(Quantiles = q$quantiles, Sample = Sample,Counterfactual = q$marginal_counterfactual,Total = q$total_effect, Structural = q$structral_effect, Characteristics = q$composition_effect)
    if(i==0){obj0[,Sex:="Male"]}else{obj0[,Sex:="Female"]} 
    obj0[,Education:=j] 
    obj0 = melt(obj0,id.vars = c("Quantiles","Sex","Education"))
    
    load(paste("002_qob_genderskill_",i,j,"_debug_se.RData",sep=""),verbose=T)
    obj1 = data.table(Quantiles = q$quantiles, Counterfactual = q$model_quantile_counter[[2]],Total = q$resTE[10:18], Structural = q$resSE[10:18], Characteristics = q$resCE[10:18])
    if(i==0){obj1[,Sex:="Male"]}else{obj1[,Sex:="Female"]} 
    obj1[,Education:=j]
    obj1 = melt(obj1,id.vars = c("Quantiles","Sex","Education"))
    
    obj = merge(obj0,obj1,by=c("Quantiles","Sex","Education","variable"),all=T)
    obj[,lb := value.x-qnorm(0.995)*value.y]
    obj[,ub := value.x+qnorm(0.995)*value.y]
    names(obj) = names(qob.gs)
    
    qob.gs = rbind(qob.gs,obj) 
  }
}
qob.gs[,Education:=factor(Education,levels=c(1,2,3),labels=c("Low","Medium","High"))]
fwrite(qob.gs,paste(graphs.dir,"/002_qob_genderskill_se.csv",sep=""),row.names = F)

#Net of FEs
net.qob.gs = data.table(Quantiles=numeric(),Sex=character(),Education=character(),Component=factor(),Value=numeric(),'Std Error' = numeric(), 'Value lower bound'=numeric(), 'Value upper bound'=numeric())
for (i in 0:1){
  for (j in 1:3){
    cat(i,j,"\n \n")
    
    load(paste("002_net_qob_genderskill_",i,j,suffix,".RData",sep=""),verbose=T)
    Sample = q$marginal_counterfactual+q$total_effect
    obj0 = data.table(Quantiles = q$quantiles, Sample = Sample,Counterfactual = q$marginal_counterfactual,Total = q$total_effect, Structural = q$structral_effect, Characteristics = q$composition_effect)
    if(i==0){obj0[,Sex:="Male"]}else{obj0[,Sex:="Female"]} 
    obj0[,Education:=j] 
    obj0 = melt(obj0,id.vars = c("Quantiles","Sex","Education"))
    
    load(paste("002_net_qob_genderskill_",i,j,"_debug_se.RData",sep=""),verbose=T)
    obj1 = data.table(Quantiles = q$quantiles, Counterfactual = q$model_quantile_counter[[2]],Total = q$resTE[10:18], Structural = q$resSE[10:18], Characteristics = q$resCE[10:18])
    if(i==0){obj1[,Sex:="Male"]}else{obj1[,Sex:="Female"]} 
    obj1[,Education:=j]
    obj1 = melt(obj1,id.vars = c("Quantiles","Sex","Education"))
    
    obj = merge(obj0,obj1,by=c("Quantiles","Sex","Education","variable"),all=T)
    obj[,lb := value.x-qnorm(0.995)*value.y]
    obj[,ub := value.x+qnorm(0.995)*value.y]
    names(obj) = names(net.qob.gs)
    
    net.qob.gs = rbind(net.qob.gs,obj) 
  }
}
net.qob.gs[,Education:=factor(Education,levels=c(1,2,3),labels=c("Low","Medium","High"))]
fwrite(net.qob.gs,paste(graphs.dir,"/002_net_qob_genderskill_se.csv",sep=""),row.names = F)


# 4.4 QOB Race and Skill
qob.rs = data.table(Quantiles=numeric(),Race=character(),Education=character(),Component=factor(),Value=numeric(),'Std Error' = numeric(), 'Value lower bound'=numeric(), 'Value upper bound'=numeric())
for (i in 0:1){
  for (j in 1:3){
    cat(i,j,"\n \n")
    
    load(paste("003_qob_raceskill_",i,j,suffix,".RData",sep=""),verbose=T)
    Sample = q$marginal_counterfactual+q$total_effect
    obj0 = data.table(Quantiles = q$quantiles, Sample = Sample,Counterfactual = q$marginal_counterfactual,Total = q$total_effect, Structural = q$structral_effect, Characteristics = q$composition_effect)
    if(i==0){obj0[,Race:="White"]}else{obj0[,Race:="Nonwhite"]} 
    obj0[,Education:=j] 
    obj0 = melt(obj0,id.vars = c("Quantiles","Race","Education"))
    
    load(paste("003_qob_raceskill_",i,j,"_debug_se.RData",sep=""),verbose=T)
    obj1 = data.table(Quantiles = q$quantiles, Counterfactual = q$model_quantile_counter[[2]],Total = q$resTE[10:18], Structural = q$resSE[10:18], Characteristics = q$resCE[10:18])
    if(i==0){obj1[,Race:="White"]}else{obj1[,Race:="Nonwhite"]} 
    obj1[,Education:=j]
    obj1 = melt(obj1,id.vars = c("Quantiles","Race","Education"))
    
    obj = merge(obj0,obj1,by=c("Quantiles","Race","Education","variable"),all=T)
    obj[,lb := value.x-qnorm(0.995)*value.y]
    obj[,ub := value.x+qnorm(0.995)*value.y]
    names(obj) = names(qob.rs)
    
    qob.rs = rbind(qob.rs,obj) 
  }
}
qob.rs[,Education:=factor(Education,levels=c(1,2,3),labels=c("Low","Medium","High"))]
fwrite(qob.rs,paste(graphs.dir,"/003_qob_raceskill_se.csv",sep=""),row.names = F)

#Net of FEs
net.qob.rs = data.table(Quantiles=numeric(),Race=character(),Education=character(),Component=factor(),Value=numeric(),'Std Error' = numeric(), 'Value lower bound'=numeric(), 'Value upper bound'=numeric())
for (i in 0:1){
  for (j in 1:3){
    cat(i,j,"\n \n")
    
    load(paste("003_qob_raceskill_",i,j,suffix,".RData",sep=""),verbose=T)
    Sample = q$marginal_counterfactual+q$total_effect
    obj0 = data.table(Quantiles = q$quantiles, Sample = Sample,Counterfactual = q$marginal_counterfactual,Total = q$total_effect, Structural = q$structral_effect, Characteristics = q$composition_effect)
    if(i==0){obj0[,Race:="White"]}else{obj0[,Race:="Nonwhite"]} 
    obj0[,Education:=j] 
    obj0 = melt(obj0,id.vars = c("Quantiles","Race","Education"))
    
    load(paste("003_qob_raceskill_",i,j,"_debug_se.RData",sep=""),verbose=T)
    obj1 = data.table(Quantiles = q$quantiles, Counterfactual = q$model_quantile_counter[[2]],Total = q$resTE[10:18], Structural = q$resSE[10:18], Characteristics = q$resCE[10:18])
    if(i==0){obj1[,Race:="White"]}else{obj1[,Race:="Nonwhite"]} 
    obj1[,Education:=j]
    obj1 = melt(obj1,id.vars = c("Quantiles","Race","Education"))
    
    obj = merge(obj0,obj1,by=c("Quantiles","Race","Education","variable"),all=T)
    obj[,lb := value.x-qnorm(0.995)*value.y]
    obj[,ub := value.x+qnorm(0.995)*value.y]
    names(obj) = names(net.qob.rs)
    
    net.qob.rs = rbind(net.qob.rs,obj) 
  }
}
net.qob.rs[,Education:=factor(Education,levels=c(1,2,3),labels=c("Low","Medium","High"))]
fwrite(qob.rs,paste(graphs.dir,"/003_net_qob_raceskill_se.csv",sep=""),row.names = F)