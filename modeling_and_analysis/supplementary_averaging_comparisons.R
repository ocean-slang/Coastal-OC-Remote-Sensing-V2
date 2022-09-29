library(ICC)
library(ggplot2)
library(car)
library(RColorBrewer)
library(broom)
library(plyr)
library(dplyr)
library(zoo)
library(Metrics)
library(scales)
library(tidyr)
library(grid) #create tables
library(gridExtra)
library(stats) #used for AIC
library(lme4) #mixed modeling
library(apaTables) #export APA tables
library(patchwork) #data visualization package
library(rMR) #oxygen calculation package
setwd('/Volumes/slangSSD/mcwc_R_mat/code_for_submission/9_2022_Reruns')



#data for Part 1: Statistical Water Quality Model
data<-read.csv("S2_only_averaging_comparison.csv")#switch to acolitematch.csv to see acolite
data$type1<-factor(data$type1)
data$type2<-factor(data$type2)

ggplot(data, aes(x=zsd_3,y=zsd_1)) + 
  geom_point(aes(shape=type1),color="darkorange2",size=3)+
  theme_bw()+ #white, no gridlines
  xlab("10 m x 10 m Secchi depth (m)")+
  ylab("30 m x 30 m Secchi depth (m)")+
  xlim(0,4)+
  ylim(0,4)+
  theme(axis.title.x = element_text(size=16),axis.text.x=element_text(size=16))+ 
  theme(axis.title.y = element_text(size=16),axis.text.y=element_text(size=16))+ #defining attributes for y axis label
  geom_abline(linetype="dashed", size=1)+
  theme(aspect.ratio = 1)+
  guides(color=guide_legend(title="Satellite"))+ #adding legend title
  theme(legend.title=element_text(size=12),legend.text=element_text(size=12),legend.position="bottom") #legend attributes
ggsave("30_10.pdf",width=5,height=5) #save pdf

alldat<-read.csv("averaging_comparison.csv")
alldat$type1<-factor(alldat$type1)
alldat$type2<-factor(alldat$type2)
satlabels<- c(L8 = "Landsat-8", S2 = "Sentinel-2")
ggplot(alldat, aes(x=zsd_30,y=zsd_90)) + 
  geom_point(aes(shape=type1,color=type2),size=3)+
  scale_colour_manual(breaks = c("L8", "S2"),
                      values = c("forestgreen", "darkorange2"),
                      name="",
                      labels=satlabels) +
  theme_bw()+ #white, no gridlines
  xlab("30 m x 30 m Secchi depth (m)")+
  ylab("90 m x 90 m Secchi depth (m)")+
  xlim(0,4)+
  ylim(0,4)+
  theme(axis.title.x = element_text(size=16),axis.text.x=element_text(size=16))+ 
  theme(axis.title.y = element_text(size=16),axis.text.y=element_text(size=16))+ #defining attributes for y axis label
  geom_abline(linetype="dashed", size=1)+
  theme(aspect.ratio = 1)+
  guides(color=guide_legend(title="Satellite"))+ #adding legend title
  theme(legend.title=element_text(size=12),legend.text=element_text(size=12),legend.position="bottom") #legend attributes
ggsave("30_90.pdf",width=5,height=5) #save pdf

