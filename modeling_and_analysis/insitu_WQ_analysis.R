library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(patchwork) 
library(rMR) #oxygen calculation package
setwd('/Volumes/slangSSD/mcwc_R_mat/code_for_submission')

wqdrivers_raw<-read.csv('wqdrivers.csv')

#remove nh4 and po4 measurements that fall outside the detection limit
wqdrivers<-wqdrivers_raw %>%
  mutate(pom = replace(pom, pom<0, NA))%>%
  mutate(pim = replace(pim, pim<0, NA))%>%
  mutate(tss = replace(tss, tss<0, NA))%>%
  mutate(tss = replace(tss, pom<0, NA))%>%
  mutate(tss = replace(tss, pim<0, NA))%>%
  mutate(nh4 = replace(nh4, nh4<0.36, NA))%>%
  mutate(nh4 = replace(nh4, nh4>45, NA))%>%
  mutate(po4 = replace(po4, po4<0.16, NA))%>%
  mutate(po4 = replace(po4, po4>20, NA))

#oxygen calculations from rMR package
wqdrivers$percentsat <- DO.saturation(DO.mgl = wqdrivers$do,
                                      temp.C = wqdrivers$dotemp, salinity=wqdrivers$sctsal, salinity.units="pp.thou",elevation.m = 0) #percent oxygen saturation- not used in apparent oxygen utilization measurement, but could be useful anyway
wqdrivers$oxy_equil<-Eq.Ox.conc(temp.C=wqdrivers$dotemp, elevation.m = 0, out.DO.meas = "mg/L", salinity = wqdrivers$sctsal, salinity.units = "pp.thou") #calculate oxygen equilibrium saturation concentration at given water temperature and salinity
wqdrivers$aou<-wqdrivers$oxy_equil-wqdrivers$do #difference between oxygen equilibrium sat concentration and dissolved oxygen
wqdrivers$aou <- replace(wqdrivers$aou,wqdrivers$aou < -50, NA) #remove outlier

#standardizing the data
standardData<-wqdrivers[,c("measureDate","yearday","secchi","chlor_a", "tss", "aou","scttemp","sctsal","sctcond","pom","pim","phaeopigments_a","TDN","nh4","po4")] #subset data to be standardized
standardData[,c("secchi","chlor_a", "tss", "aou","scttemp","sctsal","sctcond","pom","pim","phaeopigments_a","TDN","nh4","po4")] <- lapply(standardData[,c("secchi","chlor_a","tss", "aou","scttemp","sctsal","sctcond","pom","pim","phaeopigments_a","TDN","nh4","po4")], function(x) c(scale(x))) #standardize data

#Plotting individual graphs, grouping parameters
# In order to plot two y axises in R, you must transform the second axis by some multiple of the first, and shift the parameters you are plotting as well. Transforming an axis with negative values is really tricky... I had to use some creative transformations for x2, x3, and x4. x1 was more simple, just simply multiplying the first axis by 2.5/30 and dividing the parameter on the secondary axis (Secchi depth) by that number. For the others, transformations were more complicated! some hand written calculations, some trial by error!
labels1<-c("Temperature","Secchi depth")
#used to transform the left axis #s to the right axis
coeff<-2.5/30
x1<-
  ggplot(data=wqdrivers, aes(x=yearday))+
  geom_point(aes(y=secchi/coeff),color="gray")+ #divide by coefficient
  geom_smooth(aes(y=secchi/coeff,color="secchi"),method='loess',se=F)+
  geom_point(aes(y=scttemp),color="lightsteelblue1")+
  geom_smooth(aes(y=scttemp,color="scttemp"),se=F)+
  scale_colour_manual(breaks = c("scttemp", "secchi"),
                      values = c("blue", "black"),
                      name="",
                      labels=labels1) +
  scale_y_continuous(name="Temperature C\u00b0",breaks = scales::pretty_breaks(n = 5),sec.axis = sec_axis(~.*coeff,name="Secchi depth, m",breaks = scales::pretty_breaks(n = 5)))+ #multiply secondary axis by coefficient
  theme_classic()+
  theme(legend.position="top")+ #move legend to top
  theme(aspect.ratio = 1)+ #square graph
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+ #amount of tick marks on x axis
  xlab("Year day")+
  labs(title="A) Temperature")

#equations in y and trans: https://stackoverflow.com/questions/54612136/how-to-scale-a-secondary-axis-with-ggplot2-second-axis-has-negative-values
#had to play around with different transformations, weird because tough to translate neg axis to pos one...
#secondary axis has to be translated by using the first axis,.. only one to one transformations allowed!!

labels <- c(sctsal = "Salinity", sctcond = "Conductivity", aou = "Apparent oxygen\nutilization",secchi="Secchi\ndepth")
coeff4<-1.5
x2<-ggplot()+
  geom_point(data=wqdrivers,aes(x=yearday,y=((secchi-2)/coeff4)),color="gray")+ #plot secchi depths with transformation, opposite formula to axis transformation, secchi is on secondary axis
  geom_smooth(data=wqdrivers,aes(x=yearday,y=((secchi-2)/coeff4),color="secchi"),method='loess',se=F)+ #loess smooth, secondary axis
  geom_smooth(data=standardData,aes(x=yearday,y=sctsal,color="sctsal"),method='loess',se=F)+ #primary axis, no transformation necessary
  geom_smooth(data=standardData,aes(x=yearday,y=sctcond,color="sctcond"),method='loess',se=F)+#primary axis, no transformation
  geom_smooth(data=standardData,aes(x=yearday,y=aou,color="aou"),method='loess',se=F)+#primary axis, no transformation
  #assign colors to each smooth:
  scale_colour_manual(breaks = c("sctsal", "sctcond", "aou","secchi"),
                      values = c("palegreen", "seagreen4", "royalblue","black"),
                      name="",
                      labels=labels) +
  theme_classic()+
  theme(legend.position="top")+
  theme(aspect.ratio = 1)+
  scale_y_continuous(name="Z-Score",breaks = scales::pretty_breaks(n = 10),sec.axis = sec_axis(trans = ~.*coeff4+2,name="Secchi depth, m",breaks = scales::pretty_breaks(n = 5)))+ #assign transformation to secondary axis and # of tick mark labels on y-axis
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  xlab("Year day")+
  labs(title="B) Salinity, conductivity, and oxygen utilization")+
  theme(plot.title = element_text(hjust = 0.5))

#same code for x3 and x4 as x2
coeff6<-2
labels2<-c("POM","PIM","TSS","Secchi\ndepth")
x3<-
  ggplot()+
  geom_point(data=wqdrivers,aes(x=yearday,y=((secchi-2)/coeff6)),color="gray")+
  geom_smooth(data=wqdrivers,aes(x=yearday,y=((secchi-2)/coeff6),color="secchi"),method='loess',se=F)+
  geom_smooth(data=standardData,aes(x=yearday,y=pom,color="pom"),method='loess',se=F)+
  geom_smooth(data=standardData,aes(x=yearday,y=pim,color="pim"),method='loess',se=F)+
  geom_smooth(data=standardData,aes(x=yearday,y=tss,color="tss"),method='loess',se=F)+
  scale_colour_manual(breaks = c("pom", "pim", "tss","secchi"),
                      values = c("royalblue", "tomato", "purple4","black"),
                      name="",
                      labels=labels2) +
  theme_classic()+
  theme(legend.position="top")+ 
  theme(aspect.ratio = 1)+
  scale_y_continuous(name="Z-Score",breaks = scales::pretty_breaks(n = 10),sec.axis = sec_axis(trans = ~.*coeff6+2,name="Secchi depth, m",breaks = scales::pretty_breaks(n = 5)))+
  xlab("Year Day")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  labs(title="C) Suspended particulates")

coeff5<-1.1
labels3<-c("TDN","PO4","NH4","Phaeopigments","Chlorophyll-a","Secchi depth")
x4<-
  ggplot()+
  geom_point(data=wqdrivers,aes(x=yearday,y=((secchi-2)/coeff5)),color="gray")+
  geom_smooth(data=wqdrivers,aes(x=yearday,y=((secchi-2)/coeff5),color="secchi"),method='loess',se=F)+
  geom_smooth(data=standardData,aes(x=yearday,y=TDN,color="TDN"),method='loess',se=F)+
  geom_smooth(data=standardData,aes(x=yearday,y=po4,color="po4"),method='loess',se=F)+
  geom_smooth(data=standardData,aes(x=yearday,y=nh4,color="nh4"),method='loess',se=F)+
  geom_smooth(data=standardData,aes(x=yearday,y=phaeopigments_a,color="phaeopigments_a"),method='loess',se=F)+
  geom_smooth(data=standardData,aes(x=yearday,y=chlor_a,color="chlor_a"),method='loess',se=F)+
  scale_colour_manual(breaks = c("TDN", "po4", "nh4","phaeopigments_a","chlor_a","secchi"),
                      values = c("royalblue", "tomato", "purple4","palegreen","seagreen4","black"),
                      name="",
                      labels=labels3) +
  theme_classic()+
  theme(legend.position="top")+ 
  theme(aspect.ratio = 1)+
  scale_y_continuous(name="Z-Score",breaks = scales::pretty_breaks(n = 10),sec.axis = sec_axis(trans = ~.*coeff5+2,name="Secchi depth, m",breaks = scales::pretty_breaks(n = 5)))+
  xlab("Year day")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  labs(title="D) Nutrients and pigments")

#patchwork
(x1+x2)/(x3+x4)
x1+x3+x2+x4+plot_layout(ncol=2) #assign plots in two columns