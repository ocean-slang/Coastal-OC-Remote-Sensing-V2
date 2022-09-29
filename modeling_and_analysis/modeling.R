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
setwd('/Volumes/slangSSD/mcwc_R_mat/code_for_submission')

#data for Part 1: Statistical Water Quality Model
data<-read.csv("matchup.csv")

#Plot to search for outliers, only select observations from Sentinel-2 (S2)
ggplot(data=data[which (data$type=="S2"),], aes(x=sat, y=insitu))+
  geom_point()+
  geom_text(aes(label=sat),hjust=0, vjust=0)+
  theme_classic()
#Plot to search for outliers, only select observations from Landsat-8 (L8)
ggplot(data=data[which (data$type=="S2"),], aes(x=sat, y=insitu))+
  geom_point()+
  geom_text(aes(label=sat),hjust=0, vjust=0)+
  theme_classic()
#Remove outliers, row 73, where insitu=0.36 m and L8=3.7m

#create new variable "region", assign NAs to all rows
data$region<-NA
#assign each site to "Ocean Inlet (OC)," "Lagoon (L)," or "Mainland Creek (MC)"
data<-data%>%
  mutate(region=ifelse(site==5 | site==3 | site==15 | site==6 | site==12 | site==17, "OI", ifelse(site==8| site==16, "L", ifelse(site==2 | site==13 | site==14, "MC", NA))))

data$catdepth<-NA
data<-data%>%
  mutate(catdepth=ifelse(site==2 | site==3 | site==5 | site==15, "shallow", ifelse(site==8 |site==13 | site==14, "intermediate", ifelse(site==6 | site==12 | site==16 | site==17, "deep", NA))))


#factor site and region
data$site<-factor(data$site)
data$region<-factor(data$region)

l8model<-lm(insitu~sat,data[data$type=="L8",])
car::Anova(l8model,type="II")
plot(data[data$type=="L8",]$insitu,data[data$type=="L8",]$sat) #scatterplot satellite vs. in situ zsd
s2model<-lm(insitu~sat,data[data$type=="S2",])
summary(s2model)
plot(data[data$type=="S2",]$insitu,data[data$type=="S2",]$sat)
car::Anova(s2model,type="II")

#Basic statistics, subsetting data to find mean and standard deviation (sd) of just landsat-8 or sentinel-2
mean(data[which (data$type=="L8"),]$sat)
sd(data[which (data$type=="L8"),]$sat)
mean(data[which (data$type=="S2"),]$sat)
sd(data[which (data$type=="S2"),]$sat)
mean(data$insitu)
sd(data$insitu)

#Pearson product moment correlations of satellite and in situ, complete.obs removes NAs
cor.test(data$sat, data$insitu, method = "pearson", use = "complete.obs")
#Landsat-8 only
cor.test(data[which (data$type=="L8"),]$sat, data[which (data$type=="L8"),]$insitu, method = "pearson", use = "complete.obs")
#Sentinel-2 only
cor.test(data[which (data$type=="S2"),]$sat, data[which (data$type=="S2"),]$insitu, method = "pearson", use = "complete.obs")

#Plot all matchups with 1:1 line
ggplot(data=data, aes(x=sat, y=insitu))+
  geom_point(aes(color=type),size=3)+ #color observations by satellite type
  theme_classic()+ 
  theme(aspect.ratio = 1)+ #square plot
  geom_abline()+ #1:1 line
  xlab("Satellite Secchi Depth (m)")+
  ylab("In Situ Secchi Depth (m)")


L8_data<-data[data$type=="L8",];
S2_data<-data[data$type=="S2",];

model<-l8model #switch to s2model when testing s2model assumptions
data<-L8_data #switch to S2_data
summary(model)

##Checking model assumptions

#standardize model residuals
Emodel<-rstandard(model)
#histogram of standardized residuals
hist(Emodel,xlab="Residuals", main="Histogram of Residuals")
#qqline to test for normality of residuals
qqnorm(Emodel)
qqline(Emodel)

#plot model residuals by fitted model values to test for homogeneity of variance
plot(x=fitted(model),y=resid(model,type="pearson"))
abline(a=0, b=0, lty=3)
#boxplot to view mean and distribution by site
boxplot(data$insitu~data$site, xlab="Site", ylab="In Situ Secchi Depth (m)")

#Autocorrelation by site
#split data by site
split.site<-split(data,data$site)
#set margin sizes for autocorrelation function (ACF) graphs
par(mar=c(1,1,1,1))
#S3 class of indexed totally ordered observations of irregular time series. Done by site
ts2<-zoo(split.site$'2'$insitu,split.site$'2'$date)
#autocorrelation function (ACF) graph
acf(ts2, na.action=na.omit, main="Auto-correlation plot for residuals, Site 2")
ts5<-zoo(split.site$'5'$insitu,split.site$'5'$date)
acf(ts5, na.action=na.pass, main="Auto-correlation plot for residuals, Site 5")
#had to remove duplice in situ values because zoo orders by in situ Secchi depth and cannot handle values that are not unique
ts6<-zoo(split.site$'6'[split.site$'6'$insitu!=1.05,]$insitu,split.site$'6'[split.site$'6'$insitu!=1.05,]$date)
acf(ts6, na.action=na.pass, main="Auto-correlation plot for residuals, Site 6")
ts12<-zoo(split.site$'12'$insitu,split.site$'12'$date)
acf(ts12, na.action=na.pass, main="Auto-correlation plot for residuals, Site 12")
ts13<-zoo(split.site$'13'$insitu,split.site$'13'$date)
acf(ts13, na.action=na.pass, main="Auto-correlation plot for residuals, Site 13")
ts16<-zoo(split.site$'16'$insitu,split.site$'16'$date)
acf(ts16, na.action=na.pass, main="Auto-correlation plot for residuals, Site 16")
ts17<-zoo(split.site$'17'$insitu,split.site$'17'$date)
acf(ts17, na.action=na.pass, main="Auto-correlation plot for residuals, Site 17")

#Model stats
summary(model) #model summary
car::Anova(model,type="III") #type three sum of squares
#generate regression and ANOVA tables in APA format
apa.reg.table(model, filename = "RegTable_APA.doc", table.number = 2)
apa.aov.table(model, filename = "AOVTable_APA.doc", table.number = 4)

#Predictions from model 

#create new data frame for predictions
preds <- expand.grid(insitu = NA,
                     sat = seq(from = min(data$sat), to = max(data$sat), by = (max(data$sat) - min(data$sat)) / 100),
                     site = unique(data$site),
                     type=unique(data$type))
#predict secchi depth values (in situ values, y) from model (with satellite secchi depths as predictor variable, x). generate confidence and prediction intervals
insitupreds <- data.frame(predict(model, newdata = preds, interval='confidence'), preds$sat,preds$type)
insitupreds2 <- data.frame(predict(model , newdata = preds, interval='prediction'), preds$sat,preds$type)
#generate fit for graphing model
insitupreds$preds.insitu<-insitupreds$fit #confidence intervals
insitupreds2$preds.insitu<-insitupreds2$fit #prediction intervals


#same procedure as above, just for separate satellie models.
#create new data frame for predictions
l8preds <- expand.grid(insitu = NA,
                       sat = seq(from = min(L8_data$sat), to = max(L8_data$sat), by = (max(L8_data$sat) - min(L8_data$sat)) / 100))
s2preds <- expand.grid(insitu = NA,
                       sat = seq(from = min(S2_data$sat), to = max(S2_data$sat), by = (max(S2_data$sat) - min(S2_data$sat)) / 100))
insitupredsl8 <- data.frame(predict(l8model, newdata = l8preds, interval='confidence'), l8preds$sat)
insitupredss2 <- data.frame(predict(s2model , newdata = s2preds, interval='confidence'), s2preds$sat)
#generate fit for graphing model
insitupredsl8$preds.insitu<-insitupredsl8$fit #confidence intervals
insitupredss2$preds.insitu<-insitupredss2$fit #prediction intervals

L8_data$preds<-predict(l8model)
rmse(L8_data$insitu,L8_data$preds)
mape(L8_data$insitu,L8_data$preds)

S2_data$preds<-predict(s2model)
rmse(S2_data$insitu,S2_data$preds)
mape(S2_data$insitu,S2_data$preds)

#include predictions into main dataframe
data$preds <- NA
data$preds<-predict(model)

#plotting residuals
data$resid<-NA
data$resid<-data$preds-data$insitu
ggplot(aes(x=sat,y=resid),data=data)+
  geom_point()+
  ylab("Residuals (Model Zsd - In situ Zsd)")+
  xlab("Satellite Zsd (m)")+theme_classic()

#Root mean square error calculations, by site and region
rmse(data$insitu,data$preds) #adjusted satellite algorithm models
rmse(data$insitu,data$sat) #unadjusted satellite algorithm models

rmse(data[data$region=="OI",]$insitu,data[data$region=="OI",]$preds)
rmse(data[data$region=="L",]$insitu,data[data$region=="L",]$preds)
rmse(data[data$region=="MC",]$insitu,data[data$region=="MC",]$preds)


# Mean absolute percent error (difference) calculations
mape(data$insitu,data$preds) #adjusted satellite algorithm models

mape(data$insitu,data$sat) #unadjusted satellite algorithm models

mape(data[data$region=="OI",]$insitu,data[data$region=="OI",]$preds)
mape(data[data$region=="L",]$insitu,data[data$region=="L",]$preds)
mape(data[data$region=="MC",]$insitu,data[data$region=="MC",]$preds)

#Plot predictions vs. observations
#r<-data.frame(r="r = 0.57") #wrong now
satlabels<- c(L8 = "Landsat-8", S2 = "Sentinel-2")
ggplot(data=data,aes(x=insitu,y=preds))+
  geom_point(aes(color=type))+
  scale_colour_manual(breaks = c("L8", "S2"),
                      values = c("forestgreen", "darkorange2"),
                      name="",
                      labels=satlabels) +
  geom_abline()+
  theme_classic()+
  guides(color=guide_legend(title="Satellite"))+
  theme(legend.title=element_text(size=12),legend.text=element_text(size=12),legend.position="bottom")+
  xlab("Model predictions of Secchi depth (m)")+
  labs(y=expression(paste(italic("In situ"), " Secchi depth (m)")))+ #use "expression" with "paste" to italicize in situ
  geom_text(data=r, mapping=aes(x=0.4, y=1.5, label=r), size=6, hjust   = 0, vjust   = 0)+
  theme(aspect.ratio = 1)+
  ylim(0.4,1.6)+
  xlim(0.4,1.6)
ggsave(file="Modelpreds.pdf",width=5,height=5)

#correlation coefficient
cor.test(data$insitu,data$preds)
#reliability index, https://www.rdocumentation.org/packages/qualV/versions/0.3-3/topics/GRI
library(qualV)
GRI(data$insitu,data$preds)
#modeling efficiency, http://www.imsbio.co.jp/RGM/R_rdfile?f=hydroGOF/man/hydroGOF-package.Rd&d=R_CC, https://www.rdocumentation.org/packages/hydroGOF/versions/0.4-0/topics/NSE
library(hydroGOF)
#idk if the code matches though
NSE(data$preds,data$insitu)
mNSE(data$preds,data$insitu)

#Plot of in situ Secchi depth vs Satellite Secchi depth 
mainlabels<-data.frame(rmse="RMSE = 0.16 m",mapd="MAPD = 19%",nl="N (L8) = 11", dash="--- 1:1 line")

l8<-ggplot(data=data[data$type=="L8",], aes(x=sat,y=insitu)) + 
  #color by type, put in aes command so ggplot considers this information as part of the data. this means that there will be a legend displaying the information
  geom_point(color="forestgreen", size=2)+
  theme_classic()+ #white, no gridlines
  xlab("Landsat-8 Secchi depth (m)")+
  theme(axis.title.x = element_text(size=16), axis.text.x=element_text(size=16))+ #defining attributes for x axis label
  labs(y=expression(paste(italic("In situ"), " Secchi depth (m)")))+
  ylim(0,1.7)+
  xlim(0,3.5)+
  theme(axis.title.y = element_text(size=16),axis.text.y=element_text(size=16))+ #defining attributes for y axis label
  #layer to graph confidence intervals, alpha specifies how light/dark the color of the ribbon is:
  geom_ribbon(data=insitupredsl8, aes(x=l8preds.sat, y=fit, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) +
  #geom_ribbon(data=insitupreds[insitupreds$preds.type=="L8",], aes(x=preds.sat, y=preds.insitu, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) + #graph confidence intervals
  #prediction line:
  # geom_line(data = insitupreds[insitupreds$preds.type=="L8",], aes(x = preds.sat, y =preds.insitu), size = 1,color="forestgreen")
  geom_line(data = insitupredsl8, aes(x = l8preds.sat, y =fit), size = 1,color="forestgreen") + #prediction line, from landsat-8 specific predictions
  geom_abline(linetype="dashed", size=1)+ #1:1 line+plotting the outlier with an asterick (shape=8)
  theme(aspect.ratio = 1) +
  labs(subtitle = expression(paste(R[adj]^2 == "38.6%")))+ #use expression command to plot "adj" as subscript and 2 as superscript
  theme(plot.subtitle=element_text(size=18, hjust=0.08, vjust=-7.5))+ #plot and adjust position of R2
  #plotting text- define position of each string
  geom_text(data=mainlabels, mapping=aes(x=0.1, y=1.4+0.04, label=mapd), size=6, hjust   = 0, vjust   = 0)+
  geom_text(data=mainlabels, mapping=aes(x=0.1, y=1.26+0.04, label=rmse), size=6, hjust   = 0, vjust   = 0)+
  geom_text(data=mainlabels, mapping=aes(x=0.1, y=1.1+0.04, label=nl), size=6, hjust   = 0, vjust   = 0)+
  geom_text(data=mainlabels, mapping=aes(x=0.1, y=0.9+0.04, label=dash), size=6, hjust   = 0, vjust   = 0)+
  theme(aspect.ratio = 0.45)

l8

mainlabels<-data.frame(rmse="RMSE = 0.22 m",mapd="MAPD = 26%",ns="N (S2) = 40", dash="--- 1:1 line")


se2<-ggplot(data=data[data$type=="S2",], aes(x=sat,y=insitu)) + 
  geom_point(color="darkorange2", size=2)+
  theme_classic()+ #white, no gridlines
  xlab("Sentinel-2 Secchi depth (m)")+
  theme(axis.title.x = element_text(size=16),axis.text.x=element_text(size=16))+ 
  labs(y=expression(paste(italic("In situ"), " Secchi depth (m)")))+
  ylim(0,1.7)+
  xlim(0,3.5)+
  theme(axis.title.y = element_text(size=16),axis.text.y=element_text(size=16))+ #defining attributes for y axis label
  #geom_ribbon(data=insitupreds[insitupreds$preds.type=="S2",], aes(x=preds.sat, y=preds.insitu, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) + #graph confidence intervals
  geom_ribbon(data=insitupredss2, aes(x=s2preds.sat, y=fit, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) + #graph confidence intervals
  geom_line(data = insitupredss2, aes(x = s2preds.sat, y =fit), size = 1,color="darkorange2") +
  # geom_line(data = insitupreds[insitupreds$preds.type=="S2",], aes(x = preds.sat, y =preds.insitu), size = 1,color="darkorange2") + #line of predicted values
  geom_abline(linetype="dashed", size=1)+
  theme(aspect.ratio = 1) +
  labs(subtitle = expression(paste(R[adj]^2 == "31.9%")))+ #use expression command to plot "adj" as subscript and 2 as superscript
  theme(plot.subtitle=element_text(size=18, hjust=0.08, vjust=-7.5))+ #plot and adjust position of R2
  #plotting text- define position of each string
  geom_text(data=mainlabels, mapping=aes(x=0.1, y=1.4+0.04, label=mapd), size=6, hjust   = 0, vjust   = 0)+
  geom_text(data=mainlabels, mapping=aes(x=0.1, y=1.26+0.04, label=rmse), size=6, hjust   = 0, vjust   = 0)+
  geom_text(data=mainlabels, mapping=aes(x=0.1, y=1.1+0.04, label=ns), size=6, hjust   = 0, vjust   = 0)+
  geom_text(data=mainlabels, mapping=aes(x=0.1, y=0.9+0.04, label=dash), size=6, hjust   = 0, vjust   = 0)+
  theme(aspect.ratio = 0.45)

both<-l8+se2 #patchwork: https://patchwork.data-imaginist.com/
ggsave(filename="both.pdf",both,width=15,height=10)

# plots by regions and depth
rmse(data[data$catdepth=="shallow",]$insitu,data[data$catdepth=="shallow",]$preds)
rmse(data[data$catdepth=="intermediate",]$insitu,data[data$catdepth=="intermediate",]$preds)
rmse(data[data$catdepth=="deep",]$insitu,data[data$catdepth=="deep",]$preds)

mape(data[data$catdepth=="shallow",]$insitu,data[data$catdepth=="shallow",]$preds)
mape(data[data$catdepth=="intermediate",]$insitu,data[data$catdepth=="intermediate",]$preds)
mape(data[data$catdepth=="deep",]$insitu,data[data$catdepth=="deep",]$preds)
#Plot by water type
#data frame with figure labels, and text to be plotted on graphs
regionlabels<-data.frame(depths=c("shallow","intermediate","deep"), regionlabs=c("Shallow", "Intermediate", "Deep"),rmseregions=c("RMSE = 0.18 m","RMSE = 0.19 m","RMSE = 0.24 m"), nregions=c("N = 20","N = 6", "N = 25"), mapdregions=c("MAPD = 22.1%","MAPD = 29.2%","MAPD = 25.8%")) 



shallow<-ggplot(data=data[data$catdepth=="shallow",], aes(x=sat,y=insitu)) + 
  geom_point(aes(color = type), size=2)+
  geom_abline(linetype="dashed", size=1)+
  scale_colour_manual(breaks = c("L8", "S2"),
                      values = c("forestgreen", "darkorange2"),
                      name="",
                      labels=satlabels) +
  guides(color=guide_legend(title="Satellite"))+ #adding legend title
  theme_classic()+ #white, no gridlines
  theme(legend.title=element_text(size=12),legend.text=element_text(size=12),legend.position="bottom")+ #legend attributes
  xlab("Satellite Secchi Depth (m)")+
  theme(axis.title.x = element_text(size=12), axis.text.x=element_text(size=12))+ #defining attributes for x axis label
  ylab("In Situ Secchi Depth (m)")+
  theme(axis.title.y = element_text(size=12),axis.text.y=element_text(size=12))+ 
  #geom_line(data = insitupreds[insitupreds$preds.type=="L8"&insitupreds$preds.sat<2.6,], aes(x = preds.sat, y =preds.insitu), size = 1,color="forestgreen") + #line of predicted values
  #geom_line(data = insitupreds[insitupreds$preds.type=="S2"&insitupreds$preds.sat<2.6,], aes(x = preds.sat, y =preds.insitu), size = 1,color="darkorange2") + #line of predicted values
  geom_text(data=regionlabels[regionlabels$depths=="shallow",], mapping=aes(x=0.1, y=1.53, label=regionlabs), size=5, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$depths=="shallow",], mapping=aes(x=0.1, y=1.38, label=mapdregions), size=4, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$depths=="shallow",], mapping=aes(x=0.1, y=1.27, label=rmseregions), size=4, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$depths=="shallow",], mapping=aes(x=0.1, y=1.16, label=nregions), size=4, hjust   = 0, vjust   = 0)+
  #  geom_ribbon(data=insitupredsl8, aes(x=l8preds.sat, y=fit, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) +
  #geom_ribbon(data=insitupreds[insitupreds$preds.type=="L8",], aes(x=preds.sat, y=preds.insitu, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) + #graph confidence intervals
  #prediction line:
  # geom_line(data = insitupreds[insitupreds$preds.type=="L8",], aes(x = preds.sat, y =preds.insitu), size = 1,color="forestgreen")
  geom_line(data = insitupredsl8, aes(x = l8preds.sat, y =fit), size = 1,color="forestgreen") + #
  geom_line(data = insitupredss2, aes(x = s2preds.sat, y =fit), size = 1,color="darkorange2") +
  ylim(0,1.6)+
  xlim(0,3)+
  theme(aspect.ratio = 1)

#take out S2 prediction line, only two MC observations
int<-ggplot(data=data[data$catdepth=="intermediate",], aes(x=sat,y=insitu)) + 
  geom_point(aes(color = type), size=2)+
  geom_abline(linetype="dashed", size=1)+
  scale_colour_manual(breaks = c("L8", "S2"),
                      values = c("forestgreen", "darkorange2"),
                      name="",
                      labels=satlabels) +
  guides(color=guide_legend(title="Satellite"))+ #adding legend title
  theme_classic()+ #white, no gridlines
  theme(legend.title=element_text(size=12),legend.text=element_text(size=12),legend.position="bottom")+ #legend attributes
  xlab("Satellite Secchi Depth (m)")+
  theme(axis.title.x = element_text(size=12), axis.text.x=element_text(size=12))+ #defining attributes for x axis label
  ylab("In Situ Secchi Depth (m)")+
  theme(axis.title.y = element_text(size=12),axis.text.y=element_text(size=12))+
  #eom_line(data = insitupreds, aes(x = preds.sat, y =preds.insitu), size = 1,color="black") +
  geom_text(data=regionlabels[regionlabels$depths=="intermediate",], mapping=aes(x=0.1, y=1.53, label=regionlabs), size=5, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$depths=="intermediate",], mapping=aes(x=0.1, y=1.38, label=mapdregions), size=4, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$depths=="intermediate",], mapping=aes(x=0.1, y=1.27, label=rmseregions), size=4, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$depths=="intermediate",], mapping=aes(x=0.1, y=1.16, label=nregions), size=4, hjust   = 0, vjust   = 0)+
  ylim(0,1.6)+
  xlim(0,3)+
  geom_line(data = insitupredsl8, aes(x = l8preds.sat, y =fit), size = 1,color="forestgreen") + #
  geom_line(data = insitupredss2, aes(x = s2preds.sat, y =fit), size = 1,color="darkorange2") +
  theme(aspect.ratio = 1)


deep<-ggplot(data=data[data$catdepth=="deep",], aes(x=sat,y=insitu)) + 
  geom_point(aes(color = type), size=2)+
  geom_abline(linetype="dashed", size=1)+
  scale_colour_manual(breaks = c("L8", "S2"),
                      values = c("forestgreen", "darkorange2"),
                      name="",
                      labels=satlabels) +
  guides(color=guide_legend(title="Satellite"))+ #adding legend title
  theme_classic()+ #white, no gridlines
  theme(legend.title=element_text(size=12),legend.text=element_text(size=12),legend.position="bottom")+ #legend attributes
  xlab("Satellite Secchi Depth (m)")+
  theme(axis.title.x = element_text(size=12), axis.text.x=element_text(size=12))+ #defining attributes for x axis label
  ylab("In Situ Secchi Depth (m)")+
  theme(axis.title.y = element_text(size=12),axis.text.y=element_text(size=12))+ 
  #geom_line(data = insitupreds, aes(x = preds.sat, y =preds.insitu), size = 1,color="black") +
  geom_text(data=regionlabels[regionlabels$depths=="deep",], mapping=aes(x=0.1, y=1.53, label=regionlabs), size=5, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$depths=="deep",], mapping=aes(x=0.1, y=1.38, label=mapdregions), size=4, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$depths=="deep",], mapping=aes(x=0.1, y=1.27, label=rmseregions), size=4, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$depths=="deep",], mapping=aes(x=0.1, y=1.16, label=nregions), size=4, hjust   = 0, vjust   = 0)+
  ylim(0,1.6)+
  geom_line(data = insitupredsl8, aes(x = l8preds.sat, y =fit), size = 1,color="forestgreen") + #
  geom_line(data = insitupredss2, aes(x = s2preds.sat, y =fit), size = 1,color="darkorange2") +
  theme(aspect.ratio = 1)

#patchwork for more info see: https://patchwork.data-imaginist.com/articles/guides/assembly.html
depths<-wrap_plots(shallow,int,deep)+guide_area()+plot_layout(guides='collect')+plot_annotation(tag_levels='A')+plot_layout(ncol = 3) #ncol assigns number of columns to arrange plots in, plot_annotation assigns subplots labels A-D
ggsave(filename="depths.pdf",depths,width=12,height=8)


regionlabels<-data.frame(region=c("L","MC","OI"), regionlabs=c("Lagoon", "Mainland Creek", "Ocean Inlet"),rmseregions=c("RMSE = 0.17 m","RMSE = 0.18 m","RMSE = 0.22 m"), nregions=c("N = 8","N = 6", "N = 37"), mapdregions=c("MAPD = 23.6%","MAPD = 31.8%","MAPD = 23.8%")) 


#same as above, subsetted by "region"
#lagoon sites
lagoon<-ggplot(data=data[data$region=="L",], aes(x=sat,y=insitu)) + 
  geom_point(aes(color = type), size=2)+
  geom_abline(linetype="dashed", size=1)+
  scale_colour_manual(breaks = c("L8", "S2"),
                      values = c("forestgreen", "darkorange2"),
                      name="",
                      labels=satlabels) +
  guides(color=guide_legend(title="Satellite"))+ #adding legend title
  theme_classic()+ #white, no gridlines
  theme(legend.title=element_text(size=12),legend.text=element_text(size=12),legend.position="bottom")+ #legend attributes
  xlab("Satellite Secchi Depth (m)")+
  theme(axis.title.x = element_text(size=12), axis.text.x=element_text(size=12))+ #defining attributes for x axis label
  ylab("In Situ Secchi Depth (m)")+
  theme(axis.title.y = element_text(size=12),axis.text.y=element_text(size=12))+ 
  #geom_line(data = insitupreds[insitupreds$preds.type=="L8"&insitupreds$preds.sat<2.6,], aes(x = preds.sat, y =preds.insitu), size = 1,color="forestgreen") + #line of predicted values
  #geom_line(data = insitupreds[insitupreds$preds.type=="S2"&insitupreds$preds.sat<2.6,], aes(x = preds.sat, y =preds.insitu), size = 1,color="darkorange2") + #line of predicted values
  geom_text(data=regionlabels[regionlabels$region=="L",], mapping=aes(x=0.1, y=1.53, label=regionlabs), size=5, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$region=="L",], mapping=aes(x=0.1, y=1.38, label=mapdregions), size=4, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$region=="L",], mapping=aes(x=0.1, y=1.27, label=rmseregions), size=4, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$region=="L",], mapping=aes(x=0.1, y=1.16, label=nregions), size=4, hjust   = 0, vjust   = 0)+
  #  geom_ribbon(data=insitupredsl8, aes(x=l8preds.sat, y=fit, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) +
  #geom_ribbon(data=insitupreds[insitupreds$preds.type=="L8",], aes(x=preds.sat, y=preds.insitu, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) + #graph confidence intervals
  #prediction line:
  # geom_line(data = insitupreds[insitupreds$preds.type=="L8",], aes(x = preds.sat, y =preds.insitu), size = 1,color="forestgreen")
  geom_line(data = insitupredsl8, aes(x = l8preds.sat, y =fit), size = 1,color="forestgreen") + #
  geom_line(data = insitupredss2, aes(x = s2preds.sat, y =fit), size = 1,color="darkorange2") +
  ylim(0,1.6)+
  xlim(0,3)+
  theme(aspect.ratio = 1)

#take out S2 prediction line, only two MC observations
mainlandcreek<-ggplot(data=data[data$region=="MC",], aes(x=sat,y=insitu)) + 
  geom_point(aes(color = type), size=2)+
  geom_abline(linetype="dashed", size=1)+
  scale_colour_manual(breaks = c("L8", "S2"),
                      values = c("forestgreen", "darkorange2"),
                      name="",
                      labels=satlabels) +
  guides(color=guide_legend(title="Satellite"))+ #adding legend title
  theme_classic()+ #white, no gridlines
  theme(legend.title=element_text(size=12),legend.text=element_text(size=12),legend.position="bottom")+ #legend attributes
  xlab("Satellite Secchi Depth (m)")+
  theme(axis.title.x = element_text(size=12), axis.text.x=element_text(size=12))+ #defining attributes for x axis label
  ylab("In Situ Secchi Depth (m)")+
  theme(axis.title.y = element_text(size=12),axis.text.y=element_text(size=12))+
  #eom_line(data = insitupreds, aes(x = preds.sat, y =preds.insitu), size = 1,color="black") +
  geom_text(data=regionlabels[regionlabels$region=="MC",], mapping=aes(x=0.1, y=1.53, label=regionlabs), size=5, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$region=="MC",], mapping=aes(x=0.1, y=1.38, label=mapdregions), size=4, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$region=="MC",], mapping=aes(x=0.1, y=1.27, label=rmseregions), size=4, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$region=="MC",], mapping=aes(x=0.1, y=1.16, label=nregions), size=4, hjust   = 0, vjust   = 0)+
  ylim(0,1.6)+
  xlim(0,3)+
  geom_line(data = insitupredsl8, aes(x = l8preds.sat, y =fit), size = 1,color="forestgreen") + #
  geom_line(data = insitupredss2, aes(x = s2preds.sat, y =fit), size = 1,color="darkorange2") +
  theme(aspect.ratio = 1)


oceaninlet<-ggplot(data=data[data$region=="OI",], aes(x=sat,y=insitu)) + 
  geom_point(aes(color = type), size=2)+
  geom_abline(linetype="dashed", size=1)+
  scale_colour_manual(breaks = c("L8", "S2"),
                      values = c("forestgreen", "darkorange2"),
                      name="",
                      labels=satlabels) +
  guides(color=guide_legend(title="Satellite"))+ #adding legend title
  theme_classic()+ #white, no gridlines
  theme(legend.title=element_text(size=12),legend.text=element_text(size=12),legend.position="bottom")+ #legend attributes
  xlab("Satellite Secchi Depth (m)")+
  theme(axis.title.x = element_text(size=12), axis.text.x=element_text(size=12))+ #defining attributes for x axis label
  ylab("In Situ Secchi Depth (m)")+
  theme(axis.title.y = element_text(size=12),axis.text.y=element_text(size=12))+ 
  #geom_line(data = insitupreds, aes(x = preds.sat, y =preds.insitu), size = 1,color="black") +
  geom_text(data=regionlabels[regionlabels$region=="OI",], mapping=aes(x=0.1, y=1.53, label=regionlabs), size=5, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$region=="OI",], mapping=aes(x=0.1, y=1.38, label=mapdregions), size=4, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$region=="OI",], mapping=aes(x=0.1, y=1.27, label=rmseregions), size=4, hjust   = 0, vjust   = 0)+
  geom_text(data=regionlabels[regionlabels$region=="OI",], mapping=aes(x=0.1, y=1.16, label=nregions), size=4, hjust   = 0, vjust   = 0)+
  ylim(0,1.6)+
  geom_line(data = insitupredsl8, aes(x = l8preds.sat, y =fit), size = 1,color="forestgreen") + #
  geom_line(data = insitupredss2, aes(x = s2preds.sat, y =fit), size = 1,color="darkorange2") +
  theme(aspect.ratio = 1)

#patchwork for more info see: https://patchwork.data-imaginist.com/articles/guides/assembly.html
regions<-wrap_plots(lagoon,mainlandcreek,oceaninlet)+guide_area()+plot_layout(guides='collect')+plot_annotation(tag_levels='A')+plot_layout(ncol = 3) #ncol assigns number of columns to arrange plots in, plot_annotation assigns subplots labels A-D
ggsave(filename="region.pdf",regions,width=12,height=8)

