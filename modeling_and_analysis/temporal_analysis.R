
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
library(tidymv)
library(mgcv)
setwd('/Volumes/slangSSD/mcwc_R_mat')

ldata<-read.csv('L8_ALL.csv') 
ldata$SECCHI<-(ldata$SECCHI*0.25)+0.36

sdata<-read.csv('S2_ALL.csv') 
sdata$SECCHI<-(sdata$SECCHI*0.37)+0.1
sdata$TYPE<-'S2'

idata<-read.csv('in_DATA.csv')
idata$SECCHI<-(idata$SECCHI)/100

alldata<-rbind(ldata,sdata,idata)

alldata$DATE<-as.Date(alldata$DATE)
plot(alldata$DATE,alldata$SECCHI)

alldata$YEARDAY<-as.POSIXlt(alldata$DATE)$yday 

# switch between sites you want to look at
  
pol<-rbind(ldata,sdata)
look<-pol%>%
  dplyr::filter(SITE=="6")%>%
  dplyr::select(SECCHI)

look2<-na.omit(look$SECCHI)

# just switch b/w 2, 6, 16 so don't have to copy and paste code?
sat_timedata2<-alldata%>%
  dplyr::filter(SITE=="16")


plot(sat_timedata2$decimaldate,sat_timedata2$SECCHI)
plot(sat_timedata2$YEARDAY,sat_timedata2$SECCHI)

knots <- list(yearday= c(0.5, 366.5)) #to specifiy interval to assign knots over
satgam<-gam(SECCHI~s(YEARDAY,bs="cc",k=15)+s(decimaldate,bs="tp",k=15)+ti(YEARDAY,decimaldate,bs=c('cc','tp'),k=c(10,10)),data=sat_timedata2,method = 'REML', knots = knots) #model equation: s= smoothing factors for decimal date (interannual variability) and yearday (seasonal trends). Tensor product (ti) to account for interaction between the two
set.seed(1) #gam.check will produce different results because checking with randomized values, so use set.seed to keep constant 
gam.check(satgam) #if k-index is much lower than one, less of a chance that the model is accurate. want higer p-value
summary(satgam) 
plot(satgam, scheme=1)
plot(satgam, scheme=2)

modelpsat_dd<-get_gam_predictions(satgam,decimaldate,series_length = 300) #ALL predictions by decimal date, var names from old code
modelpsat_yd<-get_gam_predictions(satgam,YEARDAY,series_length = 365) #ALL predictions by yearday

legend<-c("in situ","Landsat-8 OLI", "Sentinel-2 MSI")

time=ggplot()+
  xlab("Time")+
  ylab("Secchi depth (m)")+
  geom_point(data=sat_timedata2,aes(x=decimaldate,y=SECCHI,color=TYPE),show.legend=FALSE)+ #raw data, in sample
  geom_ribbon(data=modelpsat_dd,aes(x=decimaldate,ymin=CI_lower,ymax=CI_upper),fill='gray',alpha=0.6)+ #plot confidence intervals, alpha sets transparency
  geom_line(data=modelpsat_dd,aes(x=decimaldate,y=SECCHI),color='black',size=1)+ #line created from model predictions
  scale_x_continuous(limits=c(2013,2021),breaks = scales::pretty_breaks(n=10)) +#assigning number of labeled tick marks on x axis
  theme_classic()+
  theme(plot.title = element_text(size=18))+
  #labs(title = "Interannual Variability")+
  theme(aspect.ratio = 0.7)+
  #xlim(2013,2021)+
  ylim(0,2)+
  scale_color_manual(values = c("blue", "forestgreen","darkorange2"))
  #guides(fill=guide_legend(title="Site Type"))+
  #theme(legend.position="bottom")
time
ggsave(filename="time16.pdf",time,width=5,height=3)


season=ggplot()+
  xlab("Year day")+
  ylab("Secchi depth (m)")+
  geom_point(data=sat_timedata2,aes(x=YEARDAY,y=SECCHI,color=TYPE),show.legend=FALSE)+
  geom_ribbon(data=modelpsat_yd,aes(x=YEARDAY,ymin=CI_lower,ymax=CI_upper),fill='gray',alpha=0.6)+#plot confidence intervals, alpha sets transparency
  geom_line(data=modelpsat_yd,aes(x=YEARDAY,y=SECCHI),color='black',size=1)+#line created from model predictions
  scale_x_continuous(limits=c(0,365),breaks = scales::pretty_breaks(n = 10)) +
  theme_classic()+
  ylim(0,2)+
  #labs(title = "    Seasonal Cycle")+
  theme(plot.title = element_text(size=18))+
  theme(aspect.ratio = 0.7)+
  scale_color_manual(values = c("blue", "forestgreen","darkorange2"))
  #guides(fill=guide_legend(title="Site type"))+
  #theme(legend.position="bottom") 
season
ggsave(filename="season16.pdf",season,width=5,height=3)



