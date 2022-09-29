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

comparedata<-read.csv("compare_L8S2.csv")

## satellite data assessment

secchidata <- comparedata %>%
  dplyr::filter(!is.na(s2_secchi)) %>%
  dplyr::filter(!is.na(l8_secchi))
lm(formula = l8_secchi~s2_secchi, data = secchidata)
#ggplot
both=ggplot(data=secchidata, aes(y=l8_secchi,x=s2_secchi))+
  #geom_point(aes(color=types2))+
  geom_point()+
  theme_classic()+
  xlim(0,4)+
  ylim(0,4)+
  geom_abline(lty=2)+
  geom_smooth(method='lm', formula= y~poly(x,1),color="red")+
  ylab("Landsat-8 Secchi depth (m)")+
  xlab("Sentinel-2 Secchi depth (m)")+
  theme(axis.title.x = element_text(size=16),axis.title.y = element_text(size=16),axis.text.y=element_text(size=14),axis.text.x=element_text(size=14))+
  labs(title="N = 547", subtitle = expression(paste(R[adj]^2 == "85.3%")), caption=expression(paste("y = ","0.72 x + 0.068")))+
  theme(plot.title=element_text(size=18,hjust=0.04,vjust=-13),plot.subtitle=element_text(size=18, hjust = 0.04, vjust=-15), plot.caption=element_text(size=18, hjust = 0.04, vjust=87, color="black"))+
  theme(aspect.ratio = 1)
ggsave(filename="satdiff.pdf",both,width=7,height=7)


both=ggplot(data=secchidata, aes(y=l8_kdmin,x=s2_kdmin))+
  #geom_point(aes(color=types2))+
  geom_point()+
  theme_classic()+
  geom_abline(lty=2)+
  geom_smooth(method='lm', formula= y~poly(x,1),color="red")+
  ylab("Landsat-8 Secchi depth (m)")+
  xlab("Sentinel-2 Secchi depth (m)")+
  theme(axis.title.x = element_text(size=16),axis.title.y = element_text(size=16),axis.text.y=element_text(size=14),axis.text.x=element_text(size=14))+
  theme(plot.title=element_text(size=18,hjust=0.04,vjust=-13),plot.subtitle=element_text(size=18, hjust = 0.04, vjust=-15), plot.caption=element_text(size=18, hjust = 0.04, vjust=87, color="black"))+
  theme(aspect.ratio = 1)
ggsave(filename="satdiff.pdf",both,width=7,height=7)


both=ggplot(data=secchidata, aes(y=l8_rrs,x=s2_rrs))+
  geom_point(aes(color=date))+
  #geom_point()+
  theme_classic()+
  geom_abline(lty=2)+
  geom_smooth(method='lm', formula= y~poly(x,1),color="red")+
  ylab("Landsat-8 Secchi depth (m)")+
  xlab("Sentinel-2 Secchi depth (m)")+
  theme(axis.title.x = element_text(size=16),axis.title.y = element_text(size=16),axis.text.y=element_text(size=14),axis.text.x=element_text(size=14))+
  labs(title="N = 547", subtitle = expression(paste(R[adj]^2 == "76.2%")), caption=expression(paste("y = ","0.70 x + 0.14")))+
  theme(plot.title=element_text(size=18,hjust=0.04,vjust=-13),plot.subtitle=element_text(size=18, hjust = 0.04, vjust=-15), plot.caption=element_text(size=18, hjust = 0.04, vjust=87, color="black"))+
  theme(aspect.ratio = 1)
ggsave(filename="satdiff.pdf",both,width=7,height=7)

