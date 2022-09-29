
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

atmocorrect<-read.csv('atmocorrect.csv')

bands<-read.csv("atmocorrect_bands_S2long.csv")

#Comparing Secchi depths
plot(atmocorrect$insitu,atmocorrect$acolite)
atmocorrect_no_outlier<-atmocorrect
#rmse and mapd- rather than using "rmse" and "mape", explicitly writing out formula in order to use na.rm (remove NA's)
sqrt(sum((atmocorrect$acolite - atmocorrect$insitu)^2 , na.rm = TRUE ) / 47)
sqrt(sum((atmocorrect$seadas - atmocorrect$insitu)^2 , na.rm = TRUE ) / 51) 

mean((abs(atmocorrect$insitu-atmocorrect$acolite)/atmocorrect$insitu),na.rm=TRUE)
mean((abs(atmocorrect$insitu-atmocorrect$seadas)/atmocorrect$insitu),na.rm=TRUE)


mean(atmocorrect_no_outlier$seadas,na.rm=TRUE)
sd(atmocorrect_no_outlier$seadas,na.rm=TRUE)

mean(atmocorrect_no_outlier$acolite,na.rm=TRUE)
sd(atmocorrect_no_outlier$acolite,na.rm=TRUE)

mean(data$insitu,na.rm=TRUE)
sd(data$insitu,na.rm=TRUE)


sdas<-lm(insitu~seadas,data=atmocorrect_no_outlier) #single atmo correction model, using satellite zsd from SeaDAS to predict in situ zsd
summary(sdas)

data=atmocorrect_no_outlier%>%
  dplyr::filter(!is.na(seadas));
preds <- expand.grid(insitu = NA,
                     seadas = seq(from = min(data$seadas), to = max(data$seadas), by = (max(data$seadas) - min(data$seadas)) / 100),
                     site = unique(data$site),
                     type=unique(data$type))
insitupreds <- data.frame(predict(sdas, newdata = preds, interval='confidence'), preds$seadas,preds$type)
#generate fit for graphing model
insitupreds$preds.insitu<-insitupreds$fit #confidence intervals

adas<-lm(insitu~acolite,data=atmocorrect_no_outlier) #single atmo correction model, using satellite zsd from SeaDAS to predict in situ zsd
summary(adas)

adata1<-atmocorrect_no_outlier[1:38,]
adata2<-atmocorrect_no_outlier[90:102,]
adata<-rbind(adata1,adata2)
adata<-adata[,3:4]
adata<-na.omit(adata)
adata$preds <- NA
adata$preds<-predict(adas)

satlabels<- c(L8 = "Landsat-8", S2 = "Sentinel-2")


preds2 <- expand.grid(insitu = NA,acolite = seq(from = min(adata$acolite), to = max(adata$acolite), by = (max(adata$acolite) - min(adata$acolite)) / 100))
#predict secchi depth values (in situ values, y) from model (with satellite secchi depths as predictor variable, x). generate confidence and prediction intervals
insitupreds23 <- data.frame(predict(adas, newdata = preds2, interval='confidence'), preds2$acolite)
insitupreds22 <- data.frame(predict(adas , newdata = preds2, interval='prediction'), preds2$acolite)
#generate fit for graphing model
insitupreds23$preds.insitu<-insitupreds23$fit #confidence intervals
insitupreds22$preds.insitu<-insitupreds22$fit #prediction intervals

labels <- c(acolite = "ACOLITE",dash="1:1 Line")

ac<-ggplot(data=atmocorrect_no_outlier)+
  geom_point(data=atmocorrect_no_outlier[atmocorrect_no_outlier$type=="S2",],aes(x=acolite, y=insitu),color="darkorange2")+ 
  geom_point(data=atmocorrect_no_outlier[atmocorrect_no_outlier$type=="L8",],aes(x=acolite, y=insitu),color="forestgreen")+ 
  #geom_smooth(aes(x=acolite,color='acolite'),method='lm',se=F)+ #plotting regression line, assigning color to 'acolite' to be assigned below in scale_colour_manual
  geom_line(data = insitupreds23, aes(x=preds2.acolite, y=fit), size = 1,color="black") +
  geom_abline(aes(slope=1,intercept=0,color='dash'),linetype="dashed",size=1,show.legend = FALSE)+#1:1 line
  geom_ribbon(data=insitupreds23, aes(x=preds2.acolite, y=fit, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) +
  #assigning colors to geom_smooths
  scale_colour_manual(breaks = c("L8", "S2"),
                      values = c("forestgreen", "darkorange2"),
                      name="",
                      labels=satlabels) +
  #plotting text
  #geom_text(data=text, mapping=aes(x=2, y=1.1, label=seadas), size=4.5, hjust   = 0, vjust   = 0)+
  #geom_text(data=text, mapping=aes(x=2, y=1.2, label=acolite), size=4.5, hjust   = 0, vjust   = 0)+
  xlab("Satellite Secchi depth (m)")+
  labs(y=expression(paste(italic("In situ"), " Secchi depth (m)")))+
  theme(axis.title.y = element_text(size=20),axis.title.x = element_text(size=20))+
  theme_classic()+
  
  scale_x_continuous(limits=c(0,3.2),breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(limits=c(0,2),breaks = scales::pretty_breaks(n = 6))+
  #geom_ribbon(data=insitupreds, aes(y=preds.sat, x=preds.insitu, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) + #graph confidence intervals
  #geom_ribbon(data=insitupreds23, aes(y=preds2.acolite, x=preds.insitu, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) + #graph confidence intervals
  theme(legend.position="bottom",legend.text=element_text(size=12))


labels <- c(seadas = "l2gen",dash="1:1 Line")
sea<-ggplot(data=atmocorrect_no_outlier,aes(y=insitu))+
  #green points from seadas
  geom_point(data=atmocorrect_no_outlier[atmocorrect_no_outlier$type=="S2",],aes(x=seadas),color="darkorange2")+ 
  geom_point(data=atmocorrect_no_outlier[atmocorrect_no_outlier$type=="L8",],aes(x=seadas),color="forestgreen")+
  geom_ribbon(data=insitupreds, aes(x=preds.seadas, y=preds.insitu, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) + #graph confidence intervals
  geom_line(data = insitupreds, aes(x = preds.seadas, y =preds.insitu), size = 1,color="black") + #line of predicted values
  #geom_smooth(aes(y=seadas,color='seadas'),method='lm',se=F)+ #plotting regression line, assigning color to 'seadas' to be assigned below in scale_colour_manual
  #geom_smooth(aes(y=acolite,color='acolite'),method='lm',se=F)+ #plotting regression line, assigning color to 'acolite' to be assigned below in scale_colour_manual
  geom_abline(aes(slope=1,intercept=0,color='dash'),linetype="dashed",size=1,show.legend = FALSE)+#1:1 line
  #assigning colors to geom_smooths
  scale_colour_manual(breaks = c("L8", "S2"),
                      values = c("forestgreen", "darkorange2"),
                      name="",
                      labels=satlabels) +
  #plotting text
  #geom_text(data=text, mapping=aes(x=2, y=1.1, label=seadas), size=4.5, hjust   = 0, vjust   = 0)+
  #geom_text(data=text, mapping=aes(x=2, y=1.2, label=acolite), size=4.5, hjust   = 0, vjust   = 0)+
  xlab("Satellite Secchi depth (m)")+
  labs(y=expression(paste(italic("In situ"), " Secchi depth (m)")))+
  theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20))+
  theme_classic()+
  
  scale_x_continuous(limits=c(0,3.2),breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(limits=c(0,2),breaks = scales::pretty_breaks(n = 6))+
  #geom_ribbon(data=insitupreds, aes(y=preds.sat, x=preds.insitu, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) + #graph confidence intervals
  #geom_ribbon(data=insitupreds23, aes(y=preds2.acolite, x=preds.insitu, ymin=lwr, ymax=upr), fill="grey", alpha=0.4) + #graph confidence intervals
  theme(legend.position="bottom",legend.text=element_text(size=12))
#

sea+ac
ggsave("SeaDASvsAcolite.pdf",width=8,height=4) #save pdf
# function to obtain R-Squared from the data

#seadas combined satellite model rmse and mape
rmse(data$insitu,data$preds)
mape(data$insitu,data$preds)

#acolite combined satellite model rmse and mape
rmse(adata$insitu,adata$preds)
mape(adata$insitu,adata$preds)

# bootstrapping with 1000 replications
set.seed(980)
results <- boot(data=atmocorrect_no_outlier, statistic=bs, R=10000, formula=insitu~acolite)

# view results
results
boot.ci(results, type="bca",index=1) #intercept
boot.ci(results, type="bca",index=2) #slope

set.seed(980)
results <- boot(data=atmocorrect_no_outlier, statistic=bs, R=1000, formula=insitu~seadas)
# view results
results

plot(results,index=1)

# get 95% confidence interval
boot.ci(results, type="bca",index=1) #intercept
boot.ci(results, type="bca",index=2) #slope

set.seed(980)
results1 <- boot(data=atmocorrect_no_outlier, statistic=rsq,
                 R=1000, formula=insitu~acolite)

# view results
results1

set.seed(980)
results1 <- boot(data=atmocorrect_no_outlier, statistic=rsq,
                 R=1000, formula=insitu~seadas)

# view results
results1

plot(results1)

# get 95% confidence interval
boot.ci(results1, type="bca")


results2 <- boot(data=atmocorrect_no_outlier, statistic=stnderr,
                 R=1000, formula=insitu~acolite)

# view results
results2

results2 <- boot(data=atmocorrect_no_outlier, statistic=stnderr,
                 R=1000, formula=insitu~seadas)

# view results
results2

plot(results2)

# get 95% confidence interval
boot.ci(results2, type="bca")

# plot spectra

bands$site<-factor(bands$site)
bands$rrs[bands$rrs<0]=NA
bands$rrs[bands$site=="1"]=NA
bands$rrs[bands$site=="9"]=NA
bands$rrs[bands$site=="4"]=NA
bands$rrs[bands$site=="10"]=NA
bands$rrs[bands$site=="11"]=NA
bands<-na.omit(bands)
bands$date<-factor(bands$date)

#bands$wavelength<-factor(bands$wavelength)
summary(bands)

sub_AC_data<-bands[bands$date=="18-Oct-2016",]
sub_AC_data$wavelength<-factor(sub_AC_data$wavelength)
summary(sub_AC_data)

ggplot(data=bands[bands$date=="18-Oct-2016",],aes(x=wavelength,y=rrs))+
  geom_point(aes(color = site), size=2)+
  scale_colour_manual(breaks = c("2","3","5","6","7","8","12","13","14","15","16","17"),
                      values = c("red", "orange","yellow","green","forestgreen","lightblue","darkblue","purple2","magenta","pink","pink3","gray"),
                      name="") +
  geom_line(linetype="solid",data=bands[bands$site=="2"&bands$ac=="seadas",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="dashed",data=bands[bands$date=="18-Oct-2016"&bands$site=="2"&bands$ac=="acolite",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="solid",data=bands[bands$date=="18-Oct-2016"&bands$site=="3"&bands$ac=="seadas",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="dashed",data=bands[bands$date=="18-Oct-2016"&bands$site=="3"&bands$ac=="acolite",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="solid",data=bands[bands$date=="18-Oct-2016"&bands$site=="5"&bands$ac=="seadas",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="dashed",data=bands[bands$date=="18-Oct-2016"&bands$site=="5"&bands$ac=="acolite",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="solid",data=bands[bands$date=="18-Oct-2016"&bands$site=="6"&bands$ac=="seadas",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="dashed",data=bands[bands$date=="18-Oct-2016"&bands$site=="6"&bands$ac=="acolite",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="solid",data=bands[bands$date=="18-Oct-2016"&bands$site=="7"&bands$ac=="seadas",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="dashed",data=bands[bands$date=="18-Oct-2016"&bands$site=="7"&bands$ac=="acolite",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="solid",data=bands[bands$date=="18-Oct-2016"&bands$site=="8"&bands$ac=="seadas",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="dashed",data=bands[bands$date=="18-Oct-2016"&bands$site=="8"&bands$ac=="acolite",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="solid",data=bands[bands$date=="18-Oct-2016"&bands$site=="12"&bands$ac=="seadas",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="dashed",data=bands[bands$date=="18-Oct-2016"&bands$site=="12"&bands$ac=="acolite",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="solid",data=bands[bands$date=="18-Oct-2016"&bands$site=="13"&bands$ac=="seadas",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="dashed",data=bands[bands$date=="18-Oct-2016"&bands$site=="13"&bands$ac=="acolite",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="solid",data=bands[bands$date=="18-Oct-2016"&bands$site=="14"&bands$ac=="seadas",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="dashed",data=bands[bands$date=="18-Oct-2016"&bands$site=="14"&bands$ac=="acolite",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="solid",data=bands[bands$date=="18-Oct-2016"&bands$site=="15"&bands$ac=="seadas",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="dashed",data=bands[bands$date=="18-Oct-2016"&bands$site=="15"&bands$ac=="acolite",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="solid",data=bands[bands$date=="18-Oct-2016"&bands$site=="16"&bands$ac=="seadas",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="dashed",data=bands[bands$date=="18-Oct-2016"&bands$site=="16"&bands$ac=="acolite",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="solid",data=bands[bands$date=="18-Oct-2016"&bands$site=="17"&bands$ac=="seadas",],aes(x=wavelength,y=rrs,color=factor(site)))+
  geom_line(linetype="dashed",data=bands[bands$date=="18-Oct-2016"&bands$site=="17"&bands$ac=="acolite",],aes(x=wavelength,y=rrs,color=factor(site)))+
  theme_bw()+
  guides(color=guide_legend(title="Site"))+ #adding legend title
  theme(legend.title=element_text(size=12),legend.text=element_text(size=12),legend.position="bottom")+ #legend attributes
  xlab("wavelength (nm)")+
  theme(axis.title.x = element_text(size=12), axis.text.x=element_text(size=12))+ #defining attributes for x axis label
  labs(y=expression(italic(paste(R[rs]," ",(sr^-1)))))+
  scale_x_continuous(breaks=c(443,490,560,665),labels=c("443","490","560","665"))+
  labs(title="18-October-2016 Sentinel-2A MSI")

ggsave("oct0261bands.pdf",width=6,height=6) #save pdf