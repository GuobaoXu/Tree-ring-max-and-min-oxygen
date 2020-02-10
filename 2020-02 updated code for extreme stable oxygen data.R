##This code is used to process the meteorological data and climate analysis for the tree-ring stable oxygen isotope extreme values
## The aims are:
###     1. Process the meteorological data and climate response.
###     2. Detect the signal of the tree-ring stable oxygen isotope extreme 
###     3. Climate reconstruction and analysis
###
## Author: GB Xu, xgb234@lzb.ac.cn
## Date: 2019-6-14

##  Part 0 Initial call the packages-----

library(openxlsx)
library(dplyr)
library(reshape2)
library(ggplot2)
library(treeclim)
library(grid)
library(dplR)
library(treeclim)
library(MASS)
library(yhat)
library(ggpubr)

## Part 1. Process the climate data --------

## 1.1 read and load the climate data-----
mdata<-read.table("E:/Rwork/Freezingrain/S201806051426312322400.txt",header = TRUE)
ny.mdata<-subset(mdata,mdata$V01000==57776)
ny.mdata[ny.mdata==32766]<-NA
ny.mymdata.full<-subset(ny.mdata,select = c(1:3,11,17,21,23,26,6,14,5))

varname<-c("month","station","year","pre.day",
           "tmean","water.pressure","rh","pre","tmin","tmax","ssd")
colnames(ny.mymdata.full)<-varname
ny.mymdata.mean <- ny.mymdata.full %>%
                      filter(year>1952)%>%
                      group_by(month)%>%
                      summarise_each(funs(mean(.,na.rm=TRUE)))

## 1.1.1 processing the missing data and write as ssd1 and evp.-----
library(mice)
imp<-mice(ny.mymdata[,c(1,3,11)],10)
fit<-with(imp,lm(ssd~month))
pooled<-pool(fit)
result4=complete(imp,action=3)
ny.mymdata$ssd1<-result4$ssd

evp<-read.xlsx("E:/Rwork/Freezingrain/evp57776.xlsx")
head(evp)
evp$V8[evp$V8 == 32766] <- NA

evp.mean<-evp %>% group_by(V5,V6)%>% 
  summarise(mean.evp=mean(V8,na.rm=TRUE))
evp.mean<-data.frame(evp.mean)
colnames(evp.mean)<-c('year','month','evp')
head(evp.mean)

imp<-mice(evp.mean,10)
fit<-with(imp,lm(evp~month))
pooled<-pool(fit)
result4=complete(imp,action=3)
ny.mymdata$evp<-result4$evp[c(1:773)]
ny.mymdata<-subset(ny.mymdata,year>1952 & year<2015)


## precipitation, temperature and water vapor pressure unit is 0.1..
#####   1.1.2 calculate the VPD based on the temperature and RH----
ea_o=6.112*exp(17.67*(ny.mymdata$tmean*0.1)/((ny.mymdata$tmean*0.1)+243.5))# The unit of tem should be degress, the unit of ea is hpa.
vpd <- ea_o*(1-ny.mymdata$rh/100)
ny.mymdata$vpd <- vpd



#1.1.3 plot the climtagraph at month------
  library(plotrix)
 ### calculate the ratio between y1 and y2
  preclim<-c(50,300)
  tclim<-c(0.2,25)
  d<-diff(tclim)/diff(preclim)
  c<-preclim[1]-tclim[1]*d
  
   ny.mymdata.mean$pre1<-ny.mymdata.mean$pre/10
   ny.mymdata.mean$tmean1<-ny.mymdata.mean$tmean/10
   
   clima<-ggplot(data=ny.mymdata.mean,aes(x=month))+
   geom_bar(aes(y=pre1),
            stat = "identity",position = "identity")+
  geom_line (aes(y=c+(tmean1)/d),col="red")+
     geom_point(aes(y=c+(tmean1)/d),col="red")+
  xlab("Month")+
  scale_y_continuous("Precipitation (mm)",
                     sec.axis = sec_axis( ~ (. - c)*d, 
                                         name = "Temperature (℃)"), 
                     expand=c(0.01,0.05))+
     scale_x_continuous("Month", breaks = 1:12,expand=c(0.01,0.05)) +
  mythemeplot()+
  theme(plot.title = element_text(hjust = 0.5))+
   theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
     theme(plot.margin = unit(c(0,-0.2,0,0),"lines"))
  
 
   ssdclim<-c(30,230)
   rhclim <-c(70,95)
   s<-diff(rhclim)/diff(ssdclim) #3 becareful the relationship, y2 and y1
   r<-ssdclim[1]-rhclim[1]/s ## the relationship scale between rh and ssd. 
   
   climb<-ggplot(data=ny.mymdata.mean,aes(x=month))+
   geom_bar( aes(y=ssd/10),
            stat = "identity",position = "identity")+
     geom_line (aes(y=r+(rh)/s),col="blue")+
     geom_point(aes(y=r+(rh)/s),col="blue")+
  xlab("Month")+
  scale_y_continuous("SSD (h)", 
                     #limits = c(50,400),
                     sec.axis = sec_axis(~ (. - r)*s, 
                                         name = "Relative humidity (%)"),
expand=c(0.01,0.05) ) +
  scale_x_continuous("Month", breaks = 1:12,
                     expand=c(0.01,0.05)) +
  mythemeplot()+
  theme(plot.title = element_text(hjust = 0.5))+
   theme(axis.line.y.right = element_line(color = "blue"), 
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"), 
        axis.title.y.right = element_text(color = "blue")) +
   theme(plot.margin = unit(c(0,-0.1,0,0),"lines"))

## 1.1.4 load the scPDSI data from CRU---
 crupdsi<-read.table("./cru/iscpdsi_112.5-112.7E_27.27-27.5N_n.dat",
                 header = FALSE)
colnames(crupdsi)<-mon
crupdsi <- subset(crupdsi,year<2015)
 
 # 1.2 compare the d18O data between ISOGSM model and observation-----
  ## The precipitation d18O data from ISOGSM model
  ## The precipitation data from the GNIP Changsha station
  #### 1.2.1 Process the d18O data from Changsha Station-----
   oxy.changsha <- read.xlsx("./rawdata/wiser_gnip-monthly-cn-gnipm.xlsx",
                            sheet = "Data",colNames = TRUE)
  head(oxy.changsha)
  oxy.changsha.reshape <- subset(oxy.changsha,select=c(SampleName, month, O18))
  colnames(oxy.changsha.reshape) <- c("Var1","Var2","value")
  
  ##split the data from GNIP
  oxy.changsha.reshape.1999 <-subset(oxy.changsha.reshape,Var1>1999)
  
#### 1.2.2 Process the d18O data from ISOGSM data-----
#### a. for the precipitation -----
data <- read.delim("F:/IsoGSM/x061y062_ensda_monthly.dat",header = FALSE)
data1<-data[c(-1,-2),c(-1)]
data1.ts<-ts(data1,start = c(1871,1),frequency = 12)
p.oxyts<-ts((data1$V6/data1$V5-1)*1000,start = c(1871,1),frequency = 12)
p.oxy<-(data1$V6/data1$V5-1)*1000
p.oxy[abs(p.oxy)>13]<-NA ## remove the outliers, set the threshold is abs(13), which is based on the mean value of multi-year observation.

p.rate<-matrix(data1$V5,ncol=12,byrow=TRUE)
p.rateoxy<-matrix(p.oxy,ncol=12,byrow=TRUE)## here, to calculate the oxygen according to original data!!
##where SMOW=[H18O]/[H2O] or [HDO]/[H2O] in Standard Mean Ocean Water.
# To calculate delta18o in precipitation, do followings:
#   delta18O_p[permil]=(PRATE18O/PRATE-1)*1000
rownames(p.rateoxy)<-c(1871:2010)
p.tmp<-matrix(data1$V17,ncol=12,byrow=TRUE)
p.rh<-matrix(data1$V18,ncol=12,byrow=TRUE)

plot(data1.ts[,2])
lines(p.oxyts,col=2)

### b. process for the stable oxygen isotope of the water vapor at monthly scales-----
vp.oxy<-(data1$V15/data1$V14-1)*1000
vp.oxy[abs(vp.oxy)>30]<-NA ## remove the outliers, set the threshold is abs(30)
## reference: Xie Yulong, Zhang Xinping, et al., Monitoring and analysis of stable isotopes of the near surface water vapor in 
## Changsha, Environmental Science, 2016,37(2):475-481

monthvp.oxy<-as.data.frame(matrix(vp.oxy,ncol=12,byrow=TRUE))
colnames(monthvp.oxy)<-c(1:12)
monthvp.oxy<-cbind(year=c(1871:2010),monthvp.oxy)

p.rateoxy.shape<-melt(p.rateoxy) 

   p.rateoxy.shape.1988 <-
   subset(p.rateoxy.shape,Var1 >1987 & Var1 <1993)
  # p.rateoxy.shape.1988 <-
  #   subset(p.rateoxy.shape,Var1 %in% oxy.changsha.reshape$Var1)
  
  p.oxy <- rbind(oxy.changsha.reshape.1999,
                 p.rateoxy.shape.1988[order(p.rateoxy.shape.1988$Var1),])
  p.oxy$type <- c(rep("Changsha",60),
                  rep("Model",60))
  p.oxy$date <- c(seq.Date(from = as.Date('1988-01-01'),by = 'month', length.out = 60),
                  seq.Date(from = as.Date('1988-01-01'),by = 'month', length.out = 60))
  
  oxy.p <- ggplot(p.oxy,aes(x=Var2,y=value, na.rm=TRUE,color=type))+
    geom_point()+
    geom_smooth(method="loess",se=TRUE,lty=1,lwd=1.5,aes(fill =type))+
    xlab("Month")+ylab(expression(paste(delta ^"18","O (‰)")))+
    scale_x_continuous(limits = c(1,12),breaks=c(1:12), 
                       labels = c(1:12))+
    mythemeplot()+
    theme(legend.position = c(0.2,0.15),legend.title = element_blank())+
    theme(plot.margin = unit(c(0,0,0,0),"lines"))
  
  
 ## Part 2 Tree-ring stable oxygen isotope data load and plot-----
 ##  This part is show the stable oxygen isotope 
 ## 
 
 ## 2.1 plot the position of the extreme values------
 
  
  stabe.all.o.max <- read.xlsx("E:/Rwork/highresolution/rawdata/omax.xlsx")
  stabe.allEW.o.max <- read.xlsx("E:/Rwork/highresolution/rawdata/allEWomax.xlsx")
  stabe.allLW.o.max <- read.xlsx("E:/Rwork/highresolution/rawdata/allLWomax.xlsx")
  stabe.all.o.min <- read.xlsx("E:/Rwork/highresolution/rawdata/omin.xlsx")
  
 max.oplot<- ggplot(stabe.all.o.max, aes(x=V4, color=wood)) +
  geom_histogram(fill="white", alpha=0.5, 
                 position="identity",binwidth = 0.1)+
  mythemeplot()+
  xlab('Proportion to boundary')+ylab("count")+
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2,0,0,0),"lines"))+
  scale_x_continuous(
  labels = scales::number_format(accuracy = 0.1))+
  scale_color_manual(values=c(LW="darkgreen",
                                  EW="green"))

maxEW.oplot<- ggplot(stabe.allEW.o.max, aes(x=V4, color=wood)) +
  geom_histogram(fill="white", alpha=0.5, 
                 position="identity",binwidth = 0.1)+
  #scale_color_manual(values=c("#00BFC4"))+
  mythemeplot()+
  xlab('Proportion to boundary')+ylab("count")+
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2,0,0,0),"lines"))+
  scale_color_manual(values=c(LW="darkgreen",
                                  EW="green"))

maxLW.oplot<- ggplot(stabe.allLW.o.max, aes(x=V4, color=wood)) +
  geom_histogram(fill="white", alpha=0.5, 
                 position="identity",binwidth = 0.1)+
  #scale_color_manual(values=c("#00BFC4"))+
  mythemeplot()+
  xlab('Proportion to boundary')+ylab("count")+
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2,0,0,0),"lines"))+
  scale_color_manual(values=c(LW="darkgreen",
                                  EW="green"))

min.oplot <-ggplot(stabe.all.o.min, aes(x=V4,color=wood)) +
  geom_histogram(fill="white", alpha=0.5, 
                 position="identity",binwidth = 0.1)+
  xlab('Proportion to boundary')+ylab("count")+
  mythemeplot()+
  theme(legend.title = element_blank(),
        plot.margin = unit(c(0.2,0,0,0),"lines"))+
  scale_x_continuous(
  labels = scales::number_format(accuracy = 0.1))+
  scale_color_manual(values=c(LW="darkgreen",
                                  EW="green"))

stable.all.omin.mean.date <- read.xlsx("E:/Rwork/highresolution/rawdata/oxy_all.xlsx")

oxyplot<-ggplot(data=stable.all.omin.mean.date)+
  scale_x_date(expand = c(0.01,0.01))+
   geom_line(aes(x=date,y = min,col="Min"),lwd=1.0)+ 
          geom_smooth(aes(x=date,y = min),method = "lm",lty=2,se=FALSE)+
    geom_line(aes(x=date,y = mean,col="Mean"),lwd=1.0)+
    #geom_smooth(aes(x=date,y = mean),method = "lm",lty=2)+
    geom_line(aes(x=date,y = max,col="Max"),lwd=1.0)+
    geom_smooth(aes(x=date,y = max),method = "lm",
                col="DarkBlue",lty=2,se=FALSE)+
   geom_line(aes(x=date,y = LWmax,col="LWmax"),lwd=1.0)+
      geom_smooth(aes(x=date,y = LWmax),method = "lm",
                col="darkgreen",lty=2,se=FALSE)+
   geom_line(aes(x=date,y = EWmax,col="EWmax"),lwd=1.0)+
      geom_smooth(aes(x=date,y = EWmax),method = "lm",
                col="green",lty=2,se=FALSE)+
   scale_x_date(name="Year",
                expand = c(0,0),
                breaks = "10 years",
                labels=date_format("%Y"),
                limits = as.Date(c("1900-01-01","2015-06-01")))+
   ylab(expression(paste(delta^"18","O (‰)")))+
      scale_color_manual(values=c(LWmax="darkgreen",
                                  EWmax="green",
                                  Max="DarkBlue",
                                  Mean="DeepSkyBlue",Min="blue"))+
   mythemeplot()+
   guides(col = guide_legend(ncol = 1))+
   theme(legend.position = c(0.9,0.85))+
   theme(legend.background = element_rect(fill = "transparent"), 
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent"), 
        #legend.spacing = unit(2, "lines")
        )+
   theme(legend.title = element_blank())
   #reposition_legend(pdsiplot1, 'left')
   #
   summary(lm(stable.all.omin.mean.date$EWmax~stable.all.omin.mean.date$year))
   summary(lm(stable.all.omin.mean.date$LWmax~stable.all.omin.mean.date$year))
   summary(lm(stable.all.omin.mean.date$max~stable.all.omin.mean.date$year))
   cor.test(stable.all.omin.mean.date$EWmax,stable.all.omin.mean.date$LWmax)
   cor.test(stable.all.omin.mean.date$EWmax,stable.all.omin.mean.date$max)
   cor.test(stable.all.omin.mean.date$LWmax,stable.all.omin.mean.date$max)
   
    ### try to insert the mean value for parameters---
   oxygen.extremlong <- gather(data=stable.all.omin.mean.date,
                               key="para",
                               value = "d18",min,max,mean,EWmax,LWmax,-year)
   
   oxy.meanbox <-
#   ggplot()+
# geom_boxplot(data=extreme.predata.mean,
#             aes(x=label,y=value,col=label))+
  ggboxplot(oxygen.extremlong, x = "para", y = "d18",
  color = "para",width = 0.4,bxp.errorbar.width = 0.1,outlier.shape = NA)+
  xlab("")+ylab("")+
  scale_y_continuous(breaks = c(25,30,35)) + 
  scale_color_manual(values=c(LWmax="darkgreen",                                  EWmax="green",max="DarkBlue",
                    mean="DeepSkyBlue",min="blue"))+
  theme_classic()+
  mytranstheme()+
  theme(legend.position = "none",axis.line.x = element_blank(),
axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x= element_blank())

 # called a "grop" in Grid terminology
oxymean_grob <- ggplotGrob(oxy.meanbox)

oxyplot1 <- oxyplot + annotation_custom(grob = oxymean_grob, xmin = as.Date("1940-01-01"),xmax=as.Date("1970-06-01"), ymin = 32, ymax = 35.5)
   
tiff("./plot/Figure 1-GRL distribution & variability of oxygen max and min-1.tiff ",
     height = 22,width=20,units = "cm",res=800,compression = "lzw",
     family = "serif")
  ggarrange(
    ggarrange(clima,climb,oxy.p,
             ncol=3,labels=c("a","b","c"),
             label.x = c(0.17,0.17,0.19),
             label.y = 1,
            font.label = list(size=22,family="serif")),
    ggarrange(min.oplot,max.oplot,maxEW.oplot,maxLW.oplot,
            ncol=4,labels = c("d","e","f","g"),
            label.x = c(0.18,0.17,0.17,0.17),
            label.y = 1.0,
            common.legend = TRUE, legend="right",
            font.label = list(size=22,family="serif")),
    oxyplot1,
    ncol=1,nrow = 3,
    labels = c("","","h"),
                label.x = 0.08,
                label.y = 0.99,
            font.label = list(size=22,family="serif"))
dev.off()

## Part 3. Climate response analysis----

## 3.3.1 load the chronology-----
#iso.chron1 <- as.data.frame(stable.all.omin.mean[2])
#iso.chron1 <- as.data.frame(stable.all.omax.mean[2])
#iso.chron1 <- as.data.frame(oc.mean[3])

## for the EW and LW min and max
# iso.chron1 <- as.data.frame(stable.allEW.omax.mean$mean)
# iso.chron1 <- as.data.frame(stable.allLW.omax.mean$mean)
# signal is weak for the EW omax
# # for the LW and EW max lag one year
#  iso.chron1 <- as.data.frame(LWEWmax)
#  rownames(iso.chron1) <- c(1900:2013)
# 
# iso.chron1 <- as.data.frame(stable.allEW.omin.mean$mean)
# iso.chron1 <- as.data.frame(stable.allLW.omin.mean$mean)
# PDSI (JJA) is the strongest correlation -0.6 for the min oxygen
# 
rownames(iso.chron1) <- c(1900:2014)
head(iso.chron1)
#iso.chron1 <- c.min.dis ## this format for carbon
#iso.chron1 <- pin[2]

### 3.3.2 climate response-----
###
###   NOTE: This part and heatmap plot should be looped using the different chronologies(max,min, maxLw....), 
###
tmean.res <- dcc(iso.chron1,data.frame(ny.mymdata[,c(3,1,5)]), 
    var_names =c("tem"), method = "correlation",
    selection=.range("tem",-10:12)+.mean("tem",6:8)+.mean("tem",7:9)+.mean("tem",8:11))
evp.res <- dcc(iso.chron1,data.frame(ny.mymdata[,c(3,1,13)]), 
               var_names =c("evp"), method = "correlation",
               selection=.range("evp",-10:12)+.mean("evp",6:8)+.mean("evp",7:9)+.mean("evp",8:11))
dtr.res <- dcc(iso.chron1,data.frame(ny.mymdata[,c(3,1,15)]), 
               var_names =c("dtr"), method = "correlation",
               selection=.range("dtr",-10:12)+.mean("dtr",6:8)+.mean("dtr",7:9)+.mean("dtr",8:11))
tmax.res <- dcc(iso.chron1,data.frame(ny.mymdata[,c(3,1,10)]), 
               var_names =c("tmax"), method = "correlation",
               selection=.range("tmax",-10:12)+.mean("tmax",6:8)+.mean("tmax",7:9)+.mean("tmax",8:11))
tmin.res <- dcc(iso.chron1,data.frame(ny.mymdata[,c(3,1,9)]), 
               var_names =c("tmin"), method = "correlation",
               selection=.range("tmin",-10:12)+.mean("tmin",6:8)+.mean("tmin",7:9)+.mean("tmin",8:11))
rh.res <- dcc(iso.chron1,data.frame(ny.mymdata[,c(3,1,7)]), 
               var_names =c("rh"), method = "correlation",
               selection=.range("rh",-10:12)+.mean("rh",6:8)
              +.mean("rh",7:9)+.mean("rh",8:11))
ssd.res <- dcc(iso.chron1,data.frame(ny.mymdata[,c(3,1,12)]), 
            var_names =c("ssd"), method = "correlation",
            selection=.range("ssd",-10:12)+.mean("ssd",6:8)+.mean("ssd",7:9)+.mean("ssd",8:11))
vpd.res <- dcc(iso.chron1,data.frame(ny.mymdata[,c(3,1,14)]), 
             var_names =c("vpd"), method = "correlation",
             selection=.range("vpd",-10:12)+.mean("vpd",6:8)+.mean("vpd",7:9)+.mean("vpd",8:11))
pre.res <- dcc(iso.chron1,data.frame(ny.mymdata[,c(3,1,8)]), 
            var_names =c("pre"), method = "correlation",
            selection=.range("pre",-10:12)+.sum("pre",6:8)+.sum("pre",4:8)+.sum("pre",8:11))
presure.res <- dcc(iso.chron1,data.frame(ny.mymdata[,c(3,1,6)]), 
             var_names =c("presure"), method = "correlation",
             selection=.range("presure",-10:12)+.sum("presure",6:8)+.sum("presure",7:9)+.sum("presure",8:11))
pre.day.res <- dcc(iso.chron1,data.frame(ny.mymdata[,c(3,1,4)]), 
             var_names =c("pre.day"), method = "correlation",
             selection=.range("pre.day",-10:12)+.mean("pre.day",6:8)+.mean("pre.day",7:9)+.mean("pre.day",8:11))

## here the pdsi data are from CRU data from 1901-2017;
## read and load PDSI data excuted in detectVariabilityCRU.R [current WD]
pdsi.res<- dcc(iso.chron1,data.frame(crupdsi), 
              timespan = c(1953,2013),
             var_names =c("pdsi"), method = "correlation",
             selection =.range("pdsi",-10:12)+.mean("pdsi",6:8)
             +.mean("pdsi",7:9)+.mean("pdsi",8:11))
# plot(tmean.res)
# plot(pre.day.res)
# plot(tmax.res)
# plot(tmin.res)
# plot(rh.res)
# plot(ssd.res)
# plot(pre.res)
# plot(presure.res)
# plot(evp.res)
# plot(vpd.res)
# plot(pdsi.res)
# plot(dtr.res)





omin.miving<-plot(pdsi.movingres)+
  scale_y_continuous(breaks = seq(0.5,2.5,1),labels=c("JJA","A-O","A-N"),
                                          expand = c(0.005,0.005))+
  scale_fill_gradientn(limits=c(-0.8,0.8),colors = Col.5,na.value = "white")+
   scale_x_continuous(breaks = seq(0.5, 
                      by = 1, length.out = ncol(pdsi.movingres$coef$coef)),
                       labels = names(pdsi.movingres$coef$coef),
                      expand = c(0.001,0.001))+
  mythemeplot()+xlab("")+ylab("")+
  theme(axis.ticks = element_line(),
        axis.text.x = element_blank())+
  theme(plot.margin = unit(c(0.5,0,-0.3,0),"lines"))+
  annotate("text", x=1, y=2.5, label="a",size=10,family="serif")
  
pdsi.movingres1<- dcc(iso.chron1,data.frame(crupdsi), 
              #timespan = c(1953,2014),
             var_names =c("pdsi"), method = "correlation",
             selection =.mean("pdsi",6:8)
             +.mean("pdsi",4:10)+.mean("pdsi",8:11),
             dynamic = "moving", win_size = 30,sb = FALSE)



## heatmap plot----

month <- c("o","n","d","J","F","M","A","M","J","J","A","S","O","N","D","JJA","JAS","A-N")
corr.mon <- rbind(tmean.res$coef,tmax.res$coef,tmin.res$coef,pre.res$coef,rh.res$coef,pdsi.res$coef,
                vpd.res$coef,evp.res$coef,ssd.res$coef)
corr.mon$coef.raw <-corr.mon$coef
clim.vars.name <- c("TEM","TMAX","TMIN","PRE","RH","scPDSI","VPD","EVP","SSD")
climgroup <- getgroup(18,clim.vars.name) ## produce the group name for different chronology!!
mongroup <- rep(month,length(clim.vars.name))

corr.mon$climgroup <- climgroup
corr.mon$mongroup <- mongroup

## select the significant correlations
##corr.mon$coef[which(corr.mon$significant=="FALSE")]<-NA
corr.mon$coef[which(abs(corr.mon$coef)<0.247)]<-NA
## plot the climate response at monthly scale


response.heatmap <- 
  ggplot(data=corr.mon,mapping = aes(x=id,
                                     y=factor(climgroup,levels=clim.vars.name),
                                     fill=coef))+
  geom_tile()+xlab(label = "Month")+ylab(label = "Climatic variable")+
  #scale_fill_gradient2(limits=c(-0.6,0.6),low = "steelblue", mid="white",high = "DarkOrange",na.value="grey70")+
  #scale_fill_gradientn(limits=c(-1,1),colors = Col.5,na.value = #"white")+
   scale_fill_gradientn(limits=c(-0.8,0.8),colors = Col.5,na.value = "white")+
  scale_x_continuous(breaks=c(1:18),expand = c(0.03,0.01),labels=month)+
  mythemeplot()+
  theme(axis.text.x = element_text( family = "serif", vjust = 0.5, hjust = 0.5, angle = 90))

c.min.heatmap <- response.heatmap+
    annotate("text", x=1, y=9, label="c",size=10,family="serif")
c.max.heatmap <- response.heatmap+
  annotate("text", x=1, y=9, label="d",size=10,family="serif")

o.max.heatmap <- response.heatmap
o.max.heatmap1<-o.max.heatmap+
   theme(plot.margin = unit(c(0,0,-0.8,0.1),"lines"))+
  theme(legend.position = "none")
        # axis.text.y = element_blank(),
        # axis.title.y = element_blank())

o.min.heatmap <- response.heatmap
o.min.heatmap1 <- o.min.heatmap+
  theme(legend.position = "none")+
      theme(plot.margin = unit(c(0,0,-0.8,0),"lines"))
  
o.mean.heatmap <-response.heatmap
o.mean.heatmap1 <-o.mean.heatmap+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(0,0,-0.8,0.1),"lines"))+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

o.LWmax.heatmap <- response.heatmap
o.LWmax.heatmap1 <-o.LWmax.heatmap+
  theme(plot.margin = unit(c(0,0,-0.8,0.1),"lines"))
  
        
o.LWEWmax.heatmap <- response.heatmap
o.LWEWmax.heatmap1 <-o.LWEWmax.heatmap+
  theme(legend.position = "none",
    plot.margin = unit(c(0,0,-0.8,0.1),"lines"))+
   theme(axis.text.y = element_blank(),
         axis.title.y = element_blank())

legendmap <-o.LWEWmax.heatmap+ 
  theme(legend.position = "bottom")+
  scale_fill_gradientn(limits=c(-0.8,0.8),colors = Col.5,na.value = "white",guide = guide_colorbar(direction = "horizontal",label.vjust = 0,label.theme = element_text(size = 12,family = "serif"),barwidth = 10,title = "correlation",title.position = "bottom",title.hjust = 0.5,title.theme = element_text(size=14,family = "serif"),frame.colour ="gray50"))

leg <- get_legend(legendmap)
	# Convert to a ggplot and print
leg1 <-as_ggplot(leg)+theme(plot.margin = unit(c(0.8,0.1,0.5,0.3),"lines"))

tiff("./plot/LWEW-correlation-response-monthly.tiff",
     width = 10,height =8 ,units = "cm",compression = "lzw",bg="white",family = "serif",res=500)
print(o.LWEWmax.heatmap)
dev.off()

source("./code/climateresponse of EWmax.R")

### 3.3.3 output the figure for the moving correaltions-----
   tiff(file="./plot/Figure 3.3.1 climate Response.tiff",width = 16,
         height = 20,units ="cm",compression="lzw",bg="white",res=800)
    ggarrange(
    ggarrange(o.min.heatmap1,o.mean.heatmap1,
             o.max.heatmap1,o.EWmax.heatmap1,
             o.LWmax.heatmap1,o.LWEWmax.heatmap1,
             nrow = 3,ncol = 2,widths = c(1.2, 1),
             labels = c("a","b","c","d","e","f"),
             label.x = c(0.2,0.015),
             label.y = c(1,1,1.02,1.02,1.02,1.02),
             font.label = list(size=24,face="bold",family="serif"),
              legend="none"),
    leg1,
    nrow = 2,ncol = 1,
    align = "hv",heights = c(1, 0.1),
    widths = c(3.5,1))
    dev.off()
    
 ### Part 4. seascorr analysis for the chronologies-----   
### 4.1 seascorr correlation -----
    crupdsi.long <- gather(crupdsi,key="month",
                           value = "scpdsi",-year)
    crupdsi.long1<-crupdsi.long %>% 
      arrange(year, match(month,month.abb))
    
    ny.mymdata$scpdsi <- subset(crupdsi.long1,year>1952)$scpdsi
    head(ny.mymdata)
    
   pdsiresp.season <- seascorr(iso.chron1,
                      climate=data.frame(ny.mymdata[,c(3,1,8,16)]),
                      complete=11,
                      season_lengths = c(1,3,4,5),
                      primary = 2,secondary = 1,
                      #var_names = c("pre","scpdsi")
                      )
   plot(pdsiresp.season)
   
    pdsiresp.season1 <- seascorr(iso.chron1,
                      climate=data.frame(ny.mymdata[,c(3,1,8,16)]),
                      complete=11,
                      season_lengths = c(1,3,4,5),
                      #primary = 2,secondary = 1,
                      #var_names = c("pre","scpdsi")
                      )
   plot(pdsiresp.season1)
   
   recon <- skills(object = pdsiresp.season,
       target =  .mean("scpdsi",7:9),
       calibration = "-50%")
   # set 50% is for 1983-2014 as calibration
  plot(recon)
 recon
 recon$cal.model
 recon$full.model
 recon$cal.years

 ## here, 1 - (Residual Deviance/Null Deviance) will give the R2.
fit<-lm(x~y,data=recon$full)
summary(fit)
BIC(fit)
AIC(fit)
sqrt(mean(fit$residuals^2))# calculate RMSE
 
  paneltheme<-theme(panel.grid.major =element_line(colour = "gray80",size=0.5,inherit.blank = TRUE),panel.grid.minor =element_line(colour = "gray90",size = 0.2),strip.background=element_rect(fill=NA)) 
  
minpdsi1<-plot(pdsiresp.season)+
     scale_x_continuous(breaks = seq(1, 
                      by = 1, 14),
                       labels = c("o","n","d","J","F",
                                  "M","A","M","J","J",
                                  "A","S","O","N"),
                      expand = c(0.001,0.001))+
  xlab("")+
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme_pubr()+theme(strip.text.y = element_text())
  paneltheme
 
minpre1<-plot(pdsiresp.season1)+
    scale_x_continuous(breaks = seq(1, 
                      by = 1, 14),
                       labels = c("o","n","d","J","F",
                                  "M","A","M","J","J",
                                  "A","S","O","N"),
                      expand = c(0.001,0.001))+
  xlab("")+
  theme_pubr()+
  paneltheme

rhresp.season <- seascorr(iso.chron1,
                      climate=data.frame(ny.mymdata[,c(3,1,7,16)]),
                      complete=11,
                      season_lengths = c(1,3,4,5),
                      #primary = 2,secondary = 1,
                      #var_names = c("pre","scpdsi")
                      )
   plot(rhresp.season)
rhresp.season2 <- seascorr(iso.chron1,
                      climate=data.frame(ny.mymdata[,c(3,1,7,16)]),
                      complete=11,
                      season_lengths = c(1,3,4,5),
                      primary = 2,secondary = 1,
                      #var_names = c("pre","scpdsi")
                      )
   plot(rhresp.season2)

LWmaxrh1<-plot(rhresp.season)+
    scale_x_continuous(breaks = seq(1, 
                      by = 1, 14),
                       labels = c("o","n","d","J","F",
                                  "M","A","M","J","J",
                                  "A","S","O","N"),
                      expand = c(0.001,0.001))+
  theme(plot.margin = unit(c(-1,1,0.2,1), "lines"))+
  theme_pubr()+
  paneltheme

LWmaxpdsi1<-plot(rhresp.season2)+
    scale_x_continuous(breaks = seq(1, 
                      by = 1, 14),
                       labels = c("o","n","d","J","F",
                                  "M","A","M","J","J",
                                  "A","S","O","N"),
                      expand = c(0.001,0.001))+
  theme_pubr()+
  paneltheme
   
LWEWrh1<-plot(rhresp.season)+
    scale_x_continuous(breaks = seq(1, 
                      by = 1, 14),
                       labels = c("o","n","d","J","F",
                                  "M","A","M","J","J",
                                  "A","S","O","N"),
                      expand = c(0.001,0.001))+
  theme(plot.margin = unit(c(-1,1,0.2,1), "lines"))+
  theme_pubr()+
  paneltheme

LWEWpdsi1<-plot(rhresp.season2)+
    scale_x_continuous(breaks = seq(1, 
                      by = 1, 14),
                       labels = c("o","n","d","J","F",
                                  "M","A","M","J","J",
                                  "A","S","O","N"),
                      expand = c(0.001,0.001))+
  theme_pubr()+ 
  paneltheme

## 4.2 output the figures--------------
 

tiff(file="./plot/Figure 3. seacorr for LWEW-max & min pdsi=2 for min.tiff",
     width = 21,height = 18,units ="cm",compression="lzw",
     bg="white",res=800, family = "serif")
ggarrange(minpre1,LWEWpdsi1,
         ncol=1,nrow = 2,
         labels = c("a","b"),
         label.x = 0.05,
         label.y = 1.,
         font.label = list(size=20,family="serif"),
         common.legend = TRUE,legend = "top" )
dev.off()


tiff(file="./plot/Figure S4. seacorr for LWEW-max & min.tiff",
     
     width = 21,height = 18,units ="cm",compression="lzw",
     bg="white",res=800, family = "serif")
ggarrange(minpdsi1,LWEWrh1,
         ncol=1,nrow = 2,
         labels = c("a","b"),
         label.x = 0.05,
         label.y = 1.,
         font.label = list(size=20,family="serif"),
         common.legend = TRUE,legend = "top" )
dev.off()



## 
## ## calculate the correlations between different parameters--
maxmin.data<-stable.all.omin.mean.date %>%
  select(min,max,mean,LWmax,EWmax) %>%
  as.data.frame()
maxmin.data <-array(as.numeric(unlist(maxmin.data)), dim=c(115, 5))
colnames(maxmin.data)<-c("min","max","mean","LWmax","EWmax")

cc.proxy<-rcorr(maxmin.data,
  type="pearson")
cc.proxy1<-rcorr(maxmin.data[c(1:51),],
  type="pearson")
cc.proxy2<-rcorr(maxmin.data[c(52:115),],
  type="pearson")

head(maxmin.data)
## correlation between LW and lag1 EW
cor(maxmin.data[-1,5],maxmin.data[-115,4])

## combine EWmax and LW max
summary(maxmin.data[-1,5])
summary(maxmin.data[-115,4])
LWEWmax<-(maxmin.data[-1,5]+maxmin.data[-115,4])/2

## Part 5. multiple variable and common analysis-------------

## ##5.1 using the nlm model----
model.1=lm(mean~ mean.vpd+mean.rh+mean.pre+mean.ssd+mean.value+scpdsi+mean.tmean+mean.evp,data=semmodeldata)
step1 <- stepAIC(model.1, direction="both")
step1$anova # display results
model.2=lm(mean~ mean.ssd+mean.value+scpdsi+mean.pre+mean.rh,
           data=semmodeldata)
model.3=lm(mean~ mean.ssd+mean.value+scpdsi,data=semmodeldata)


LWmodel.1=lm(mean~ mean.rh+mean.pre+mean.ssd+mean.value+scpdsi+mean.vpd, data=semmodeldata2)
LWmodel.2=lm(mean~ mean.vpd+mean.rh, data=semmodeldata2)
LWmodel.3=lm(mean~ mean.rh+mean.pre, data=semmodeldata2)
step <- stepAIC(LWmodel.1, direction="both")
step$anova # display results

head(semmodeldata3)
LWEWmodel.1=lm(LWEWmax ~ rh+pre+ssd1+mean.value+scpdsi+vpd+evp, data=semmodeldata3)
LWEWmodel.2=lm(LWEWmax~ vpd+rh, data=semmodeldata3)
LWEWmodel.3=lm(LWEWmax~ rh+pre, data=semmodeldata3)
LWEWmodel.4=lm(LWEWmax~ rh, data=semmodeldata3)
step1 <- stepAIC(LWEWmodel.1, direction="both")
step1$anova # display results

#commonality assessment--

regr(model.1)
regr(model.2)## this depend the beta weight!!
regr(model.3) ##$Commonality_Data $Commonality_Data$`CC shpw the cntributions
commonality(model.1)

## All-possible-subsets regression
     apsOut=aps(semmodeldata,"mean",list("scpdsi", "mean.value","mean.ssd"))

  ## Commonality analysis
     commonality(apsOut)

regr(LWmodel.1)
regr(LWmodel.2)
regr(LWmodel.3)
commonality(model.1)

regr(LWEWmodel.1)
regr(LWEWmodel.2)
regr(LWEWmodel.3)
regr(LWEWmodel.4)

## Part 6. Climate reconstruction and comparison---------

## 6.1 reconstruction test-----
## 
 ## subset the chronology
Amin.chron1 <- as.data.frame(stable.all.omin.mean[2])
#iso.chron1 <- as.data.frame(stable.all.omax.mean[2])
#iso.chron1 <- as.data.frame(oc.mean[3])
rownames(Amin.chron1) <- c(1900:2014)
head(Amin.chron1)
  
## for the EW and LW min and max
# iso.chron1 <- as.data.frame(stable.allEW.omax.mean$mean)
# iso.chron1 <- as.data.frame(stable.allLW.omax.mean$mean)
# 
# # for the LW and EW max lag one year
LWEW.chron1 <- as.data.frame(LWEWmax)
rownames(LWEW.chron1) <- c(1900:2013)

# PDSI (JAS) is the strongest correlation -0.667 for the min oxygen
# 

 pdsi.res1<- dcc(Amin.chron1,data.frame(subset(crupdsi,year>1952)), 
              timespan = c(1953,2014),
             var_names =c("pdsi"), method = "correlation",
             selection =.range("pdsi",-10:12)+.mean("pdsi",6:8)
             +.mean("pdsi",7:9)+.mean("pdsi",8:11))
 plot(pdsi.res1) 
 
 rhLWEWmax.res1<- dcc(LWEW.chron1,
                      data.frame(ny.mymdata[,c(3,1,7)]), 
              #timespan = c(1953,2014),
             var_names =c("rh"), method = "correlation",
             selection =.range("rh",10)+.mean("rh",6:8)
             +.mean("rh",4:10)+.mean("rh",8:11))
 plot(rhLWEWmax.res1)
 
 rhLWmax.res1<- dcc(iso.chron1,data.frame(ny.mymdata[,c(3,1,7)]), 
              timespan = c(1953,2014),
             var_names =c("rh"), method = "correlation",
             selection =.mean("rh",6:8)
             +.mean("rh",4:10)+.mean("rh",8:11))
 plot(rhLWmax.res1)
 
 
sk.pdsimin<-skills(object = pdsi.res1,
                   target =.mean("pdsi",7:9),
                   calibration = "-50%",
       model="ols")
ggplotly(plot(sk.pdsimin))
sk.pdsimin$full.model$rsquare 
summary(sk.pdsimin$full.model$call)
sk.pdsimin$RE#
sk.pdsimin$CE
sk.pdsimin$cal.model

sk.pdsimin.2<-skills(object = pdsi.res1,
                   target =.mean("pdsi",4:10),
                   calibration = "49%",
       model="ols")
ggplotly(plot(sk.pdsimin.2))
sk.pdsimin.2$full.model$rsquare 
summary(sk.pdsimin.2$coef.full)
sk.pdsimin.2$RE#
sk.pdsimin.2$CE
sk.pdsimin.2$cal.model

sk.rhLWEWmax<-skills(object = rhLWEWmax.res1,
           target =.range("rh",10),
           calibration = "-51%",
           timespan = c(1953,2013),
           model="ols")
ggplotly(plot(sk.rhEWmax))
sk.rhLWEWmax$full.model 
sk.rhLWEWmax$cal.model
sk.rhLWEWmax$DW
sk.rhLWEWmax$RE
sk.rhLWEWmax$CE
sk.rhLWEWmax$cal.years
  
sk.rhLWmax<-skills(object =rhLWmax.res1,
           target =.mean("rh",8:11),
           calibration = "-51%",
           timespan = c(1953,2014),
           model="ols")
ggplotly(plot(sk.rhLWmax))
sk.rhLWmax$full.model 
sk.rhLWmax$cal.model
sk.rhLWmax$DW
sk.rhLWmax$RE
sk.rhLWmax$CE

sk.rhLWmax.2<-skills(object =rhLWmax.res1,
           target =.mean("rh",8:11),
           calibration = "49%",
           model="ols")
ggplotly(plot(sk.rhLWmax.2))
sk.rhLWmax.2$cal.years
sk.rhLWmax.2$full.model
sk.rhLWmax.2$cal.model
sk.rhLWmax.2$DW
sk.rhLWmax.2$RE
sk.rhLWmax.2$CE

fit<-lm(x~y,data=sk.rhLWmax$full)
summary(fit)
BIC(fit)
AIC(fit)
sqrt(mean(fit$residuals^2))# calculate RMSE


title<-cbind(Calibration=c("1953-1983","1984-2014"),
         Verification=c("1984-2014","1953-1983"))
REtable.PDSI<-cbind(title,
                    cbind(RE=c(sk.pdsimin.2$RE,sk.pdsimin$RE),
                  CE=c(sk.pdsimin.2$CE,sk.pdsimin$CE)))

REtable.rh<-cbind(title,
                  cbind(RE=c(sk.rhLWmax.2$RE,sk.rhLWmax$RE),
                  CE=c(sk.rhLWmax.2$CE,sk.rhLWmax$CE)))


regree.data<-data.frame(cbind(sk.pdsimin$full.model$y,sk.pdsimin$full.model$x,
                  c(sk.rhLWEWmax$full.model$y,NA),
                  c(sk.rhLWEWmax$full.model$x,NA)))
colnames(regree.data)<-c("scpdsi","amino18","rh10","LWEWmaxo18")

cor(regree.data$scpdsi[-62],
    regree.data$rh10[-62])
 
# mulfit1<-lm(regree.data$rh8.11[-(59:62)]~
#               regree.data$LWmaxo18[-(59:62)]+
#               sk.rhEWmax$full.model$x[-(59:61)])
# summary(mulfit1) 



m1<-lm(regree.data$scpdsi~regree.data$amino18)
reg1<-ggplot(regree.data,aes(x=amino18,y=scpdsi)) +
  geom_point(shape=1,col=4) +
  geom_smooth(method=lm, lty=2, color=4, se=TRUE)+
  ylab("July-September scPDSI")+
  xlab(expression(paste("Annual minimum tree-ring"," ",~delta^18,"O")))+
  geom_text(y = 4.5, x = 26, label = lm_eqn(m1),
            parse = TRUE,
            colour="black",family="TN",size=3.5)+
  mythemeplot()

m2<-lm(regree.data$rh10~regree.data$LWEWmaxo18)
reg2<-ggplot(regree.data,aes(x=LWEWmaxo18,y=rh10)) +
  geom_point(shape=1,col="darkgreen") +
  geom_smooth(method=lm , lty=2, color="darkgreen", se=TRUE)+ 
  ylab("October RH (%)")+
  xlab(expression(paste("LW + EW(lag1) maximum tree-ring"," ",~delta^18,"O")))+
  geom_text(x = 29.5, y = 70, label = lm_eqn(m2),
            parse = TRUE,
            colour="black",family="TN",size=3.5)+
  mythemeplot()
 
## 6.2 climate reconstruction------

## Reconstruction data-- 
  sk.pdsimin$full.model 
  pdsi.recon<-32.0118-1.209782*stable.all.omin.mean[2]
  sk.rhLWEWmax$full.model
  rh.recon <- 197.4765-3.896857*LWEWmax
  recondata<-cbind(pdsi.recon,c(rh.recon,NA))
  colnames(recondata)<-c("scpdsi","rh10")
  recondata$year<-1900:2014
  
  obs<-subset(regree.data,select = c("scpdsi","rh10"))
  obs$year<-1953:2014
  
  CRU<-subset(crupdsi,select = c(8:10),year<1954)%>%
    mutate(pdsi=rowMeans(.))
  CRU<-cbind(CRU[,4],NA,year=(1901:1953))
  colnames(CRU)<-c("scpdsi","rh10","year")
  
  reconcomdata<-rbind(obs,recondata,CRU)
  reconcomdata$type<-c(rep("observation (CRU)",62),
                       rep("reconstruction",115),
                       rep("CRU before 1953",53))
  ## detect the slope for different period
  summary(lm(data=subset(reconcomdata, type=="reconstruction"),
     scpdsi~year))
  summary(lm(data=subset(reconcomdata,year>1952 & type=="reconstruction"),
     scpdsi~year))
  summary(lm(data=subset(reconcomdata,year<1953 & type=="reconstruction"),
     scpdsi~year))
  summary(lm(data=subset(reconcomdata, type=="observation (CRU)"),
     scpdsi~year))
  summary(lm(data=subset(reconcomdata, type=="CRU before 1953"),
     scpdsi~year))
  
  summary(lm(data=subset(reconcomdata, type=="reconstruction"),
     rh10~year))
  summary(lm(data=subset(reconcomdata,year>1952 & type=="reconstruction"),
     rh10~year))
  summary(lm(data=subset(reconcomdata,year<1953 & type=="reconstruction"),
     rh10~year))
  write.csv(reconcomdata,"reconstruction.csv")
  
  x=subset(reconcomdata,year< 1953 & type=="reconstruction"|
                 year<1953 & type=="CRU before 1953",
               select =c("scpdsi","year","type") )
  cor.test(x$scpdsi[1:52],x$scpdsi[53:104])
    
  cor(x=subset(reconcomdata,year>1952 & type=="reconstruction",
               select =c("scpdsi","rh10") ),
      use="complete.obs",
    method = "pearson")
  cor(x=subset(reconcomdata,year<1953 & type=="reconstruction",
               select =c("scpdsi","rh10") ),
      use="complete.obs",
    method = "pearson")
  
  cor(subset(reconcomdata,year>1983 & type=="reconstruction",
               select =c("scpdsi","rh10") ),
           subset(reconcomdata,year>1983 & type=="observation (CRU)",
               select =c("scpdsi","rh10") ),
      use="complete.obs",
    method = "pearson")
  cor(subset(reconcomdata,year>1953 &year <1984 & type=="reconstruction",
               select =c("scpdsi","rh10") ),
           subset(reconcomdata,year>1953 &year <1984 & type=="observation (CRU)",
               select =c("scpdsi","rh10")),
      use="complete.obs",
    method = "pearson")
  
  ## here, comparison between different filter functions!!
  #   spline.pdsi1<-smooth.spline(recondata$year,recondata$scpdsi,n = 10)
  #   spline.pdsi2<- pass.filt(recondata$scpdsi, W=10, type="low", method="Butterworth")## for 10 year low pass
  #   spline.pdsi2 <- as.data.frame(cbind(x=spline.pdsi$x, y=spline.pdsi2))
  #   
  # spline.pdsi<-smooth.spline(recondata$year,recondata$scpdsi,spar = 0.2)##
  # spline.pdsi <- as.data.frame(cbind(x=spline.pdsi$x, y=spline.pdsi$y))
  # plot(spline.pdsi$x, spline.pdsi$y, type="l",col=2)
  # par(new=TRUE)
  # plot(spline.pdsi1$x, spline.pdsi1$y, type="l")
  # par(new=TRUE)
  # plot(spline.pdsi2$x, spline.pdsi2$y, type="l",col=4)
  
  ## reconstruction and 20-year loess smooother
  pdsireconplot<-ggplot(reconcomdata,aes(x=year,y=scpdsi)) +
    geom_line(aes(colour= type))+
     geom_smooth(data=subset(reconcomdata,type=="reconstruction"),aes(x=year,y=scpdsi),
       method = "loess",span=0.2,se=FALSE,lwd=1.5,col=4)+
    #geom_line(data=spline.pdsi,aes(x=x,y=y))+
    geom_smooth(data=subset(reconcomdata,type=="reconstruction"),aes(x=year,y=scpdsi),method = "loess",span=0.75,se=TRUE,col=c("blue"))+#col=c("#00BFC4"))
    xlab("")+ylab("July-September scPDSI")+
    scale_x_continuous(expand = c(0.01,0.01))+
    mythemeplot()+
    theme(legend.position = c(0.85,0.87),
          legend.title = element_blank())+
    geom_vline(xintercept=1984,lty=2,col="gray70")+
    theme(plot.margin = unit(c(-0.2,0.3,0,0.3),"lines"))+#+
    # geom_line(data=subset(crupdsi.4.10.date,year<1954),
    # aes(x=year(date),y=growing,col="Darkorange"),
    #         lwd=0.2)+
    scale_color_manual(values=c("observation (CRU)" = "#F8766D", 
                                "reconstruction" = "blue",
                                "CRU before 1953"="Darkorange"),
                   labels=c("CRU before 1953", "observation (CRU)", "reconstruction"))+
    annotate("text", x = 1984, y = -3.2, 
             label = expression(paste("Verification: ", italic(r), "= 0.75; Calibration: ", italic(r)," = 0.49")),
             family="serif")#+
    #annotate("text", x = 1984, y = -3.5, label = "RE = 0.526, CE = 0.473",family="serif")
  
  rhreconplot<-ggplot(reconcomdata,aes(x=year,y=rh10)) +
    geom_line(aes(colour= type))+
    geom_smooth(data=subset(reconcomdata,type=="reconstruction"),aes(x=year,y=rh10),
       method = "loess",span=0.2,se=FALSE,lwd=1.5,col=c("darkgreen"))+
    geom_smooth(data=subset(reconcomdata,type=="reconstruction"),aes(x=year,y=rh10),method = "loess",span=0.75,se=TRUE,col=c("darkgreen"))+
    xlab("Year")+ylab("October RH (%)")+
    scale_x_continuous(expand = c(0.01,0.01))+
    mythemeplot()+
    theme(legend.position = c(0.2,0.2),
          legend.title = element_blank())+
    geom_vline(xintercept=1984,lty=2,col="gray70")+
    theme(plot.margin = unit(c(-0.5,0.3,0.3,0.6),"lines"))+
    scale_color_manual(values=c("observation (CRU)" = "#00BFC4", 
                                "reconstruction" = "darkgreen",
                                "CRU before 1953"=NA),
                   labels=c("", "observation", "reconstruction"))+
    annotate("text", x = 1984, y = 60, 
             label = expression(paste("Verification: ", italic(r), "= 0.77; Calibration: ", italic(r)," = 0.61")),
             family="serif")#+
    #annotate("text", x = 1984, y = 72, label = "RE = 0.464, CE = 0.461", family="serif")

  tiff(file="./plot/Figure 7.1.1 reconstruction1.tiff",
     width = 16,height = 18,
     units ="cm",compression="lzw",bg="white",res=800)
ggarrange(
  ggarrange(reg1,reg2,ncol=2,labels = c("a","b"),
            label.x = 0.87,
            label.y = c(1,0.99),
            font.label = list(size=20,family="serif")),
  ggarrange(pdsireconplot,rhreconplot,
         nrow = 2,
         labels = c("c","d"),
            label.x = 0.1,
            label.y = c(1,1.04),
            align = "v",
            font.label = list(size=20,family="serif")),
nrow = 2,align = "v",heights = c(0.6,1),
# labels = c("","c"),
#             label.x = 0.1,
#             label.y = 1.04,
            font.label = list(size=20,family="serif"))
  dev.off()
  
  

### Part 7. Supplementary figure plot--------
### 
### 7.3. Figure S3----------
## here, the max value have a lag significant correlation, it means the significant old carbon reuse?? McCaroll et al., 2017

tiff("./plot/Figure S3 oxygen parameter correlation 1900-2014.tiff",width=8,height = 8,units = "cm",
     compression = "lzw",bg="white",res = 300)
windowsFonts(TN = windowsFont("Times New Roman"))  
 par(mgp=c(2.0,0.5,0),family="TN",ps=8)
 # par(mfrow=c(1,3),mgp=c(1.0,0.5,0),family="TN",ps=13)
#par(mar=c(0, 0, 0.0, 0) + 0.1)
par(oma=c(0,0,0.02,0.02))

corrplot(corr = cc.proxy$r,type="upper",
         col=brewer.pal(n=10, name="PuOr"),cl.lim = c(0, 1),
         tl.pos="d",tl.col = 1,tl.cex=1.2,
         p.mat = cc.proxy$P, sig.level = 0.05,insig ="pch",
         pch.cex = 3,pch.col = rgb(255, 0, 0,100, maxColorValue=255))

corrplot(corr=cc.proxy$r,add=TRUE,type="lower",method = "number", number.cex = 1,number.font=2,col=1,
         diag=FALSE,tl.pos="n", cl.pos="n",p.mat = cc.proxy$P, 
         sig.level = 0.05,insig ="pch",pch.cex = 3,
         pch.col = rgb(255, 0, 0, 100, maxColorValue=255))
dev.off()

### 7.4 Figure S4--------------
  ## Figure S4 has been ouputed in the part 4.2

## 7.5  Figure S5. correlation between chrongologies and ISOGSM data-----

## detect the climatic signal of the GNIP data (precipitation oxygen data )
## the aim of this part is to detect the climate response in the tree-ring d18O and d18O in precipitation
### 7.5.1 d18O precipitation response to maximum and minimum tree-ring -----
omin.mean <- as.data.frame(stable.all.omin.mean[2])
omax.mean <- as.data.frame(stable.all.omax.mean[2])

omin.mean.ts <- ts(omin.mean, start = 1900,frequency = 1)
omax.mean.ts <- ts(omax.mean, start = 1900,frequency = 1)
EWomax.mean.ts <- ts(stable.allEW.omax.mean$mean,
                     start = 1900,frequency = 1)
LWomax.mean.ts <- ts(stable.allLW.omax.mean$mean,
                     start = 1900,frequency = 1)
LWEW.mean.ts <- ts(LWEW.chron1,start = 1900,frequency = 1)

## here call the function runningclimate from E:/Rwork/myfunction/basic dplR and beyond.R
## the basic idea is used the runningclimate to detect the pearson's correlation
## call for the data @ oxygen from precipitation, @@p.rateoxy.clim

omin.mean.p <- Climateplot (omin.mean.ts,
             Climatesite = p.rateoxy.clim,
             fyr=1950,lyr=2010,
             detrended=c("No"),
             spline.length=0)


omax.mean.p <- Climateplot(omax.mean.ts,
                            Climatesite = p.rateoxy.clim,
                            fyr=1950,lyr=2010,
                            detrended=c("No"),
                            spline.length=0)

EWomax.mean.p <- Climateplot(EWomax.mean.ts,
                            Climatesite = p.rateoxy.clim,
                            fyr=1950,lyr=2010,
                            detrended=c("No"),
                            spline.length=0)

LWomax.mean.p <- Climateplot(LWomax.mean.ts,
                            Climatesite = p.rateoxy.clim,
                            fyr=1950,lyr=2010,
                            detrended=c("No"),
                            spline.length=0)
LWEWomax.mean.p <- Climateplot(LWEW.mean.ts,
                            Climatesite = p.rateoxy.clim,
                            fyr=1950,lyr=2010,
                            detrended=c("No"),
                            spline.length=0)
# Adapt these to your needs:
#parSettings <- list(layout.widths=list(left.padding=1))

omin.p <- contourplot(t(omin.mean.p),region=T,lwd=0.3,lty=2,aspect=0.4,
            col.regions=colorRampPalette(c("red","yellow","white","green3","blue")),
            at=c(seq(-0.8,0.8,0.05)),xlab="",ylab="Window length",main=NA)+
  latticeExtra::layer(panel.text(x=3, y=11.5, label="a min",family="serif",font=2,cex=1.5))

# omax.p<-contourplot(t(omax.mean.p),region=T,lwd=0.3,lty=2,
#             col.regions=colorRampPalette(c("red","yellow","white","green3","blue")),
#             at=c(seq(-0.8,0.8,0.05)),xlab=" ",ylab="Window length",main=title)

EWomax.p <-contourplot(t(EWomax.mean.p),region=T,lwd=0.3,lty=2,aspect=0.4,
            col.regions=colorRampPalette(c("red","yellow","white","green3","blue")),
            at=c(seq(-0.8,0.8,0.05)),xlab="Months",ylab="Window length")+
  latticeExtra::layer(panel.text(x=3, y=11.5, label="b EW-max",family="serif",font=2,cex=1.5))

LWomax.p <-contourplot(t(LWomax.mean.p),region=T,lwd=0.3,lty=2,aspect=0.4,
            col.regions=colorRampPalette(c("red","yellow","white","green3","blue")),
            at=c(seq(-0.8,0.8,0.05)),xlab="Months",ylab="Window length",main=NA)+
  latticeExtra::layer(panel.text(x=3, y=11.5, label="c LW-max",family="serif",font=2,cex=1.5))


LWEWomax.p<-contourplot(t(LWEWomax.mean.p),region=T,lwd=0.3,lty=2,aspect=0.4,
            col.regions=colorRampPalette(c("red","yellow","white","green3","blue")),
            at=c(seq(-0.8,0.8,0.05)),xlab="Months",ylab="Window length",main=NA)+
  latticeExtra::layer(panel.text(x=4.5, y=11.5, label="d Composite max",family="serif",font=2,cex=1.5))
            
## 7.5.2 output the correlation analysis-------
tiff("./plot/omin-EW,LWomax-precipitation-oxy-2.tiff",width = 20,height = 27,
     units = "cm",pointsize = 12,compression = "lzw",res = 300,bg="white",family = "serif")
# Combine lattice charts into one
#c(omin.p, EWomax.p)

c(LWEWomax.p,LWomax.p,EWomax.p,omin.p,
  merge.legends = TRUE,layout=c(1,4))
dev.off()

### 7.6. Variability of the cloud cover from CRU dataset----
### # read cloud cover data from CRU
 crucld<-read.table("./cru/icru4_cld_112.5-112.7E_27.27-27.5N_n.dat",
                 header = FALSE)
head(crucld)
colnames(crucld)<-c("year",1:12)
crucld <- subset(crucld,year>1952 & year<2015)
  
# Determine p-values of regression
  # 
p.vals <-NA
   
for(i in 2:13 ) {
  cldslope=coef(summary(lm(crucld[,i]~crucld[,1])))[2,4]
  p.vals <- cbind(p.vals,cldslope)
}
crucldlong <- gather(crucld,key="month",value=cld,-year)

my_breaks <- function(x) { if (min(x) < 50) seq(30, 90, 20) else seq(60, 90, 15) }

crucld.longplot<-ggplot(
  data=subset(crucldlong, year<2015),
  aes(year,cld,group=month,col=factor(month,levels=c(1:12))))+
    geom_line()+geom_point()+
    facet_grid(factor(month,levels=c(1:12))~., scales="free")+
    xlab(label = "Year")+
    ylab(label = c("Cloud cover (%)"))+
     scale_x_continuous(expand = c(0.005,0.005))+
     scale_y_continuous( breaks = my_breaks)+
    guides(col=guide_legend(title="Month"))

  crucld.plot<-ggplot(
    data=subset(crucldlong,year>1952 & year <2015), aes(year,cld,group=month,col=factor(month,levels=c(1:12))))+
    geom_line()+geom_point()+
    facet_grid(factor(month,levels=c(1:12))~.,scales = "free")+
    #facet_grid(factor(crucldlong$month,levels=c(1:12))~., scales="free")+
    xlab(label = "Year")+
    ylab(label = c("Cloud cover (%)"))+
     scale_x_continuous(expand = c(0.01,0.01))+
     scale_y_continuous( breaks = my_breaks)+
    guides(col=guide_legend(title="Month"))
  
  
 tiff(file="./plot/Figure S6. Cloud cover for 1900-now.tiff",width = 16,height = 14,units ="cm",compression="lzw",bg="white",family = "serif",res=600)
  print(crucld.longplot)
  dev.off()
 
  ### 7.7 Variability of d18O of precipitation------
  
## plot and for the seasonal oxygen isotpe in precipitation from ISOGSM model
  
  pre.oxy2.11<-subset(p.rateoxy.shape,Var2 %in% c(2,3,4,5,6,7,8,9,10,11)& Var1>1949 & Var1<2011)
  pre.oxy2.11long<-subset(p.rateoxy.shape,Var2 %in% c(2,3,4,5,6,7,8,9,10,11)& Var1>1899)
  
  # Determine p-values of regression
  # 
p.vals <-NA
   
for(i in 1:10 ) {
  pslope=pvaluecal(unique(pre.oxy2.11long$Var2)[i],
                   group=2,data=pre.oxy2.11)
  p.vals <- cbind(p.vals,pslope)
}

  pre.oxy2.11.plot<-ggplot(subset(pre.oxy2.11,Var1>1949 & Var1<2011),aes(Var1,value,group=Var2,col=as.factor(Var2)))+
    geom_line()+geom_point()+
    facet_grid(pre.oxy2.11$Var2~., scales="free")+
     # stat_smooth(method=lm,se=FALSE,lty=2,
     #             lwd=1.0,level = 0.95)+
     #geom_smooth(method = "lm",col="black",lty=2)+
    xlab(label = "Year")+ylab(label = expression(paste("Precipitation  ",delta^"18","O (‰)")))+
    guides(col=guide_legend(title="Month"))
  
    pre.oxy2.11.longplot<-ggplot(pre.oxy2.11long,aes(Var1,value,group=Var2,col=as.factor(Var2)))+
    geom_line()+geom_point()+
    facet_grid(as.factor(Var2)~., scales="free")+
     # stat_smooth(method=lm,se=FALSE,lty=2,
     #             lwd=1.0,level = 0.95)+
     #geom_smooth(method = "lm",col="black",lty=2)+
     scale_x_continuous(expand = c(0.01,0.01))+
    xlab(label = "Year")+ylab(label = expression(paste("Precipitation  ",delta^"18","O (‰)")))+
    guides(col=guide_legend(title="Month"))
  
  tiff(file="./plot/stable oxygen in Feb-Nov preciptation for 1950-now.tiff",width = 16,height = 14,units ="cm",compression="lzw",bg="white",family = "serif",res=600)
  print(pre.oxy2.11.plot)
  dev.off()
  
  tiff(file="./plot/stable oxygen in Feb-Nov preciptation for 1900-now.tiff",width = 16,height = 14,units ="cm",compression="lzw",bg="white",family = "serif",res=600)
  print(pre.oxy2.11.longplot)
  dev.off()
  
  pre.oxy5.8.mean<-pre.oxy5.8 %>% group_by(Var1)%>%
    summarise(mean.value=mean(value,na.rm=TRUE))
  pre.oxy2.4.mean<-pre.oxy2.4 %>% group_by(Var1)%>%
    summarise(mean.value=mean(value,na.rm=TRUE))
  pre.oxy2.10.sd<-pre.oxy2.10 %>% group_by(Var1)%>%
    summarise(sd.value=sd(value,na.rm=TRUE))

   tiff(file="./plot/diff in preciptation for 1900-now.tiff",width = 12,height = 8,units ="cm",compression="lzw",bg="white",res=600)
  plot(pre.diff[,1],
       abs(pre.diff[,2]),"l",xli=c(1900,2014),
       xlab="year",ylab="Difference in absolute")
  abline(fit.abs,lty=2)
  text(1950,1.5,
    label=expression(paste(italic(slope),'=-0.0093, ',italic(R)^2, '= 0.08, ', italic(p),'= 0.003')))
  dev.off()

## 7.8.plot the trend of vapor d18O-----
  ### 
  monthvp.oxylong<-gather(monthvp.oxy,key="month",value = "d18O",-year)
    
  monthvp.oxy2.11<-subset(monthvp.oxylong,month %in% c(2,3,4,5,6,7,8,9,10,11)& year>1949 & year<2011)
  monthvp.oxy2.11long<-subset(monthvp.oxylong,month %in% c(2,3,4,5,6,7,8,9,10,11)& year>1899)
  
   # Determine p-values of regression
  # 
p.vals <-NA
   
for(i in 1:10 ) {
  pslope=pvaluecal(unique(monthvp.oxy2.11long$month)[i],
                   group=2,data=monthvp.oxy2.11)
  p.vals <- cbind(p.vals,pslope)
}

  
  monthvp.oxy2.11.plot<-ggplot(monthvp.oxy2.11,aes(year,d18O,group=month,col=factor(month,levels=c(2:11))))+
    geom_line()+geom_point()+
    facet_grid(factor(month,levels = c(2:11))~., scales="free")+
    scale_x_continuous(expand = c(0.01,0.01))+
     # stat_smooth(method=lm,se=FALSE,lty=2,
     #             lwd=1.0,level = 0.95)+
     #geom_smooth(method = "lm",col="black",lty=2)+
    xlab(label = "Year")+ylab(label = expression(paste(" Water vapour ",delta^"18","O (‰)")))+
    guides(col=guide_legend(title="Month"))
  
    monthvp.oxy2.11.longplot<-ggplot(monthvp.oxy2.11long,aes(year,d18O,group=month,col=factor(month,levels=c(2:11))))+
    geom_line()+geom_point()+
    facet_grid(factor(month,levels=c(2:11))~., scales="free")+
     # stat_smooth(method=lm,se=FALSE,lty=2,
     #             lwd=1.0,level = 0.95)+
     #geom_smooth(method = "lm",col="black",lty=2)+
     scale_x_continuous(expand = c(0.01,0.01))+
    xlab(label = "Year")+ylab(label = expression(paste("Water vapour  ",delta^"18","O in precipitation (‰)")))+
    guides(col=guide_legend(title="Month"))
    
   tiff(file="./plot/Figure S8. stable oxygen in Feb-Nov vapour for 1900-now.tiff",width = 16,height = 14,units ="cm",compression="lzw",bg="white",family = "serif",res=600)
  print(monthvp.oxy2.11.longplot)
  dev.off()

  ## 7.9 Variability of seasonal mean climate-------
  clim.july_sept1<-subset(ny.mymdata,month %in% c(7,8,9))%>% 
   group_by(year)%>%
   summarise(mean.preday=mean(pre.day,na.rm=TRUE),mean.tmean=mean(tmean,na.rm=TRUE),
             mean.presure=mean(water.pressure,na.rm=TRUE),mean.rh=mean(rh,na.rm=TRUE),
             mean.pre=mean(pre,na.rm=TRUE),mean.tmin=mean(tmin,na.rm=TRUE),
             mean.tmax=mean(tmax,na.rm=TRUE),mean.ssd=mean(ssd,na.rm=TRUE),
             mean.vpd=mean(vpd,na.rm=TRUE),mean.evp=mean(evp,na.rm=TRUE),
             mean.pdsi=mean(scpdsi,na.rm = TRUE))
 
clim.mar_jun <- subset(ny.mymdata,month %in% c(3:6))%>% 
   group_by(year)%>%
   summarise(mean.3.6preday=mean(pre.day,na.rm=TRUE),mean.3.6tmean=mean(tmean,na.rm=TRUE),
             mean.3.6presure=mean(water.pressure,na.rm=TRUE),mean.3.6rh=mean(rh,na.rm=TRUE),
             mean.3.6pre=mean(pre,na.rm=TRUE),mean.3.6tmin=mean(tmin,na.rm=TRUE),
             mean.3.6tmax=mean(tmax,na.rm=TRUE),mean.3.6ssd=mean(ssd,na.rm=TRUE),
             mean.3.6vpd=mean(vpd,na.rm=TRUE),mean.3.6evp=mean(evp,na.rm=TRUE),
             mean.3.6pdsi=mean(scpdsi,na.rm = TRUE))
 
clim.july_sept <- cbind(clim.july_sept1,clim.mar_jun[-1])

clim.oct<-subset(ny.mymdata,month %in% c(10))
 
 head(clim.july_sept)
 head(clim.oct)
 head(clim.mar_jun)
 
 
 
 rh.10<-ggplot(clim.oct,aes(year,rh))+
   geom_line()+geom_point()+
   #stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   geom_smooth(method = "loess",span=0.2,se=F,col=1,lwd=1.2,lty=1)+
   xlab(label = "Year")+ylab(label = "Relative humidity (%)")+
   # annotate("text",x=1988,y=(min(clim.oct$rh,na.rm = TRUE))*1.02,
   #          label=expression(paste(italic(slope),'= 0.038, ',italic(R)^2, '= 0.06, ', italic(p),'= 0.05')))+
   mythemeplot()+
 theme(axis.title.x=element_blank())
 summary(lm( clim.oct$rh[1:51]~clim.oct$year[1:51]))
 summary(lm( clim.oct$rh~clim.oct$year))
 
 rh.7.9<-ggplot(clim.july_sept,aes(year,mean.rh))+
   geom_line(col=4)+geom_point(col=4)+
   stat_smooth(method=lm,lty=2,lwd=1.0,col=4)+
   geom_smooth(method = "loess",span=0.2,se=F,lty=1,lwd=1.2)+
   geom_line(aes(year,mean.3.6rh),col=3)+geom_point(aes(year,mean.3.6rh),col=3)+
   stat_smooth(aes(year,mean.3.6rh),method=lm,lty=2,lwd=1.0,col=3)+
   geom_smooth(aes(year,mean.3.6rh),method = "loess",span=0.2,se=F,col=3,lwd=1.2,lty=1)+
   xlab(label = "Year")+ylab(label = "Relative humidity (%)")+
    annotate("text",x=1980,y=(min(clim.july_sept$mean.rh,na.rm = TRUE))*1.02,col=4,
           label=expression(paste(italic(slope),'= 0.037, ',italic(R)^2, '= 0.04, ', italic(p),'= 0.07')))+
   annotate("text",x=1980,y=(min(clim.july_sept$mean.rh,na.rm = TRUE))*1.04,col=3,
           label=expression(paste(italic(slope),'= -0.038, ',italic(R)^2, '= 0.06, ', italic(p),'= 0.03')))+
   mythemeplot()+
   theme(axis.title.x=element_blank())
 
   summary(lm( clim.july_sept$mean.rh~clim.july_sept$year))
 summary(lm( clim.july_sept$mean.3.6rh~clim.july_sept$year))
 
 
 tmean.10 <- ggplot(clim.oct,aes(year,tmean,col="Oct"))+
   geom_line()+geom_point()+
   #stat_smooth(method=loess,span=0.02,se=FALSE,lty=2,lwd=1.0)+
   geom_smooth(method = "lm",lty=2)+
   geom_smooth(method = "loess",span=0.2,se = FALSE,lty=1)+
   xlab(label = "Year")+ylab(label = "Temperature (0.1 degree)")+
   annotate("text",x=1990,y=(min(clim.oct$tmean))*1.02,
            label=expression(paste(italic(slope),'= 0.212, ',italic(R)^2, '= 0.09, ', italic(p),'= 0.012')))+
   scale_colour_manual(name="Season",
    values=c("Oct" = 1))+
   mythemeplot()+
   theme(legend.position = "top",axis.title.x=element_blank())
 summary(lm( clim.oct$tmean/10~clim.oct$year))
 
 tmean.7.9 <- ggplot(clim.july_sept,aes(year,mean.tmean,col="July-Sept"))+
   geom_line(aes(col="July-Sept"))+geom_point()+
   #stat_smooth(method=loess,se=FALSE,lty=2,lwd=1.0)+
   #geom_smooth(method = "lm",col="black",lty=2)+
   geom_smooth(method = "loess",span=0.2,col=4,se = F,lty=1)+
   geom_line(aes(year,mean.3.6tmean,col="Mar-June"))+geom_point(aes(year,mean.3.6tmean,col="Mar-June"))+
   geom_smooth(aes(year,mean.3.6tmean,col="Mar-June"),method = "loess",span=0.2,se=F,col=3,lwd=1.5,lty=1)+
   geom_smooth(aes(year,mean.3.6tmean),
               method = "lm",col=3,lty=2)+
   scale_colour_manual(name="Season",
    values=c("Mar-June" = 3, "July-Sept"=4))+
   xlab(label = "Year")+ylab(label = "Temperature (0.1 degree)")+
   annotate("text",x=1980,y=(max(clim.july_sept$mean.tmean,na.rm = TRUE))*0.8,col=3,
            label=expression(paste(italic(slope),'= 0.203, ',italic(R)^2, '= 0.23, ', italic(p),'< 0.001')))+
   mythemeplot()+
   theme(legend.position = "top",
         axis.title.x=element_blank())
 
 summary(lm( clim.july_sept$mean.tmean~clim.july_sept$year))
 summary(lm( clim.july_sept$mean.3.6tmean~clim.july_sept$year))
 

 pdsi.10 <- ggplot(clim.oct,aes(year,scpdsi))+
   geom_line()+geom_point()+
   stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   geom_smooth(method = "lm",col="black",lty=2)+
   geom_smooth(method = "loess",span=0.2,col=1,lwd=1.2,se=F,lty=1)+
   xlab(label = "Year")+ylab(label = "scPDSI")+
    annotate("text",x=1990,y=(max(clim.oct$scpdsi))*0.95,
             label=expression(paste(italic(slope),'= -0.025, ',italic(R)^2, '= 0.03, ', italic(p),'= 0.09')))+
   mythemeplot()
 summary(lm( clim.oct$scpdsi~clim.oct$year))
 
 pdsi.7.9 <- ggplot(clim.july_sept,aes(year,mean.pdsi))+
   geom_line(col=4)+geom_point(col=4)+
   stat_smooth(method=lm,col=4,lty=2,lwd=1.0)+
   geom_smooth(method = "loess",span=0.2,col=4,se=F,lty=1,lwd=1.2)+
   geom_line(aes(year,mean.3.6pdsi),col=3)+
   geom_point(aes(year,mean.3.6pdsi),col=3)+
   stat_smooth(aes(year,mean.3.6pdsi),method=lm,col=3,lty=2,lwd=1.0)+
   geom_smooth(aes(year,mean.3.6pdsi),method = "loess",span=0.2,col=3,se=F,lty=1,lwd=1.2)+
   xlab(label = "Year")+ylab(label = "scPDSI")+
    annotate("text",x=1980,y=(max(clim.july_sept$mean.pdsi,na.rm = TRUE))*0.95,col=4,
             label=expression(paste(italic(slope),'= -0.022, ',italic(R)^2, '= 0.03, ', italic(p),'< 0.09')))+
   annotate("text",x=1980,y=(max(clim.july_sept$mean.pdsi,na.rm = TRUE))*0.80,col=3,
             label=expression(paste(italic(slope),'= -0.033, ',italic(R)^2, '= 0.12, ', italic(p),'= 0.003')))+
   mythemeplot()
 summary(lm( clim.july_sept$mean.pdsi~clim.july_sept$year))
 summary(lm( clim.july_sept$mean.3.6pdsi~clim.july_sept$year))
 
  
  tiff("./plot/Figure S9 climate variability.tiff", width = 20, height = 16,
       units = "cm",res = 400,bg = "transparent",compression = "lzw",
       family = "serif")
  ggarrange(tmean.7.9,tmean.10,
            rh.7.9,rh.10,
            pdsi.7.9,pdsi.10,
            labels = c("a","a1","b","b1",
                       "c","c1"),
            nrow = 3,ncol=2,
            label.x = 0.1,
            label.y = c(0.95,0.95,1.15,1.15,1.15,1.15),
            heights = c(0.55,0.45,0.5),
            align = "hv",
            #common.legend = TRUE,
            font.label = list(size=24,family="serif"))
  dev.off()
