##This code is used to process the meteorological data
## The aims are:
###     1. extract the signal station data from the whole dataset, this will run fastly than matlab code.
###     2.check the data # detec the missing value, interploate the missing data or delete the 
###
##Author: GB Xu, 2018-6-14

library(openxlsx)
library(dplyr)
library(ggplot2)
library(treeclim)
library(grid)

#####1. read the data and select one station data-----
path<-'H:/G/XGBMeteor/'
docs<-dir(path)
setwd(path)
TEM<-"SURF_CLI_CHN_MUL_DAY-TEM-12001*.TXT"##check all the files according to one rule.
RH<-"SURF_CLI_CHN_MUL_DAY-RHU-13003*.TXT"#SURF_CLI_CHN_MUL_DAY-RHU-13003-195212
PRS<-"SURF_CLI_CHN_MUL_DAY-PRS-10004-*.TXT"
PRE<-"SURF_CLI_CHN_MUL_DAY-PRE-13011-*.TXT"
EVP<-"SURF_CLI_CHN_MUL_DAY-EVP-13240*.TXT"
WID<-"SURF_CLI_CHN_MUL_DAY-WIN-11002-*.TXT"
SSD<-"SURF_CLI_CHN_MUL_DAY-SSD-14032-*.TXT"
GST<-"SURF_CLI_CHN_MUL_DAY-GST-12030-0cm*.TXT"

fns = Sys.glob(RH)
data<-data.frame(NULL)
for (fn in fns) {
  data1=read.table(fn) ##read .txt as a data.frame
  data.select<-subset(data1,data1$V1==57776)
  data=rbind(data,data.select)
}
library(openxlsx)# call the openxlsx packages.
write.xlsx(data,file = "E:/Rwork/Freezingrain/tem57776_1.xlsx")

## the following is used my function to extract the data which can save code
  ### the function format include that  extractstation<-function(nameVar,station,varshortname,path)
    ##
## for the detail of the data can see ./SURF_CLI_CHN_MUL_DAY_FORMAT.doc
path1<-"E:/Rwork/Freezingrain/"
stationNo=57872
extractstation(TEM,stationNo,"tem",path1)
extractstation(PRS,stationNo,"prs",path1)
extractstation(RH,stationNo,"rh",path1)
extractstation(PRE,stationNo,"pre",path1)
extractstation(EVP,stationNo,"evp",path1)
extractstation(WID,stationNo,"wid",path1)
extractstation(SSD,stationNo,"ssd",path1)
extractstation(GST,stationNo,"gst",path1)

###2.1 Read the monthly data and plot the figures------
mdata<-read.table("E:/Rwork/Freezingrain/S201806051426312322400.txt",header = TRUE)
ny.mdata<-subset(mdata,mdata$V01000==57776)
ny.mdata[ny.mdata==32766]<-NA
ny.mymdata.full<-subset(ny.mdata,select = c(1:3,11,17,21,23,26,6,14,5))

#write.xlsx(hy.mymdata,"E:/Rwork/Freezingrain/hengyang.xlsx")

varname<-c("month","station","year","pre.day",
           "tmean","water.pressure","rh","pre","tmin","tmax","ssd")
colnames(ny.mymdata.full)<-varname
ny.mymdata.mean <- ny.mymdata.full %>%
                      filter(year>1952)%>%
                      group_by(month)%>%
                      summarise_each(funs(mean(.,na.rm=TRUE)))



## 2.1.1 processing the missing data and write as ssd1 and evp.-----
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

##details of the unit and variable name can be found in SURF_CLI_CHN_MUL_MON_readme.txt
## precipitation, temperature and water vapor pressure unit is 0.1..


  #2.1.1 plot the climtagraph at month------
  library(plotrix)
tiff("Figure 1. Climate in the sampling site.tiff",width=26,height = 14,units = "cm",compression = "lzw",res = 400)
windowsFonts(TN = windowsFont("Times New Roman"))  
par(mfcol=c(1,2),mgp=c(2.0,0.5,0),mar=c(2.5,1.0,1.5,0.5),family="TN")
  
 month<-1:12
 twoord.plot(month,ny.mymdata.mean$pre/10,month,ny.mymdata.mean$tmean/10,lcol="gray60",rcol=1,lylim=c(50,300),
             xlim=c(1,12),xlab="Month",rylab="Temperature (℃)",ylab="Precipitation (mm)",
             type=c("bar","b"),xticklab=month.abb,halfwidth=0.2) ##type中b为折线类型，bar为柱状图类型，xticklab设置横坐标标签，halfwidth设置柱的半宽
 #first legend
 legend(1, 290, legend = c("Precipitation"), fill=c("grey60"),bty="n") 
 #second legend
 legend(1, 280, legend = c("Temperature"),lty = 1, col = c("black"),pch=2,bty="n")
 text(1,290,labels = "a",cex = 2)
       
 twoord.plot(month,ny.mymdata.mean$pre.day,month,ny.mymdata.mean$rh,lcol="gray70",rcol=1,lylim=c(9,22),rylim=c(75,95),lytickpos = c(5,10,15,20),
             xtickpos=c(1:12),xlim=c(1,12),xlab="Month",rylab="Relative humidity (%)",ylab="Precipitation (> 0.1 mm) days",
             type=c("bar","b"),xticklab=month.abb,halfwidth=0.2) ##type中b为折线类型，bar为柱状图类型，xticklab设置横坐标标签，halfwidth设置柱的半宽
 #first legend
 legend(6.5, 22, legend = c("Precipitation days"), fill=c("grey70"),bty="n") 
 #second legend
 legend(6.5, 21, legend = c("Relative humidity"),lty = 1, col = c("black"),pch=2,bty="n")
 text(1,21.5,labels = "b",cex = 2)
 dev.off()
 
###2.1.2 plot the variability of climatic parameters-------
 head(ny.mymdata)## check for the climatic parameters
 
 clim.jun_oct<-subset(ny.mymdata,month %in% c(10))
 clim.feb_july<-subset(ny.mymdata,month %in% c(2,3,4,5,6,7))
 
 clim.ann<-ny.mymdata %>% group_by(year)%>%
   summarise(mean.preday=mean(pre.day,na.rm=TRUE),mean.tmean=mean(tmean,na.rm=TRUE),
             mean.presure=mean(water.pressure,na.rm=TRUE),mean.rh=mean(rh,na.rm=TRUE),
             mean.pre=mean(pre,na.rm=TRUE),mean.tmin=mean(tmin,na.rm=TRUE),
             mean.tmax=mean(tmax,na.rm=TRUE),mean.ssd=mean(ssd,na.rm=TRUE),
             mean.evp=mean(evp,na.rm=TRUE),mean.vpd=mean(vpd,na.rm=TRUE),
             mean.dtr=mean(dtr,na.rm=TRUE))
   
 clim.jun_oct.mean<-clim.jun_oct %>% group_by(year)%>%
   summarise(mean.preday=mean(pre.day,na.rm=TRUE),mean.tmean=mean(tmean,na.rm=TRUE),
             mean.presure=mean(water.pressure,na.rm=TRUE),mean.rh=mean(rh,na.rm=TRUE),
             mean.pre=mean(pre,na.rm=TRUE),mean.tmin=mean(tmin,na.rm=TRUE),
             mean.tmax=mean(tmax,na.rm=TRUE),mean.ssd=mean(ssd,na.rm=TRUE))
 clim.feb_may.mean<-clim.feb_may %>% group_by(year)%>%
   summarise(mean.preday=mean(pre.day,na.rm=TRUE),mean.tmean=mean(tmean,na.rm=TRUE),
             mean.presure=mean(water.pressure,na.rm=TRUE),mean.rh=mean(rh,na.rm=TRUE),
             mean.pre=mean(pre,na.rm=TRUE),mean.tmin=mean(tmin,na.rm=TRUE),
             mean.tmax=mean(tmax,na.rm=TRUE),mean.ssd=mean(ssd,na.rm=TRUE))
 
 p.value <-NA
   
for(i in 1:length(unique(clim.jun_oct$month)) ) {
  pslope=pvaluecal2(unique(clim.jun_oct$month)[i],
                   group=1,clim.jun_oct,ix=3,iy=11)
  p.value <- cbind(p.value,pslope)
}
 p.value <- p.value[-1]
 
 ssd.7.11<-ggplot(clim.jun_oct,aes(year,ssd,color=factor(month)))+
  geom_line()+geom_point()+
   stat_smooth(data=subset(clim.jun_oct,month %in% c(8,9)),
     method=lm,se=FALSE,lty=2,lwd=1.0)+
   #geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Sunshine duration hours")+
   guides(color=guide_legend(title="Month"))+
   mythemeplot()
   #annotate("text",x=1955,y=max(clim.aug_oct$ssd,na.rm = TRUE),label="a",family="TN",size=10)
 summary(lm( clim.jun_oct.mean$mean.ssd[-1]~clim.aug_oct.mean$year[-1]))
 
 clim.oct <-clim.jun_oct %>%
   filter(month==10)
 write.xlsx(clim.oct,"./data/climateOct.xlsx")
 
 p.value<- NA
 for(i in 1:length(unique(clim.feb_july$month)) ) {
  pslope=pvaluecal2(unique(clim.feb_july$month)[i],
                   group=1,clim.feb_july,ix=3,iy=11)
  p.value <- cbind(p.value,pslope)
}
 p.value <- p.value[-1]
 
 ssd.2.7<-ggplot(clim.feb_july,aes(year,ssd,color=factor(month)))+
  geom_line()+geom_point()+
   stat_smooth(data=subset(clim.feb_july,month %in% c(6,7)),
     method=lm,se=FALSE,lty=2,lwd=1.0)+
   #geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Sunshine duration hours")+
   guides(color=guide_legend(title="Month"))+
   mythemeplot()
 
 p.value<- NA
 for(i in 1:length(unique(ny.mymdata$month)) ) {
  pslope=pvaluecal2(unique(ny.mymdata$month)[i],
                   group=1,ny.mymdata,ix=3,iy=16)
  p.value <- cbind(p.value,pslope)
}
 p.value[-1]
 head(ny.mymdata)
 
 tmean.7.11<-ggplot(clim.jun_oct,aes(year,tmean/10,color=factor(month)))+
  geom_line()+geom_point()+
   stat_smooth(
     data=subset(clim.jun_oct,month %in% c(10,11)),
     method=lm,se=FALSE,lty=2,lwd=1.0)+
   #geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+
   ylab(label = "Mean temperature")+
    guides(color=guide_legend(title="Month"))+
   mythemeplot()
   #annotate("text",x=1955,y=max(clim.aug_oct$ssd,na.rm = TRUE),label="a",family="TN",size=10)
 summary(lm( clim.jun_oct.mean$mean.ssd[-1]~clim.aug_oct.mean$year[-1]))
 
 tmean.2.7<-ggplot(clim.feb_july,aes(year,tmean/10,color=factor(month)))+
  geom_line()+geom_point()+
   stat_smooth(
     data=subset(clim.feb_july,month %in% c(2,4,5,6)),
     method=lm,se=FALSE,lty=2,lwd=1.0)+
   #geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+
   ylab(label = "Mean temperature")+
   guides(color=guide_legend(title="Month"))+
   mythemeplot()
 
 pre.7.11<-ggplot(clim.jun_oct,aes(year,pre/10,color=factor(month)))+
  geom_line()+geom_point()+
   stat_smooth(
     data=subset(clim.jun_oct,month %in% c(10)),
     method=lm,se=FALSE,lty=2,lwd=1.0)+
   #geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+
   ylab(label = "Precipitation")+
    guides(color=guide_legend(title="Month"))+
   mythemeplot()
  
 pre.2.7<-ggplot(clim.feb_july,aes(year,pre/10,color=factor(month)))+
  geom_line()+geom_point()+
   stat_smooth(
     data=subset(clim.feb_july,month %in% c(5)),
     method=lm,se=FALSE,lty=2,lwd=1.0)+
   #geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+
   ylab(label = "Precipitation")+
   guides(color=guide_legend(title="Month"))+
   mythemeplot()
 
 rh.7.11<-ggplot(clim.jun_oct,aes(year,rh,color=factor(month)))+
  geom_line()+geom_point()+
   # stat_smooth(
   #   data=subset(clim.jun_oct,month %in% c(8)),
   #   method=lm,se=FALSE,lty=2,lwd=1.0)+
   #geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+
   ylab(label = "RH(%)")+
    guides(color=guide_legend(title="Month"))+
   mythemeplot()
  
 rh.2.7<-ggplot(clim.feb_july,aes(year,rh,color=factor(month)))+
  geom_line()+geom_point()+
   stat_smooth(
     data=subset(clim.feb_july,month %in% c(4,6)),
     method=lm,se=FALSE,lty=2,lwd=1.0)+
   #geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+
   ylab(label = "RH(%)")+
   guides(color=guide_legend(title="Month"))+
   mythemeplot()
 
 pdsilot.2.7 <- ggplot(
   subset(crupdsi.l,
          variable %in% c("Feb","Mar","Apr",
                          "May","Jun","Jul")))+
   geom_line(aes(x=date,y=value,
                 col=factor(variable,levels = unique(variable))))+
   stat_smooth(data=subset(crupdsi.l,
          variable %in% c("Feb","Mar","Apr",
                          "May","Jun")),
          aes(x=date,y=value,col=factor(variable)),
     method=lm,se=FALSE,lty=2,lwd=1.0)+
    scale_x_date(expand = c(0,0),
                 labels = date_format("%Y"),
                 limits = as.Date(c("1950-01-01","2015-01-01")),
                 breaks = "10 years")+
   guides(col=guide_legend(title="Month"))+
   mythemeplot()+
   xlab("Year")+ylab("PDSI")
   
pdsilot.7.11 <- ggplot(
   subset(crupdsi.l,
          variable %in% c("Aug","Sep","Oct",
                          "Nov")))+
   geom_line(aes(x=date,y=value,
                 col=factor(variable,levels = unique(variable))))+
   stat_smooth(data=subset(crupdsi.l,
          variable %in% c("Nov")),
          aes(x=date,y=value,col=factor(variable)),
     method=lm,se=FALSE,lty=2,lwd=1.0)+
    scale_x_date(expand = c(0,0),
                 labels = date_format("%Y"),
                 limits = as.Date(c("1950-01-01","2015-01-01")),
                 breaks = "10 years")+
   guides(col=guide_legend(title="Month"))+
   mythemeplot()+
   xlab("Year")+ylab("PDSI") 
 
  tiff("./plot/Figure SX climate variability.tiff", width = 25, height = 36,
       units = "cm",res = 400,bg = "transparent",compression = "lzw",
       family = "serif")
  ggarrange(tmean.2.7,tmean.7.11,
            pre.2.7,pre.7.11,
            rh.2.7,rh.7.11,
            ssd.2.7,ssd.7.11,
            pdsilot.2.7,pdsilot.7.11,
            labels = c("a","a1","b","b1",
                       "c","c1","d","d1",
                       "e","e1"),
            nrow = 5,ncol=2,
            label.x = 0.1,
            label.y = 1,
            font.label = list(size=24,family="serif"))
  dev.off()


 ssd.8.10<-ggplot(clim.jun_oct.mean[-1,],aes(year,mean.ssd))+
   geom_line()+geom_point()+
   stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Sunshine duration hours")+
   annotate("text",x=1975,y=(min(clim.jun_oct.mean$mean.ssd[-1]))*1.01,
            label=expression(paste(italic(slope),'=-5.897, ',italic(R)^2, '= 0.15, ', italic(p),'= 0.002')))+
   mythemeplot()
 

 ssd.5.7<-ggplot(clim.feb_may.mean,aes(year,mean.ssd))+
   geom_line()+geom_point()+
   # stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   # geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Sunshine duration hours")+
   # annotate("text",x=1990,y=(max(clim.may_july.mean$mean.ssd,na.rm = TRUE))*0.95,
   #          label=expression(paste(italic(slope),'=-7.903, ',italic(R)^2, '= 0.116, ', italic(p),'= 0.005')))+
   mythemeplot()
 summary(lm( clim.feb_may.mean$mean.ssd~clim.may_july.mean$year))
 
 rh.8.10<-ggplot(clim.jun_oct.mean,aes(year,mean.rh))+
   geom_line()+geom_point()+
   stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Relative humidity (%)")+
   annotate("text",x=1988,y=(min(clim.jun_oct.mean$mean.rh,na.rm = TRUE))*1.02,
            label=expression(paste(italic(slope),'= 0.038, ',italic(R)^2, '= 0.06, ', italic(p),'= 0.05')))+
   mythemeplot()
 summary(lm( clim.jun_oct.mean$mean.rh~clim.aug_oct.mean$year))
 
 rh.5.7<-ggplot(clim.feb_may.mean,aes(year,mean.rh))+
   geom_line()+geom_point()+
   stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Relative humidity (%)")+
   annotate("text",x=1980,y=(min(clim.feb_may.mean$mean.rh,na.rm = TRUE))*1.02,
          label=expression(paste(italic(slope),'=-0.045, ',italic(R)^2, '= 0.06, ', italic(p),'= 0.04')))+
   mythemeplot()
 
   summary(lm( clim.feb_may.mean$mean.rh~clim.feb_may.mean$year))
 
 pre.8.10<-ggplot(clim.jun_oct.mean,aes(year,mean.pre))+
   geom_line()+geom_point()+
   #stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   #geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Precipitation (0.1 mm)")+
   # annotate("text",x=1990,y=(max(clim.aug_oct$ssd,na.rm = TRUE))*0.95,
   #          label=expression(paste(italic(slope),'=-7.903, ',italic(R)^2, '= 0.116, ', italic(p),'= 0.005')))+
   mythemeplot()
 summary(lm( clim.jun_oct.mean$mean.pre~clim.aug_oct.mean$year))
 
 
 summary(lm( clim.feb_may.mean$mean.pre~clim.may_july.mean$year))
 pre.5.7<- ggplot(clim.feb_may.mean,aes(year,mean.pre))+
   geom_line()+geom_point()+
   stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Precipitation (0.1 mm)")+
   annotate("text",x=1990,y=(max(clim.jun_oct.mean$mean.pre))*0.95,
            label=expression(paste(italic(slope),'= -7.722, ',italic(R)^2, '= 0.106, ', italic(p),'= 0.01')))+
   mythemeplot()
 
 tmean.8.10<-ggplot(clim.jun_oct.mean,aes(year,mean.tmean))+
   geom_line()+geom_point()+
   stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Temperature (0.1 degree)")+
   annotate("text",x=1990,y=(max(clim.jun_oct.mean$mean.tmean))*1.01,
            label=expression(paste(italic(slope),'= 0.088, ',italic(R)^2, '= 0.131, ', italic(p),'= 0.004')))+
   mythemeplot()
 summary(lm( clim.jun_oct.mean$mean.tmean~clim.aug_oct.mean$year))
 
 tmean.5.7<-ggplot(clim.feb_may.mean,aes(year,mean.tmean))+
   geom_line()+geom_point()+
   stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Temperature (0.1 degree)")+
   annotate("text",x=1980,y=(max(clim.feb_may.mean$mean.tmean,na.rm = TRUE))*1.01,
            label=expression(paste(italic(slope),'= 0.274, ',italic(R)^2, '= 0.244, ', italic(p),'< 0.001')))+
   mythemeplot()
 summary(lm( clim.feb_may.mean$mean.tmean~clim.feb_may.mean$year))
 
 
 filename<-paste("./plot/monthly-climate-variability-mean-value",width,".tiff",sep = "")
 tiff(filename,
      width = 28,height =29,units = "cm",compression = "lzw",bg="white",family = "serif",res=500)
 
 pushViewport(viewport(layout = grid.layout(4,2))) ####将页面分成2*1矩阵
 print(tmean.8.10, vp = vplayout(1,2))   ###将(1,1)的位置画图pd1
 print(tmean.5.7,vp = vplayout(1,1))
 print(pre.8.10, vp = vplayout(2,2))   ###将(1,1)的位置画图pd1
 print(pre.5.7,vp = vplayout(2,1))   ###将(2,1)的位置画图b
 print(rh.8.10, vp = vplayout(3,2))   ###将(1,1)的位置画图pd1
 print(rh.5.7,vp = vplayout(3,1))   ###将(2,1)的位置画图b
 print(ssd.8.10, vp = vplayout(4,2))   ###将(1,1)的位置画图pd1
 print(ssd.5.7,vp = vplayout(4,1))
 
 dev.off()
## 2.1.2 plot the seasonal mean for the reconstruction-----
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
 
 head(clim.july_sept)
 head(clim.oct)
 head(clim.mar_jun)
 
 summary(lm(clim.oct[-1,]$ssd~clim.oct[-1,]$year))
 
 ssd.10 <- ggplot(clim.oct[-1,],aes(year,ssd))+
   geom_line()+geom_point()+
   geom_smooth(method="loess",span=0.2,se=TRUE,lty=1,lwd=1.0)+
   #geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Sunshine duration hours")+
   # annotate("text",x=1975,y=(min(clim.oct$ssd[-1]))*1.01,
   #          label=expression(paste(italic(slope),'=-5.897, ',italic(R)^2, '= 0.15, ', italic(p),'= 0.002')))+
   mythemeplot()
 

 ssd.7.9 <- ggplot(clim.july_sept,aes(year,mean.ssd))+
   geom_line()+geom_point()+
   stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   geom_smooth(method = "loess",span=0.2,col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Sunshine duration hours")+
   # annotate("text",x=1990,y=(max(clim.may_july.mean$mean.ssd,na.rm = TRUE))*0.95,
   #          label=expression(paste(italic(slope),'=-7.903, ',italic(R)^2, '= 0.116, ', italic(p),'= 0.005')))+
   mythemeplot()
 summary(lm( clim.july_sept$mean.ssd~clim.july_sept$year))
 
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
 
 pre.10 <- ggplot(clim.oct,aes(year,pre))+
   geom_line()+geom_point()+
   stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   geom_smooth(method = "lm",col="black",lty=2)+
   geom_smooth(method = "loess",span=0.2,se=F,lty=1)+
   xlab(label = "Year")+ylab(label = "Precipitation (0.1 mm)")+
    annotate("text",x=1990,y=(max(clim.oct$pre,na.rm = TRUE))*0.95,
             label=expression(paste(italic(slope),'=-1.48, ',italic(R)^2, '= 0.078, ', italic(p),'= 0.015')))+
   mythemeplot()
 summary(lm( clim.oct$pre/10~clim.oct$year))
 
 
 summary(lm( clim.july_sept$mean.pre~clim.july_sept$year))
 
 pre.7.9 <- ggplot(clim.july_sept,aes(year,mean.pre))+
   geom_line()+geom_point()+
   # stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   # geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Precipitation (0.1 mm)")+
   # annotate("text",x=1990,y=(max(clim.oct$mean.pre))*0.95,
   #          label=expression(paste(italic(slope),'= -7.722, ',italic(R)^2, '= 0.106, ', italic(p),'= 0.01')))+
   mythemeplot()
 
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
 
 evp.10<-ggplot(clim.oct,aes(year,evp))+
   geom_line()+geom_point()+
   stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Temperature (0.1 degree)")+
   # annotate("text",x=1990,y=(max(clim.oct$evp))*1.01,
   #          label=expression(paste(italic(slope),'= 0.212, ',italic(R)^2, '= 0.09, ', italic(p),'= 0.012')))+
   mythemeplot()
 summary(lm( clim.oct$evp~clim.oct$year))
 
 evp.7.9 <- ggplot(clim.july_sept,aes(year,mean.evp))+
   geom_line()+geom_point()+
   stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
   geom_smooth(method = "lm",col="black",lty=2)+
   xlab(label = "Year")+ylab(label = "Temperature (0.1 degree)")+
    annotate("text",x=1980,y=(max(clim.july_sept$mean.evp,na.rm = TRUE))*1.01,
             label=expression(paste(italic(slope),'= -0.103, ',italic(R)^2, '= 0.09, ', italic(p),'< 0.007')))+
   mythemeplot()
 summary(lm( clim.july_sept$mean.evp~clim.july_sept$year))
 
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
 
 tiff("./plot/Figure S7 climate variability.tiff", width = 20, height = 16,
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

### 2.1.3 for the cloudcover data ------
 # read cloud cover data from CRU
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
  
  tiff(file="./plot/Cloud cover for 1950-now.tiff",width = 16,height = 14,units ="cm",compression="lzw",bg="white",family = "serif",res=600)
  print(crucld.plot)
  dev.off()
  
 tiff(file="./plot/Cloud cover for 1900-now.tiff",width = 16,height = 14,units ="cm",compression="lzw",bg="white",family = "serif",res=600)
  print(crucld.longplot)
  dev.off()
  
##2.2 process the daily data to 10-days data-----
  ## 2.2.1 load the observation data---------
ftype<-".xlsx"
tem57776<-read.xlsx(pro_file(path1,"tem57776",ftype))
tem57776[tem57776==32766]<-NA

pre57776<-read.xlsx(pro_file(path1,"pre57776",ftype))
##here, the precipitation is complex than temperature, including the snow, precipitation, frost and dew!!
## We used the 20-20 daily data, taking into account the frost and dew!! seee the raw data!!

rh57776<-read.xlsx(pro_file(path1,"rh57776",ftype))
rh57776[rh57776==32766]<-NA

evp57776<-read.xlsx(pro_file(path1,"evp57776",ftype))
evp57776[evp57776==32766]<-NA

ssd57776<-read.xlsx(pro_file(path1,"ssd57776",ftype))
ssd57776[ssd57776==32766]<-NA

library(lubridate) ## process the date data
## change the date as the day of the year, prepare for Maideniso model
day.n <- yday(as.Date(with(tem57776, 
                     paste(V5, V6, V7,sep="-")), "%Y-%m-%d"))
day.p <- yday(as.Date(with(pre57776, 
  paste(V5, V6, V7,sep="-")), "%Y-%m-%d"))
##yday(day.n)

CO2_daily<- read.xlsx("E:/xgb/xgb/data/CO2/MALCO2.xlsx")
CO2_daily <- subset(CO2_daily,year>1974 & year <2015,select = c(1:8))
head(CO2_daily)
## inteplate the missing value
library(stinepack)
library(imputeTS)## This package used to interpolate the time series

#imp<-mice(CO2_daily[,c(2:4,8),3]) ## check for the missing value
#value <- spline(CO2_daily$month,CO2_daily$value,n=365,method = "fmm")
CO2_daily$value1<-na.interpolation(CO2_daily$value,option="linear")
CO2_daily$day.co2 <- yday(as.Date(with(CO2_daily, 
  paste(year, month, day,sep="-")), "%Y-%m-%d"))

day.temp <- cbind(tem57776$V5,day.n,tem57776[,c(9,10)]/10,
                  day.p,pre57776[,c(10)]/10)
all.equal(day.temp$day.n,day.temp$day.p)## determine the two conclumns have same data

write.csv(day.temp,"./rawdata/temp_input1.csv")
write.csv(CO2_daily,"./rawdata/Manol_CO2_daily.csv")

input<-read.csv("./rawdata/temp_input.csv")
input <- subset(input,tem57776.V5>1952 & tem57776.V5<2015)
write.csv(input,"./rawdata/temp_input.csv")
plot(day.p,pre57776[,c(10)]/10,"l")

## 2.2.2 calculate the mean value for the each 10 days--------
tem10day<-NULL
  for (i in seq(1953, 2014, 1)) {
  temtmp <-
    daily_trans1 (
      subset(tem57776, tem57776$V5 == i, select = c(1:7,8)),
      10,
      8,
      fun = c("mean"),
      na.rm = TRUE
    )
  temday <- t(subset(temtmp, select = c(8)))
  tem10day <- rbind(tem10day, c(i, temday))
  }

tmax10day <- NULL
  for (i in seq(1953, 2014, 1)) {
  tmaxtmp <-
    daily_trans1 (
      subset(tem57776, tem57776$V5 == i, select = c(1:7, 9)),
      10,
      8,
      fun = c("mean"),## here, calculate the mean value of the maximum temperature
      na.rm = TRUE
    )
  tmaxday <- t(subset(tmaxtmp, select = c(8)))## note here, the 8th conclumn is tmax
  tmax10day <- rbind(tmax10day, c(i, tmaxday))
  }

tmin10day <- NULL
for (i in seq(1953, 2014, 1)) {
  tmintmp <-
    daily_trans1 (
      subset(tem57776, tem57776$V5 == i, select = c(1:7,10)),
      10,
      8,
      fun = c("mean"),
      na.rm = TRUE
    )
  tminday <- t(subset(tmintmp, select = c(8)))
  tmin10day <- rbind(tmin10day, c(i, tminday))
}

pre10day <- NULL
for (i in seq(1953, 2014, 1)) {
  pretmp <-
    daily_trans1 (
      subset(pre57776, pre57776$V5 == i, select = c(1:7, 10)),
      10,
      8,
      fun = c("sum"),
      na.rm = TRUE
    )
  preday <- t(subset(pretmp, select = c(8)))
  pre10day <- rbind(pre10day, c(i, preday))
}

rh10day <- NULL
for (i in seq(1953, 2014, 1)) {
  rhtmp <-
    daily_trans1 (
      subset(rh57776, rh57776$V5 == i, select = c(1:7, 8)),
      10,
      8,
      fun = c("mean"),
      na.rm = TRUE
    )
  rhday <- t(subset(rhtmp, select = c(8)))
  rh10day <- rbind(rh10day, c(i, rhday))
}

evp10day <- NULL
for (i in seq(1953, 2014, 1)) {
  evptmp <-
    daily_trans1 (
      subset(evp57776, evp57776$V5 == i, select = c(1:7, 8)),
      10,
      8,
      fun = c("mean"),
      na.rm = TRUE
    )
  evpday <- t(subset(evptmp, select = c(8)))
  evp10day <- rbind(evp10day, c(i, evpday))
}

## process the missing value

library(mice)
imp<-mice(evp10day,10)
fit<-with(imp,lm(V1~V2))
pooled<-pool(fit)
result4=complete(imp,action=3)
evp10day<-result4

ssd10day <- NULL
for (i in seq(1953, 2014, 1)) {
  ssdtmp <-
    daily_trans1 (
      subset(ssd57776, ssd57776$V5 == i, select = c(1:7, 8)),
      10,
      8,
      fun = c("mean"),
      na.rm = TRUE
    )
  ssdday <- t(subset(ssdtmp, select = c(8)))
  ssd10day <- rbind(ssd10day, c(i, ssdday))
}
## NOTE: the unit of pre (0.1),tem (0.1), evp(0.1) and ssd(0.1)
ny.mymdta.res<-subset(ny.mymdata,year>1952 & year<2017,select=c(3,1,4:11))
ny.mymdta.res<-subset(ny.mymdta.res,year>1952 & year<2017)
o_A_resp <-dcc(region.oxygen.chron[2], ny.mymdta.res[,c(1,2,4)], selection=.range("tmean",1:12)+.mean("tmean",2:10)+.mean("tmean",1:10),method = "correlation",var_names =c("tmean"))
o_A_resp1 <-dcc(region.oxygen.chron[15], ny.mymdta.res, selection=.range("tmin",-12:11)+.mean("tmin",3:6)+.mean("tmin",7:11),
               method = "correlation",var_names =c("pre.day","tmean","water.pressure","rh","pre","tmin","tmax","ssd"))

plot(o_A_resp)
plot(o_A_resp1)

# 3.0 process the stable oxygen data from Changsha station and IsoRSM data------
## this code is used to process the stable isotope data from the model
## 3.1 process the monthly output data----
####3.1.1 for the precipitation -----
data <- read.delim("F:/IsoGSM/x061y062_ensda_monthly.dat",header = FALSE)
data1<-data[c(-1,-2),c(-1)]
data1.ts<-ts(data1,start = c(1871,1),frequency = 12)
p.oxyts<-ts((data1$V6/data1$V5-1)*1000,start = c(1871,1),frequency = 12)
p.oxy<-(data1$V6/data1$V5-1)*1000
p.oxy[abs(p.oxy)>13]<-NA ## remove the outliers, set the threshold is abs(13)

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

### process for the stable oxygen isotope of the water vapor at monthly scales
vp.oxy<-(data1$V15/data1$V14-1)*1000
vp.oxy[abs(vp.oxy)>30]<-NA ## remove the outliers, set the threshold is abs(30)
## reference: Xie Yulong, Zhang Xinping, et al., Monitoring and analysis of stable isotopes of the near surface water vapor in 
## Changsha, Environmental Science, 2016,37(2):475-481

monthvp.oxy<-as.data.frame(matrix(vp.oxy,ncol=12,byrow=TRUE))
colnames(monthvp.oxy)<-c(1:12)
monthvp.oxy<-cbind(year=c(1871:2010),monthvp.oxy)

plot(monthvp.oxy[,3],type="l")

library(reshape2)
library(ggplot2)

p.rateoxy.shape<-melt(p.rateoxy)
plot(p.rateoxy.shape$Var2,p.rateoxy.shape$value,xlab =" month",ylab="d18O of precipitation (‰)")

windowsFonts(TN = windowsFont("Times New Roman")) 
To<-ggplot(region.oxygen,aes(x=ratio,y=region.oxygen$oxy))+geom_point()+
    geom_smooth(method="loess",level=0.99,alpha=0.5)+
    ylim(22,35)+xlab("Proportion to boundary")+ylab(expression(paste(delta^"18","O (‰)")))+
    scale_x_continuous(limits = c(0,1))+
    annotate("text",x=0.01,y=35,label = "a",family="TN",size=10)+
    mythemeplot()
To<-ggplot(scale5.chron,aes(x=group,y=mo))+geom_point()+
  geom_smooth(method="loess",level=0.99,alpha=0.5)+
  ylim(22,35)+xlab("Section of annual ring")+ylab(expression(paste(delta^"18","O (‰)")))+
  scale_x_continuous(breaks = c(1:10),labels = c(getvar(5,"EW"),getvar(5,"LW")))+
  #annotate("text",x=1,y=35,label = "a",family="TN",size=10)+
  mythemeplot()

To## tree-ring oxygen

po<-ggplot(p.rateoxy.shape,aes(x=Var2,y=value))+geom_point()+
  geom_smooth(method = "loess",col=4,level=0.99,alpha=0.5)+
   xlab("Month") + ylab(expression(paste(delta^"18","O (‰)")))+ylim(-15,6)+
  scale_x_continuous(limits = c(1,12),breaks=c(1:12), labels = month.abb)+
  #annotate("text",x=1,y=5.8,label = c("b"),family="TN",size=10)+
  mythemeplot()+
  annotate("rect",xmin=2.8,xmax=11.2,ymin=-15,ymax=2,col="gray70",alpha=0.2)
po## precipitation oxygen

library(grid)
grid.newpage()  ##新建页面
tiff(file="./plot/Figure 2b scale 5 intra-annual tree-ring oxygen with preciptation.tiff",width = 26,height = 12,units ="cm",compression="lzw",bg="white",res=600)
pushViewport(viewport(layout = grid.layout(1,2))) ####将页面分成2*1矩阵

 print(To, vp = vplayout(1,1))   ###将(1,1)的位置画图pd1
 print(po,vp = vplayout(1,2))   ###将(2,1)的位置画图b

dev.off()

# 3.1.1.2 plot the variability of monthly oxygen------
o.long<-ggplot(p.rateoxy.shape,aes(x=Var1,y=value,color=factor(Var2)))+
  geom_line()+
  stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
  geom_smooth(method = "lm",col="black",lty=2)
  
  o.long.gs<-ggplot(subset(p.rateoxy.shape,Var2==c(2,3,4) & Var1>1949),
                    aes(x=Var1,y=value,color=factor(Var2)))+
     geom_line()+
     stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
      geom_smooth(method = "lm",na.rm=TRUE,col=4,level=0.95,alpha=0.5)+
        xlab("Month") + ylab(expression(paste(delta^"18","O (‰)")))+ylim(-13,2)+
        scale_color_discrete(name="Month")+  ## modify the title of the legned
        scale_x_continuous(breaks=seq(1870,2010,20),
                       labels=seq(1870,2010,20))+
       annotate("text",x=1871,y=2,label = c("a"),family="TN",size=10)+
       #guides(fill=guide_legend(title=NULL))+
        theme(legend.position=c(0.2, 0.15))+
         mythemeplot()
        
  #+scale_colour_manual(breaks = c("0","1","3","6","9","12"))
    # scale_fill_manual(values=cbPalette)
o.long.gs

  o.long.gs1<-ggplot(subset(p.rateoxy.shape,Var2==c(5,7,9,10)& Var1>1949),aes(x=Var1,y=value,color=factor(Var2)))+
           geom_line()+
           stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
          geom_smooth(method = lm,col=4,level=0.95,alpha=0.5)+
            xlab("Month") + ylab(expression(paste(delta^"18","O (‰)")))+ylim(-13,2)+
         scale_color_discrete(name="Month")+  ## modify the title of the legned
         scale_x_continuous(breaks=seq(1870,2010,20),
                       labels=seq(1870,2010,20))+
        annotate("text",x=1871,y=2,label = c("b"),family="TN",size=10)+
        theme(legend.position=c(0.2, 0.15))+
        mythemeplot()
         
  o.long.gs1
  
  library(grid)
  grid.newpage()  ##新建页面
  tiff(file="Figure S3 variability of oxygen in preciptation.tiff",width = 16,height = 20,units ="cm",compression="lzw",bg="white",res=600)
  pushViewport(viewport(layout = grid.layout(2,1))) ####将页面分成2*1矩阵
  
  print(o.long.gs, vp = vplayout(1,1))   ###将(1,1)的位置画图pd1
  print(o.long.gs1,vp = vplayout(2,1))   ###将(2,1)的位置画图b
  
  dev.off()
  
##3.2 plot and compare seasonal oxygen isotpe in precipitation ------
  ## The precipitation d18O data from ISOGSM model
  ## The precipitation data from the GNIP Changsha station--
### 3.2.0 Plot the shangsha station data------
  oxy.changsha <- read.xlsx("./rawdata/wiser_gnip-monthly-cn-gnipm.xlsx",
                            sheet = "Data",colNames = TRUE)
  head(oxy.changsha)
  oxy.changsha.reshape <- subset(oxy.changsha,select=c(SampleName, month, O18))
  colnames(oxy.changsha.reshape) <- c("Var1","Var2","value")
  
  ##split the data from GNIP
  oxy.changsha.reshape.1999 <-subset(oxy.changsha.reshape,Var1>1999)
  
  ## this dataset is from Zhou et al. 2019 STE, observation from Hunan Normal Univ.
  oxy.changsha.2010 <-subset(oxy.changsha.reshape,Var1>1999)
  
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
  
  oxy.ts<-ggplot(p.oxy,aes(x=date, y=value, col=type)) + 
     geom_line()+
     xlab("Year")+ylab(expression(paste(delta ^"18","O")))+
    mythemeplot()+
    theme(legend.position = c(0.5,0.05),legend.title = element_blank(),
          legend.direction = "horizontal")
  
  changsha.ts <- ts(oxy.changsha.reshape$value,frequency = 12, start = 1988)
  model.ts<- ts(p.rateoxy.shape[order(p.rateoxy.shape$Var1),]$value,
           frequency = 12,start =c(1871, 01))
  
  plot(changsha.ts,xlab="Year",ylab=expression(paste(delta ^"18","O")))
  lines(model.ts,col=2 ) 
  
  
  tiff("./plot/comparison-oxygen-isotope-in-pre-and-model.tiff",width = 26,height = 20,
       units = "cm",bg="white",res = 300,family = "serif")
  
  ggarrange(oxy.p, # First row with scatter plot
            oxy.ts,
            po,
            To,
            heights = c(1, 1),
            ncol = 2, align = "v",
            nrow = 2,
            labels = c("a","b","c","d"), # Labels of the scatter plot
            font.label = list(size = 30),
            label.x = 0.12,
            label.y = 0.98
  )
  dev.off()
  
  r.preoxy <-cor.test(oxy.changsha.reshape$O18,p.rateoxy.shape.1988$value)
  
  
##3.2.1 plot and for the seasonal oxygen isotpe in precipitation from ISOGSM model------
  pre.oxy5.8<-subset(p.rateoxy.shape,Var2 %in% c(5,6,7,8))
  pre.oxy2.4<-subset(p.rateoxy.shape,Var2 %in% c(2,3,4,5))
  pre.oxy2.10<-subset(p.rateoxy.shape,Var2 %in% c(2,3,4,5,6,7,8,9,10))
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
  
  pre.oxy5.8.plot<-ggplot(subset(pre.oxy5.8.mean,Var1>1950 & Var1<2010),aes(Var1,mean.value))+
    geom_line()+geom_point()+
    # stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
    # geom_smooth(method = "lm",col="black",lty=2)+
    xlab(label = "Year")+ylab(label = expression(paste("May-Aug  ",delta^"18","O in precipitation (‰)")))+
     # annotate("text",x=1955,y=(max(pre.oxy5.10.mean$mean.value,na.rm = TRUE))*0.95,
     #          label="May-August")+
    mythemeplot()
  summary(lm( subset(pre.oxy5.8.mean,Var1>1950)$mean.value~subset(pre.oxy5.8.mean,Var1>1950)$Var1))
  
  tiff(file="./plot/stable oxygen in May-Aug preciptation for 1950-now.tiff",width = 12,height = 8,units ="cm",compression="lzw",bg="white",res=600)
  print(pre.oxy5.8.plot)
  dev.off()
  
  
  
  pre.oxy2.4.plot<-ggplot(subset(pre.oxy2.4.mean,Var1>1899),aes(Var1,mean.value))+
    geom_line()+geom_point()+
    #stat_smooth(method=lm,se=FALSE,lty=2,lwd=1.0)+
    geom_smooth(method = "lm",col="black",lty=2)+
    xlab(label = "Year")+ylab(label = expression(paste(delta^"18","O in precipitation (‰)")))+
    #xlim(1953,2017)+
     annotate("text",x=1950,y=(max(pre.oxy2.4.mean$mean.value,na.rm = TRUE))*0.95,
              label="a. Feb-May")+
    annotate("text",x=1970,y=-1,
                       label=expression(paste(italic(slope),'=-0.0092, ',italic(R)^2, '= 0.14, ', italic(p),'< 0.001')))+
    mythemeplot()
  summary(lm( subset(pre.oxy2.4.mean,Var1>1899 & Var1!=1917)$mean.value~subset(pre.oxy2.4.mean,Var1>1899& Var1!=1917)$Var1))
  
  tiff(file="./plot/stable oxygen in preciptation for 1900-now.tiff",width = 26,height = 12,units ="cm",compression="lzw",bg="white",res=600)
  pushViewport(viewport(layout = grid.layout(1,2))) ####将页面分成2*1矩阵
  
  print(pre.oxy2.4.plot, vp = vplayout(1,1))   ###将(1,1)的位置画图pd1
  print(pre.oxy5.10.plot,vp = vplayout(1,2))   ###将(2,1)的位置画图b
  
  dev.off() 
  pre.diff<-pre.oxy5.10.mean$mean.value-pre.oxy2.4.mean$mean.value
  pre.diff<-cbind(c(1871:2010),pre.diff)
  
  fit.abs<-lm(abs(subset(pre.diff,pre.diff[,1]>1899)[,2])~c(1900:2010))
  summary(fit.abs)
  
  tiff(file="./plot/diff in preciptation for 1900-now.tiff",width = 12,height = 8,units ="cm",compression="lzw",bg="white",res=600)
  plot(pre.diff[,1],
       abs(pre.diff[,2]),"l",xli=c(1900,2014),
       xlab="year",ylab="Difference in absolute")
  abline(fit.abs,lty=2)
  text(1950,1.5,
    label=expression(paste(italic(slope),'=-0.0093, ',italic(R)^2, '= 0.08, ', italic(p),'= 0.003')))
  dev.off()

  ### 3.2.2 plot the trend of vapor d18O-----
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
  
  tiff(file="./plot/stable oxygen in Feb-Nov vapour for 1950-now.tiff",width = 16,height = 14,units ="cm",compression="lzw",bg="white",family = "serif",res=600)
  print(monthvp.oxy2.11.plot)
  dev.off()
  
  tiff(file="./plot/stable oxygen in Feb-Nov vapour for 1900-now.tiff",width = 16,height = 14,units ="cm",compression="lzw",bg="white",family = "serif",res=600)
  print(monthvp.oxy2.11.longplot)
  dev.off()
##3.3 process the hourly data of the ISOGSM output ---- 
  
  ## here, to calculate the oxygen according to original data!!
  ##where SMOW=[H18O]/[H2O] or [HDO]/[H2O] in Standard Mean Ocean Water.
  ##To calculate delta18o in precipitation, do followings:
  ##delta18O_p[permil]=(PRATE18O/PRATE-1)*1000 for the delta18O in VPD, do the
  ##following equation: d18O of 2m vapor = (SPFH12m/SPFH2m-1)*1000, as similar as precipitation.
  
  path<-c("F:/IsoGSM/x061y062_isogsm2_6hrly_")
  p.vp.oxy<-NULL
  for(i in seq(1979,2017,by=1)){
  filename<-paste0(path,i,".dat")
  hour6data <- read.delim(filename,header = FALSE,skip = 2)
  hour6data1<-hour6data[,c(-1)]
  
  hour6p.oxy<-data.frame((hour6data1$V6/hour6data1$V5-1)*1000)
  hour6p.oxy[abs( hour6p.oxy)>20.5]<-NA ## remove the outliers, set the threshold is abs(20.5)
  ## reference: Xie Yulong, Zhang Xinping, et al., Monitoring and analysis of stable isotopes of the near surface water vapor in 
  ## Changsha, Environmental Science, 2016,37(2):475-481
  daypre.oxy<-n.colmeans(hour6p.oxy, 4)
  
  
  hour6vp.oxy<-data.frame((hour6data1$V15/hour6data1$V14-1)*1000)
  hour6vp.oxy[abs(hour6vp.oxy)>30]<-NA ## remove the outliers, set the threshold is abs(30)
  ## reference: Xie Yulong, Zhang Xinping, et al., Monitoring and analysis of stable isotopes of the near surface water vapor in 
  ## Changsha, Environmental Science, 2016,37(2):475-481
  dayvp.oxy<-n.colmeans(hour6vp.oxy, 4)
  p.vp.oxy.tmp<-cbind(rep(i,nrow(daypre.oxy)),daypre.oxy,dayvp.oxy)
  p.vp.oxy<-rbind(p.vp.oxy,p.vp.oxy.tmp)
  }
  colnames(p.vp.oxy)<-c("year","day1","oxy.p","day2","oxy.vp")
 head(p.vp.oxy)
 dim(p.vp.oxy)
 plot(p.vp.oxy$year,p.vp.oxy$oxy.p,"l") 
 

# 3.2.1 explore which season or days the precipitation d18O determine the d18O in tree-ring------
  ## at the daily time scales?
  ## at the monthly time scale

 library(dendroExtra)## use the package to detect the day time-sacle response
 library(treeclim)
 library(gtools)## herein, we used the function mixedsort in gtools
 
 
 ###3.2.2 different parts of the chronolgy -------
 rownames(chron.iso.scale5)<-c(1900:2014)
 chron.iso.scale5.ts<-ts(chron.iso.scale5,start = 1900,frequency = 1)
 
 p.rateoxy.clim<-cbind(seq(1871,2010),p.rateoxy)
 p.rateoxy.clim<-runningclimate (p.rateoxy.clim)
 
 for (j in (2:11)){## first col is year
   assign(getvar(10,"chron.corr")[j-1],Climateplot (chron.iso.scale5.ts[,j],
                                                 Climatesite=p.rateoxy.clim,
                                                 fyr=1980,lyr=2010,
                                                 detrended=c("No"),
                                              spline.length=0))}
##get the plot
for (i in (1:10)){## first col is year
  #levelplot(t(chron.corr),lwd=0.4,col.regions=colorRampPalette(c("red","yellow","white","green3","blue")),at=c(seq(-0.8,0.8,0.05)),xlab="Months",ylab="Window length")
 assign(getvar(10,"chron.corrplot")[i],contourplot(t(get(paste("chron.corr",i,sep=""))),region=T,lwd=0.3,lty=2,
             col.regions=colorRampPalette(c("red","yellow","white","green3","blue")),
             at=c(seq(-0.8,0.8,0.05)),xlab="Months",ylab="Window length",main=title))}

for (i in (1:10)){
 filename=paste("./plot/correlation-tree-ring-pre-oxygen-part",i,"1980-2010.tiff")
tiff(filename,width=22, height=12,units="cm",res = 300)
print(get(paste("chron.corrplot",i,sep="")))
dev.off()
}
#par(mfcol=c(5,2),mar=c(3.5,3.5,0,0),mgp=c(2,1,0),family="serif")
#plot(chron.corrplot1, split = c(1, 1, 5, 2))#nx,ny



 
 ####4.2-----climate response-------
 ### 4.2.1  tmean for the d18O----------
 tem<-tem10day[,c(1,9:20)]# tem is for the prior growing season from DOY 81 to DOY 200
 tem1<-tem10day[,c(1,21:32)]# tem is the current growing season DOY210-320
 tem2<-tem10day[,c(1,33:36, 1:8)]# tem is the current growing season DOY330-360, so we calculate the previous year
 d4.c<-d3.p<-d2.c<-d1.c<-data.frame(id=c(NA),varname=c(NA),month=c(NA),coef=c(NA),significant=c(NA),ci_lower=c(NA), ci_upper=c(NA))
 for (i in c(2:5)) {
   ##here, 2:10 is the mean value from April to June, 1:10 is the mean from March to June !!
   o_A_resp <-dcc(chron.iso.scale5[i], list(tem=p.rateoxy), selection=.range("tem",1:12)+.mean("tem",2:10)+.mean("tem",1:10),
                  method = "correlation",na.action(),var_names =c("tem"))
   d1.c<-rbind(d1.c,o_A_resp$coef)
   ##here, 1:7 is the mean value from July (last) to September, 1:10 is the mean from July to october (first two)!!
   o_resp<-dcc(region.oxygen.chron[i], list(tem=tem1), selection=.range("tem",1:12)+.mean("tem",1:7)+.mean("tem",1:10),method = "correlation",var_names =c("tem"))
   d2.c<-rbind(d2.c,o_resp$coef)
   ##here, -2:-10 is the mean value from winter December to Feb, 1:12 for all the prior growing season!!
   o_resp.p<-dcc(region.oxygen.chron[i], list(tem=tem2), selection=.range("tem",-1:-4)+.range("tem",5:12)+.mean("tem", 5:12)+.mean("tem",1:12),method = "correlation",var_names =c("tem"))
   d3.p<-rbind(d3.p,o_resp.p$coef)
   o_m_resp <-dcc(region.oxygen.chron[i], ny.mymdta.res, selection=.range("tmean",-11:11)+.mean("tmean",-11:3)+.mean("tmean",3:6)+.mean("tmean",7:11),
                  method = "correlation",var_names =c("pre.day","tmean","water.pressure","rh","pre","tmin","tmax","ssd"))
   d4.c<-rbind(d4.c,o_m_resp$coef)
 }
 
 d1.c<-na.omit(d1.c);d2.c<-na.omit(d2.c);d3.p<-na.omit(d3.p);d4.c<-na.omit(d4.c)
 chronname<-c(rep("EW1",14),rep("EW3",14),rep("LW3",14),rep("LW4",14))
 chronname1<-c(rep("EW1",16),rep("EW3",16),rep("EW3",16),rep("LW1",16))
  
  temtmp <-
    daily_trans1 (
      subset(tem57776, tem57776$V5 == i, select = c(1:7, 8)),
      10,## the intervlas of the data
      8,## the conlumn of the data that you want to be processed.
      fun = c("mean"),
      na.rm = TRUE
    )
  temday <- t(subset(temtmp, select = c(8)))
  
  

## 4 stable oxygen isotope simulation using BRFE04 model -------------------------------
 
  #oxy.sim<-simpleBRFE04model(Ta,rh,alti,d18Osw,d18Owva,a1,a2,a6,a7,Bs,L,pex,px)
  #herein, the d18O and d18Owva are from the ISOGSM model outputs
  # Ta, rh and atli are from the Nanyue station
  # a1 and a2 are from the reference?? a1=2.57; a2=0.89
  # a3 and a4 are from the reference
  # a6 is the mean value of the stable isotope value for the period 1901-2014:
  # a7 is for the bias correction 0.4.
  # Bs is the boundary layer conductance
  # L is the length of effective, 
  # pex=1.0; 
  # pex= [0.1~0.6]； pex may change with different species (Song et al., 2014, 0.26 for the gymnosperms),
  # the pex is usually used for the most of the species is 0.4 (Gessller et al. 2016)
  
  ## model for the monthly? or for the any of the parts of the cellulose?----
  ## the climate data is from 1953 to 2014; the oxygen isotope data cover the period 1871-2010. 
  alti=603;a1=2.57; a2=0.89; 
  a6=apply(subset(region.oxygen.chron[12],rownames(region.oxygen.chron)> 1952 & rownames(region.oxygen.chron)<2011),2,mean)
  a7=0.45
  oxy.sim<-simpleBRFE04model(Ta=tem10day[c(1:58),19]/10,rh=rh10day[c(1:58),19],alti=603,
                             d18Osw=subset(p.rateoxy[,7],rownames(p.rateoxy)>1952 & rownames(p.rateoxy)<2011),
                             d18Owva=subset(monthvp.oxy[,7],rownames(monthvp.oxy)>1952 & rownames(monthvp.oxy)<2011),
                             a1,a2,a6,a7,
                             Bs=0.2,
                             L=0.018,
                             pex=0.42,
                             px=1)
  
  plot(c(1953:2010),oxy.sim[[9]],type = "l",xlim=c(1900,2014),ylim=c(24,36),
       xlab="Year", ylab="Tree-ring d18O")
  par(new=TRUE)
  plot(c(1900:2014),region.oxygen.chron[12][,1],type="l",col=2,xlim=c(1900,2014),ylim=c(24,36),xlab = "",ylab="")
  legend(1950,35,legend = c("measured tree-ring","Modeled tree-ring"),col=c(2,1),lty=1,bty="n")
  
  cor.test(oxy.sim[[9]],subset(region.oxygen.chron[10],rownames(region.oxygen.chron)> 1952 & rownames(region.oxygen.chron)<2011)[,1])
  
  
  ## only the oxygen of precipitation
  plot(c(1900:2014),scale(region.oxygen.chron[10][,1]),
       type="l",col=2,xlim=c(1900,2014),ylim=c(-2,4),xlab="Year",ylab="Z-score")
  par(new=TRUE)
  plot(c(1871:2010),scale(p.rateoxy[,6]),type="l",
       xlim=c(1900,2014),ylim=c(-2,4),xlab="",ylab="")
  legend(1960,3.5,legend = c("Tree-ring","Precipitation"),col=c(2,1),lty=1,bty="n")
  
  cor.test(subset(region.oxygen.chron[12],rownames(region.oxygen.chron)> 1899 & rownames(region.oxygen.chron)<2011)[,1],
           subset(p.rateoxy[,7],rownames(p.rateoxy)> 1899 & rownames(p.rateoxy)<2011))
  
