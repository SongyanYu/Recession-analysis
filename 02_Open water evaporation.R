#'extract open water evaporation from AWRA-L for the five sites.

setwd("../../")

#---
# 1. read in shapefile
#---
library(maptools)
SEQ.clip<-readShapePoly("data/shapfile/SEQ_Clip.shp")
plot(SEQ.clip)

site.segment<-c(873151,
                874394,
                876767,
                877196,
                877254)  # Upper Bremer River, Warrill Creek, Coulson Creek, Wild Cattle Creek, Reynold Creek


all(match(site.segment,SEQ.clip$SegmentNo)) # should be "TRUE", meaning all site can be found in SEQ
n<-match(site.segment,SEQ.clip$SegmentNo)

#---
# 2. read in open water evaporation netcdf file
#---
# year of 2015
inputfile<-c("data/AWRA-L/Evaporation_Open water/msl_wet_2015_Actual_day.nc")

library(raster)
b<-brick(inputfile,varname="msl_wet")
b<-crop(b,SEQ.clip)
plot(b[[1]])

library(lubridate)
date.2015<-seq.Date(from = as.Date("2015/08/25",format="%Y/%m/%d"),
                    to=as.Date("2015/12/31",format="%Y/%m/%d"),
                    by="day")

evap_2015<-vector("list",length(1:nlayers(b)))
for(i in yday(date.2015[1]):nlayers(b)){                     # 238 means it starts from 25/08/2015, only from that date on the simulated data is invariant.
  ras.nc<-raster(b,layer=i)
  evap_2015[[i]]<-extract(ras.nc,SEQ.clip[n,],na.rm = T, weights = TRUE, fun = "mean", normalizeWeights = TRUE, small = TRUE)
  cat(i,"\n")
}

evap_2015<-evap_2015[lapply(evap_2015,length)>0]
evap_2015.df<-as.data.frame(evap_2015)
colnames(evap_2015.df)<-date.2015

# year of 2016
inputfile<-c("data/AWRA-L/Evaporation_Open water/msl_wet_2016_Actual_day.nc")

b<-brick(inputfile,varname="msl_wet")
b<-crop(b,SEQ.clip)
plot(b[[1]])

date.2016<-seq.Date(from = as.Date("2016/01/01",format="%Y/%m/%d"),
                    to=as.Date("2016/07/13",format="%Y/%m/%d"),
                    by="day")

evap_2016<-vector("list",length(1:nlayers(b)))
for(i in 1:length(date.2016)){                     # 238 means it starts from 25/08/2015, only from that date on the simulated data is invariant.
  ras.nc<-raster(b,layer=i)
  evap_2016[[i]]<-extract(ras.nc,SEQ.clip[n,],na.rm = T, weights = TRUE, fun = "mean", normalizeWeights = TRUE, small = TRUE)
  cat(i,"\n")
}

evap_2016<-evap_2016[lapply(evap_2016,length)>0]
evap_2016.df<-as.data.frame(evap_2016)
colnames(evap_2016.df)<-date.2016

# combine data for 2015 and 2016
evap.df<-cbind(evap_2015.df,evap_2016.df)
evap.df<-data.frame(t(evap.df))
colnames(evap.df)<-c("Bremer", "Warrill", "Coulson", "Wild Cattle", "Reynolds")
evap.df$date<-as.Date(rownames(evap.df),format="%Y-%m-%d")


#---
# 3. observed water loss rate in pools
#---
total.loss<-read.csv("data/Bremer Stream/PoolHeight2_v2_nonFlowing period.csv")
total.loss$Non.flowing.period<-as.Date(total.loss$Non.flowing.period,format="%d/%m/%Y")

library(tidyr)
library(dplyr)
library(ggplot2)
evap.df%>%
  pivot_longer(cols = -date,names_to = "Site",values_to = "evap")%>%
  left_join(.,total.loss,by=c("Site","date"="Non.flowing.period"))%>%
  mutate(Obs.loss_m=Obs.loss_m*1000)%>%
  ggplot(aes(x=date,y=evap,color=Site))+
  geom_line()+
  geom_point(aes(x=date,y=Obs.loss_m))+
  facet_wrap(~Site)




