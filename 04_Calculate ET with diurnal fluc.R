#---
# calculate evapotranspiration based on diurnal fluctuations of water level monitored in isolated pools.
# Author: Songyan Yu
# date created: 13/08/2021
#---

# 1. read in water height logger records
pool.level<-read.csv("../../data/Bremer Stream/PoolHeight2.csv")

library(lubridate)
obs.date<-as.Date(pool.level$DateTime,format="%d/%m/%Y")
pool.level$"Day"<-day(obs.date)

library(dplyr)
library(ggplot2)
pool.level%>%
  mutate(Date=as.Date(paste(Year,Month,Day,sep="/"),format="%Y/%m/%d"))%>%
  ggplot(aes(x=Date,y=Depth2))+
  geom_line()+
  facet_wrap(~Site)+
  theme_bw()+
  ylab("Depth (cm)")


