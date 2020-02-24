# calculate recession rate for flowing and non-flowing periods, respectively.

# 1. read in water height logger records
setwd("../../")

pool.level<-read.csv("data/Bremer Stream/PoolHeight2.csv")

library(lubridate)
obs.date<-as.Date(pool.level$DateTime,format="%d/%m/%Y")
pool.level$"Day"<-day(obs.date)

library(dplyr)
daily.pool.depth<-pool.level%>%
  group_by(Site,Year,Month,Day)%>%
  summarise(Depth=mean(Depth2))%>%
  ungroup()%>%
  mutate(Date=as.Date(paste(Year,Month,Day,sep="/"),format="%Y/%m/%d"))

disconnection.depth<-read.csv(file = "data/Bremer Stream/Disconnection depth.csv")
disconnection.depth$Discon_depth<-disconnection.depth$Discon_depth*100

library(ggplot2)

df<-daily.pool.depth%>%
  left_join(.,disconnection.depth,by="Site")%>%
  mutate(diff=c(-diff(Depth),0),
         flowing=ifelse(Depth>=Discon_depth,"Y","N"))

  df%>%ggplot(aes(x=Date,y=Depth))+
  geom_line()+
    geom_point(aes(y=diff))+
  facet_wrap(~Site)+
  theme_bw()+
  ylab("Depth (cm)")



