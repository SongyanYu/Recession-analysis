#' daily mean pool water level
#' 
#' @export

# read in logging data
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

library(ggplot2)
daily.pool.depth%>%
  ggplot(aes(x=Date,y=Depth,colour=Site))+
  geom_line()+
  facet_wrap(~Site)
