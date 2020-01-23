#' daily mean pool water level
#' 
#' @export

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

library(ggplot2)
daily.pool.depth%>%
  ggplot(aes(x=Date,y=Depth,colour=Site))+
  geom_line()+
  facet_wrap(~Site)

#write.csv(daily.pool.depth,file = "data/Bremer Stream/PoolHeight2_v2.csv",row.names = FALSE)

# 2. add flowing and non-flowing to figures
period.photo<-read.csv("data/Bremer Stream/Flowing and non-flowing period_photos.csv")

period.photo$Starting.date<-as.Date(period.photo$Starting.date,format="%d/%m/%Y")
period.photo$Ending.date<-as.Date(period.photo$Ending.date,format="%d/%m/%Y")
levels(period.photo$Site)==levels(daily.pool.depth$Site)

library(lubridate)
library(dplyr)
date.within<-function(aim.date,site.name){
  data<-period.photo%>%filter(Site==site.name)
  if(any(aim.date %within% interval(data[,2],data[,3]))){
    return(data$Value[which(aim.date %within% interval(data[,2],data[,3]))])
  }
  else{return(-99)}
}

for(i in 1:nrow(daily.pool.depth)){
  daily.pool.depth$status[i]=date.within(daily.pool.depth$Date[i],daily.pool.depth$Site[i])
  
}

library(tidyr)
daily.pool.depth%>%
  transmute(no.data=Depth,
         non.flowing=ifelse(status==0,Depth,NA),
         flowing=ifelse(status==1,Depth,NA),
         Site,
         Date)%>%
  pivot_longer(cols = c("no.data","non.flowing","flowing"),names_to = "status",values_to = "Depth")%>%
  ggplot()+
  geom_line(aes(x=Date,y=Depth,colour=Site,size=status))+
  facet_wrap(~Site)+
  scale_size_manual(values = c(3,0.2,1.5))

















