#' daily mean pool water level
#' 

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
  ggplot(aes(x=Date,y=Depth))+
  geom_line()+
  facet_wrap(~Site)+
  theme_bw()+
  ylab("Depth (cm)")+
  ggsave(filename = "Figure/Water level change.png")

nonflow.loss<-read.csv("data/Bremer Stream/PoolHeight2_v2_nonFlowing period.csv")
nonflow.loss$Date<-as.Date(nonflow.loss$Date,format="%d/%m/%Y")
nonflow.loss$Period="non-flowing"

flow.loss<-read.csv("data/Bremer Stream/PoolHeight2_v2_Flowing period.csv")
flow.loss$Date<-as.Date(flow.loss$Date,format="%d/%m/%Y")
flow.loss$Period="flowing"


library(tidyr)
library(dplyr)
library(ggplot2)

temp<-rbind(nonflow.loss,flow.loss)%>%
  filter(Obs.loss_m>=0)%>%
  right_join(.,daily.pool.depth,by=c("Site","Date"))%>%
  mutate(Obs.loss_m=Obs.loss_m*1000)
temp$Date

library(lubridate)

seq.dates<-seq.Date(from = ymd("2015-08-01"),to = ymd("2016-07-31"),by = "day")

match(temp$Date,seq.dates)

break.point<-which(diff(match(temp$Date,seq.dates))!=1)

m<-1
conse.length<-length(m)
while(conse.length < nrow(temp)){
  if(conse.length %in% break.point){
    m<-append(m,1)
  }
  else{
    m<-append(m,(m[length(m)]+1))
  }
  conse.length<-length(m)
}

non.flowing.loss<-temp%>%
  filter(Period == "non-flowing")
flowing.loss<-temp%>%
  filter(Period == "flowing")

rbind(nonflow.loss,flow.loss)%>%
  filter(Obs.loss_m>=0)%>%
  right_join(.,daily.pool.depth,by=c("Site","Date"))%>%
  mutate(Obs.loss_m=Obs.loss_m*1000,
         days.since.flowing = m,
         days.since.flowing = ifelse(Period == "flowing", days.since.flowing, 0))%>%
  ggplot()+
  geom_line(aes(x=Date,y=Depth))+
  geom_point(data = non.flowing.loss,aes(x = Date, y = Obs.loss_m),colour = "#D95F02")+
  geom_point(data = flowing.loss, aes(x = Date, y = Obs.loss_m, color = days.since.flowing))+
#  scale_color_brewer(palette="Dark2")+
  facet_wrap(~Site,scale="free")+
  scale_y_continuous(sec.axis = sec_axis(~.*1,name = "Loss rate (mm/day)"))+
  theme_bw()+
  ylab("Depth (cm)")+
  theme(legend.position = c(0.9,0.2))+
  ggsave(filename = "Figure/Water level and loss.png",width = 10,height = 10/1.564)

#write.csv(daily.pool.depth,file = "data/Bremer Stream/PoolHeight2_v2.csv",row.names = FALSE)

rbind(nonflow.loss,flow.loss)%>%
  filter(Obs.loss_m>=0)%>%
  ggplot()+
  geom_boxplot(aes(x=Site,y=Obs.loss_m,colour=Period))+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  ylab("Recession rate (m/day)")+
  theme(legend.position = c(0.4,0.8))+
  ggsave(filename = "Figure/boxplot_loss rate.png",width = 6,height = 3)


w.test.q<-rbind(nonflow.loss,flow.loss)%>%
  filter(Obs.loss_m>=0)%>%
  pivot_wider(names_from = Period,values_from=Obs.loss_m)%>%
  rename(non_flowing='non-flowing')%>%
  group_by(Site)%>%
  summarise(w.test=wilcox.test(unlist(non_flowing),unlist(flowing))[3])

w.test.w<-rbind(nonflow.loss,flow.loss)%>%
  filter(Obs.loss_m>=0)%>%
  pivot_wider(names_from = Period,values_from=Obs.loss_m)%>%
  rename(non_flowing='non-flowing')%>%
  group_by(Site)%>%
  summarise(w.test=wilcox.test(unlist(non_flowing),unlist(flowing))[1])

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
  scale_size_manual(values = c(3,0.2,1.5))+
  ggsave(filename = "Figure/Pool depth.png")
