#' daily mean pool water level
#' 

# 1. read in water height logger records
pool.level<-read.csv("../../data/Bremer Stream/PoolHeight2.csv")

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
  ggsave(filename = "../../Figure/Water level change.png")

nonflow.loss<-read.csv("../../data/Bremer Stream/PoolHeight2_v2_nonFlowing period.csv")
nonflow.loss$Date<-as.Date(nonflow.loss$Date,format="%d/%m/%Y")
nonflow.loss$Period="cease-to-flow"

flow.loss<-read.csv("../../data/Bremer Stream/PoolHeight2_v2_Flowing period.csv")
flow.loss$Date<-as.Date(flow.loss$Date,format="%d/%m/%Y")
flow.loss$Period="flowing"

library(tidyr)
library(dplyr)
library(ggplot2)

rbind(nonflow.loss,flow.loss)%>%
  filter(Obs.loss_m>=0)%>%
  right_join(.,daily.pool.depth,by=c("Site","Date"))%>%
  mutate(Obs.loss_m=Obs.loss_m*10)%>%   # I was not sure whether 10 is right, but the figure seem correct.
  ggplot(aes(x=Date))+
  geom_line(aes(y=Depth/100))+
  geom_point(aes(y = Obs.loss_m, color = Period))+
  scale_color_brewer(palette="Dark2",direction = -1)+
  facet_wrap(~Site,scale="free")+
  scale_y_continuous(sec.axis = sec_axis(~.*0.1,name = "Recession rate (m/day)"))+
  theme_bw()+
  ylab("Pool water depth (m)")+
  theme(legend.position = c(0.9,0.2))+
  ggsave(filename = "../../Figure/Water level and loss.png",width = 10,height = 10/1.564)

#write.csv(daily.pool.depth,file = "data/Bremer Stream/PoolHeight2_v2.csv",row.names = FALSE)

rbind(nonflow.loss,flow.loss)%>%
  filter(Obs.loss_m>=0)%>%
  ggplot()+
  geom_boxplot(aes(x=Site,y=Obs.loss_m,colour=Period))+
  scale_color_brewer(palette = "Dark2",direction = -1)+
  theme_bw()+
  ylab("Recession rate (m/day)")+
  theme(legend.position = c(0.4,0.8))+
  ggsave(filename = "../../Figure/boxplot_loss rate.png",width = 6,height = 3)


w.test.q<-rbind(nonflow.loss,flow.loss)%>%
  filter(Obs.loss_m>=0)%>%
  pivot_wider(names_from = Period,values_from=Obs.loss_m)%>%
  rename(cease_to_flow='cease-to-flow')%>%
  group_by(Site)%>%
  summarise(w.test=wilcox.test(unlist(cease_to_flow),unlist(flowing))[3])

w.test.w<-rbind(nonflow.loss,flow.loss)%>%
  filter(Obs.loss_m>=0)%>%
  pivot_wider(names_from = Period,values_from=Obs.loss_m)%>%
  rename(cease_to_flow='cease-to-flow')%>%
  group_by(Site)%>%
  summarise(w.test=wilcox.test(unlist(cease_to_flow),unlist(flowing))[1])

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
  ggsave(filename = "../../Figure/Pool depth.png")
