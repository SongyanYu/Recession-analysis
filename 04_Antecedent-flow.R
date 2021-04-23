#---
# identify correlation between antecedent discharge with recession rate
# author: Songyan Yu
# date created: 22/04/2021
#---

SEQ_Clip<-maptools::readShapePoly("../../data/shapfile/SEQ_Clip.shp")

site.segment<-c(873151,
                874394,
                876767,
                877196,
                877254)  # Upper Bremer River, Warrill Creek, Coulson Creek, Wild Cattle Creek, Reynold Creek

catflow.files<-list.files(path = "../../data/AWRA-L/catchment _discharge/",full.names = TRUE)

catflow.lst<-lapply(catflow.files,FUN = function(x){
  read.csv(x)[,-1]
})

catflow.df<-do.call(cbind.data.frame,catflow.lst)

head(catflow.df[,1:5])

library(lubridate)
colnames(catflow.df)<-ymd(gsub("X","",colnames(catflow.df)))
rownames(catflow.df)<-SEQ_Clip$SegmentNo

siteflow.df<-catflow.df[match(site.segment,rownames(catflow.df)),]

recession.date<-read.csv("../../data/Bremer Stream/Recession rate.csv")

site.name<-c("Bremer","Warrill","Coulson","Wild Cattle","Reynold")

bremer.date<-c("15/11/2015",
               "17/12/2015",
               "27/12/2015",
               "7/01/2016",
               "1/02/2016",
               "7/06/2016")

warrill.date<-c("3/09/2015",
               "26/09/2015",
               "20/01/2016",
               "11/02/2016",
               "29/02/2016",
               "7/04/2016",
               "23/04/2016")

coulson.date<-c("30/08/2015",
                "16/11/2015",
                "20/02/2016",
                "13/05/2016")

wild.cattle.date<-c("27/08/2015",
                    "9/01/2016",
                    "21/02/2016",
                    "7/04/2016")

reynold.date<-c("6/10/2015",
                "12/03/2016",
                "12/04/2016")


date.lst<-list(bremer.date,warrill.date,coulson.date,wild.cattle.date,reynold.date)

flow.last.lst<-lapply(1:length(site.name), FUN = function(i){
  date.match<-match(dmy(date.lst[[i]]),ymd(colnames(catflow.df)))
  
  Flow.last.5.lst<-lapply(date.match, FUN = function(x){
    apply(siteflow.df[i,(x-5):(x-1)],1,FUN = mean)
  })
  
  Flow.last.10.lst<-lapply(date.match, FUN = function(x){
    apply(siteflow.df[i,(x-10):(x-1)],1,FUN = mean)
  })
  
  Flow.last.20.lst<-lapply(date.match, FUN = function(x){
    apply(siteflow.df[i,(x-20):(x-1)],1,FUN = mean)
  })
  
  Flow.last.30.lst<-lapply(date.match, FUN = function(x){
    apply(siteflow.df[i,(x-30):(x-1)],1,FUN = mean)
  })
  
  Flow.last.60.lst<-lapply(date.match, FUN = function(x){
    apply(siteflow.df[i,(x-60):(x-1)],1,FUN = mean)
  })
  
  Flow.last.90.lst<-lapply(date.match, FUN = function(x){
    apply(siteflow.df[i,(x-90):(x-1)],1,FUN = mean)
  })
  
  Flow.last.5.df<-do.call(rbind.data.frame,Flow.last.5.lst)
  Flow.last.10.df<-do.call(rbind.data.frame,Flow.last.10.lst)
  Flow.last.20.df<-do.call(rbind.data.frame,Flow.last.20.lst)
  Flow.last.30.df<-do.call(rbind.data.frame,Flow.last.30.lst)
  Flow.last.60.df<-do.call(rbind.data.frame,Flow.last.60.lst)
  Flow.last.90.df<-do.call(rbind.data.frame,Flow.last.90.lst)
  
  Flow.last.df<-cbind(Flow.last.5.df,
                      Flow.last.10.df,
                      Flow.last.20.df,
                      Flow.last.30.df,
                      Flow.last.60.df,
                      Flow.last.90.df)
  
  colnames(Flow.last.df)<-c("flow5","flow10","flow20","flow30","flow60","flow90")
  Flow.last.df$site = site.name[i]
  return(Flow.last.df)
})

library(dplyr)
library(tidyr)

flow.last.5site.df<-do.call(rbind.data.frame,flow.last.lst)%>%
  mutate(recession.rate = recession.date$Recession.rate)

# correlation
library(ggplot2)
flow.last.5site.df%>%
  ggplot()+
  geom_point(aes(x = flow5, y = -recession.rate))+
  facet_wrap(~site,scales = "free")+
  xlab(expression(paste("Mean discharge over last 5 days (m"^"3","/s)")))+
  ylab("Water level recession rate (m/d)")+
  theme_bw()+
  ggsave(filename = "../../Figure/Scatter-plot_antecedent-flow05-vs-recession.png",
         height = 5.73,width = 9.31)

flow.last.5site.df%>%
  ggplot()+
  geom_point(aes(x = flow10, y = -recession.rate))+
  facet_wrap(~site,scales = "free")+
  xlab(expression(paste("Mean discharge over last 10 days (m"^"3","/s)")))+
  ylab("Water level recession rate (m/d)")+
  theme_bw()+
  ggsave(filename = "../../Figure/Scatter-plot_antecedent-flow10-vs-recession.png",
         height = 5.73,width = 9.31)

flow.last.5site.df%>%
  ggplot()+
  geom_point(aes(x = flow20, y = -recession.rate))+
  facet_wrap(~site,scales = "free")+
  xlab(expression(paste("Mean discharge over last 20 days (m"^"3","/s)")))+
  ylab("Water level recession rate (m/d)")+
  theme_bw()+
  ggsave(filename = "../../Figure/Scatter-plot_antecedent-flow20-vs-recession.png",
         height = 5.73,width = 9.31)

flow.last.5site.df%>%
  ggplot()+
  geom_point(aes(x = flow30, y = -recession.rate))+
  facet_wrap(~site,scales = "free")+
  xlab(expression(paste("Mean discharge over last 30 days (m"^"3","/s)")))+
  ylab("Water level recession rate (m/d)")+
  theme_bw()+
  ggsave(filename = "../../Figure/Scatter-plot_antecedent-flow30-vs-recession.png",
         height = 5.73,width = 9.31)

flow.last.5site.df%>%
  ggplot()+
  geom_point(aes(x = flow60, y = -recession.rate))+
  facet_wrap(~site,scales = "free")+
  xlab(expression(paste("Mean discharge over last 60 days (m"^"3","/s)")))+
  ylab("Water level recession rate (m/d)")+
  theme_bw()+
  ggsave(filename = "../../Figure/Scatter-plot_antecedent-flow60-vs-recession.png",
         height = 5.73,width = 9.31)

flow.last.5site.df%>%
  ggplot()+
  geom_point(aes(x = flow90, y = -recession.rate))+
  facet_wrap(~site,scales = "free")+
  xlab(expression(paste("Mean discharge over last 90 days (m"^"3","/s)")))+
  ylab("Water level recession rate (m/d)")+
  theme_bw()+
  ggsave(filename = "../../Figure/Scatter-plot_antecedent-flow90-vs-recession.png",
         height = 5.73,width = 9.31)
