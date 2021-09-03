#---
# conduct linear mix-effects modelling analysis to test if
# pool recession rates were significantly different between
# flow recession periods in each stream.
# author: Songyan Yu & Ryan Burrows
# date created: 30/08/2021
#---

library(lme4)
library(lmerTest)
require(modelr)

poolh<-read.csv("../../data/Bremer Stream/PoolHeight2_v2_nonFlowing_period_v2.csv")

#Bremer
pool.lmm1 <- lmer((Depth_mm) ~  Day * Period + (1 | Period/Site) , data=(subset(poolh, Site=="Bremer")))
summary(pool.lmm1)

#Coulson
pool.lmm2 <- lmer((Depth_mm) ~  Day * Period + (1 | Period/Site) , data=(subset(poolh, Site=="Coulson")))
summary(pool.lmm2)

#Reynolds
pool.lmm3 <- lmer((Depth_mm) ~  Day * Period + (1 | Period/Site) , data=(subset(poolh, Site=="Reynolds")))
summary(pool.lmm3)

#Warrill
pool.lmm4 <- lmer((Depth_mm) ~  Day * Period + (1 | Period/Site) , data=(subset(poolh, Site=="Warrill")))
summary(pool.lmm4)

#Wild Cattle
pool.lmm5 <- lmer((Depth_mm) ~  Day * Period + (1 | Period/Site) , data=(subset(poolh, Site=="Wild Cattle")))
summary(pool.lmm5)

# develop a single LMM with site as a random factor
pool.lmm <- lmer(Depth_mm ~ Day * Period + (1|Site/Period), data = poolh)  # not what I wanted.
summary(pool.lmm)



# test whether recession rates were linear or non-linear

# linear regression models
pool.lm.b1<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Bremer" & Period == "One"))
pool.lm.b1.r2<-summary(pool.lm.b1)$adj.r.squared


pool.lm.b2<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Bremer" & Period == "Two"))
pool.lm.b2.r2<-summary(pool.lm.b2)$adj.r.squared

pool.lm.b3<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Bremer" & Period == "Three"))
pool.lm.b3.r2<-summary(pool.lm.b3)$adj.r.squared

pool.lm.b4<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Bremer" & Period == "Four"))
pool.lm.b4.r2<-summary(pool.lm.b4)$adj.r.squared

pool.lm.b5<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Bremer" & Period == "Five"))
pool.lm.b5.r2<-summary(pool.lm.b5)$adj.r.squared

pool.lm.b6<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Bremer" & Period == "Six"))
pool.lm.b6.r2<-summary(pool.lm.b6)$adj.r.squared

# non-linear: exponential
nls.data.b1 = subset(poolh, Site =="Bremer" & Period == "One")
pool.exp.b1 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.b1,
                   start = list(a = nls.data.b1$Depth_mm[1], r = -0.1))

pool.exp.b1.r2<-rsquare(pool.exp.b1,nls.data.b1)

nls.data.b2 = subset(poolh, Site =="Bremer" & Period == "Two")
pool.exp.b2 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.b2,
                   start = list(a = nls.data.b2$Depth_mm[1], r = -0.1))
pool.exp.b2.r2<-rsquare(pool.exp.b2,nls.data.b2)

nls.data.b3 = subset(poolh, Site =="Bremer" & Period == "Three")
pool.exp.b3 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.b3,
                   start = list(a = nls.data.b3$Depth_mm[1], r = -0.1))
pool.exp.b3.r2<-rsquare(pool.exp.b3,nls.data.b3)

nls.data.b4 = subset(poolh, Site =="Bremer" & Period == "Four")
pool.exp.b4 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.b4,
                   start = list(a = nls.data.b4$Depth_mm[1], r = -0.1))
pool.exp.b4.r2<-rsquare(pool.exp.b4,nls.data.b4)

nls.data.b5 = subset(poolh, Site =="Bremer" & Period == "Five")
pool.exp.b5 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.b5,
                   start = list(a = nls.data.b5$Depth_mm[1], r = -0.1))
pool.exp.b5.r2<-rsquare(pool.exp.b5,nls.data.b5)

nls.data.b6 = subset(poolh, Site =="Bremer" & Period == "Six")
pool.exp.b6 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.b6,
                   start = list(a = nls.data.b6$Depth_mm[1], r = -0.1))
pool.exp.b6.r2<-rsquare(pool.exp.b6,nls.data.b6)

lm.b<-c(pool.lm.b1.r2,pool.lm.b2.r2, pool.lm.b3.r2,   # r square values for linear regression models for Bremer.
          pool.lm.b4.r2, pool.lm.b5.r2, pool.lm.b6.r2)

exp.b=c(pool.exp.b1.r2,pool.exp.b2.r2,pool.exp.b3.r2,   # r square values for exponential regression models for Bremer.
        pool.exp.b4.r2, pool.exp.b5.r2, pool.exp.b6.r2)
t.test(lm.b, exp.b)  
# p-value = 0.3663. 
# Cannot reject the null hypothesis that the means of the two groups of r square values are not significantly different. 



# Warrill
pool.lm.w1<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Warrill" & Period == "One"))
pool.lm.w1.r2<-summary(pool.lm.w1)$adj.r.squared

pool.lm.w2<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Warrill" & Period == "Two"))
pool.lm.w2.r2<-summary(pool.lm.w2)$adj.r.squared

pool.lm.w3<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Warrill" & Period == "Three"))
pool.lm.w3.r2<-summary(pool.lm.w3)$adj.r.squared

pool.lm.w4<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Warrill" & Period == "Four"))
pool.lm.w4.r2<-summary(pool.lm.w4)$adj.r.squared

pool.lm.w5<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Warrill" & Period == "Five"))
pool.lm.w5.r2<-summary(pool.lm.w5)$adj.r.squared

pool.lm.w6<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Warrill" & Period == "Six"))
pool.lm.w6.r2<-summary(pool.lm.w6)$adj.r.squared

pool.lm.w7<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Warrill" & Period == "Seven"))
pool.lm.w7.r2<-summary(pool.lm.w7)$adj.r.squared

# non-linear: exponential
nls.data.w1 = subset(poolh, Site =="Warrill" & Period == "One")
pool.exp.w1 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.w1,
                   start = list(a = nls.data.w1$Depth_mm[1], r = -0.1))
pool.exp.w1.r2<-rsquare(pool.exp.w1,nls.data.w1)

nls.data.w2 = subset(poolh, Site =="Warrill" & Period == "Two")
pool.exp.w2 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.w2,
                   start = list(a = nls.data.w2$Depth_mm[1], r = -0.1))
pool.exp.w2.r2<-rsquare(pool.exp.w2,nls.data.w2)

nls.data.w3 = subset(poolh, Site =="Warrill" & Period == "Three")
pool.exp.w3 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.w3,
                   start = list(a = nls.data.w3$Depth_mm[1], r = -0.1))
pool.exp.w3.r2<-rsquare(pool.exp.w3,nls.data.w3)

nls.data.w4 = subset(poolh, Site =="Warrill" & Period == "Four")
pool.exp.w4 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.w4,
                   start = list(a = nls.data.w4$Depth_mm[1], r = -0.1))
pool.exp.w4.r2<-rsquare(pool.exp.w4,nls.data.w4)

nls.data.w5 = subset(poolh, Site =="Warrill" & Period == "Five")
pool.exp.w5 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.w5,
                   start = list(a = nls.data.w5$Depth_mm[1], r = -0.1))
pool.exp.w5.r2<-rsquare(pool.exp.w5,nls.data.w5)

nls.data.w6 = subset(poolh, Site =="Warrill" & Period == "Six")
pool.exp.w6 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.w6,
                   start = list(a = nls.data.w6$Depth_mm[1], r = -0.1))
pool.exp.w6.r2<-rsquare(pool.exp.w6,nls.data.w6)

nls.data.w7 = subset(poolh, Site =="Warrill" & Period == "Seven")
pool.exp.w7 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.w7,
                   start = list(a = nls.data.w7$Depth_mm[1], r = -0.1))
pool.exp.w7.r2<-rsquare(pool.exp.w7,nls.data.w7)

lm.w<-c(pool.lm.w1.r2, pool.lm.w2.r2, pool.lm.w3.r2,   # r square values for linear regression models for Warrill
        pool.lm.w4.r2, pool.lm.w5.r2, pool.lm.w6.r2, pool.lm.w7.r2)

exp.w=c(pool.exp.w1.r2,pool.exp.w2.r2,pool.exp.w3.r2,   # r square values for exponential regression models for Bremer.
        pool.exp.w4.r2, pool.exp.w5.r2, pool.exp.w6.r2, pool.exp.w7.r2)
t.test(lm.w, exp.w) 


# Coulson
pool.lm.c1<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Coulson" & Period == "One"))
pool.lm.c1.r2<-summary(pool.lm.c1)$adj.r.squared

pool.lm.c2<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Coulson" & Period == "Two"))
pool.lm.c2.r2<-summary(pool.lm.c2)$adj.r.squared

pool.lm.c3<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Coulson" & Period == "Three"))
pool.lm.c3.r2<-summary(pool.lm.c3)$adj.r.squared

pool.lm.c4<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Coulson" & Period == "Four"))
pool.lm.c4.r2<-summary(pool.lm.c4)$adj.r.squared

nls.data.c1 = subset(poolh, Site =="Coulson" & Period == "One")
pool.exp.c1 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.c1,
                   start = list(a = nls.data.c1$Depth_mm[1], r = -0.1))
pool.exp.c1.r2<-rsquare(pool.exp.c1,nls.data.c1)

nls.data.c2 = subset(poolh, Site =="Coulson" & Period == "Two")
pool.exp.c2 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.c2,
                   start = list(a = nls.data.c2$Depth_mm[1], r = -0.1))
pool.exp.c2.r2<-rsquare(pool.exp.c2,nls.data.c2)

nls.data.c3 = subset(poolh, Site =="Coulson" & Period == "Three")
pool.exp.c3 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.c3,
                   start = list(a = nls.data.c3$Depth_mm[1], r = -0.1))
pool.exp.c3.r2<-rsquare(pool.exp.c3,nls.data.c3)

nls.data.c4 = subset(poolh, Site =="Coulson" & Period == "Four")
pool.exp.c4 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.c4,
                   start = list(a = nls.data.c4$Depth_mm[1], r = -0.1))
pool.exp.c4.r2<-rsquare(pool.exp.c4,nls.data.c4)

lm.c<-c(pool.lm.c1.r2, pool.lm.c2.r2, pool.lm.c3.r2,   # r square values for linear regression models for Warrill
        pool.lm.c4.r2)

exp.c=c(pool.exp.c1.r2,pool.exp.c3.r2,   # r square values for exponential regression models for Warrill
        pool.exp.c4.r2)
t.test(lm.c, exp.c) 

# Reybolds
pool.lm.r1<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Reynolds" & Period == "One"))
pool.lm.r1.r2<-summary(pool.lm.r1)$adj.r.squared

pool.lm.r2<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Reynolds" & Period == "Two"))
pool.lm.r2.r2<-summary(pool.lm.r2)$adj.r.squared

pool.lm.r3<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Reynolds" & Period == "Three"))
pool.lm.r3.r2<-summary(pool.lm.r3)$adj.r.squared

nls.data.r1 = subset(poolh, Site =="Reynolds" & Period == "One")
pool.exp.r1 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.r1,
                   start = list(a = nls.data.r1$Depth_mm[1], r = -0.1))
pool.exp.r1.r2<-rsquare(pool.exp.r1,nls.data.r1)

nls.data.r2 = subset(poolh, Site =="Reynolds" & Period == "Two")
pool.exp.r2 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.r2,
                   start = list(a = nls.data.r2$Depth_mm[1], r = -0.1))
pool.exp.r2.r2<-rsquare(pool.exp.r2,nls.data.r2)

nls.data.r3 = subset(poolh, Site =="Reynolds" & Period == "Three")
pool.exp.r3 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.r3,
                   start = list(a = nls.data.r3$Depth_mm[1], r = -0.1))
pool.exp.r3.r2<-rsquare(pool.exp.r3,nls.data.r3)

lm.r<-c(pool.lm.r1.r2, pool.lm.r2.r2, pool.lm.r3.r2)   # r square values for linear regression models for Reynolds
exp.r=c(pool.exp.r1.r2,pool.exp.r2.r2,pool.exp.r3.r2)   # r square values for exponential regression models for Reynolds
t.test(lm.r, exp.r) 


# Wild Cattle
pool.lm.wc1<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Wild Cattle" & Period == "One"))
pool.lm.wc1.r2<-summary(pool.lm.wc1)$adj.r.squared

pool.lm.wc2<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Wild Cattle" & Period == "Two"))
pool.lm.wc2.r2<-summary(pool.lm.wc2)$adj.r.squared

pool.lm.wc3<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Wild Cattle" & Period == "Three"))
pool.lm.wc3.r2<-summary(pool.lm.wc3)$adj.r.squared

pool.lm.wc4<-lm(Depth_mm ~ Day, data = subset(poolh, Site =="Wild Cattle" & Period == "Four"))
pool.lm.wc4.r2<-summary(pool.lm.wc4)$adj.r.squared

nls.data.wc1 = subset(poolh, Site =="Wild Cattle" & Period == "One")
pool.exp.wc1 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.wc1,
                   start = list(a = nls.data.wc1$Depth_mm[1], r = -0.1))
pool.exp.wc1.r2<-rsquare(pool.exp.wc1,nls.data.wc1)

nls.data.wc2 = subset(poolh, Site =="Wild Cattle" & Period == "Two")
pool.exp.wc2 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.wc2,
                    start = list(a = nls.data.wc2$Depth_mm[1], r = -0.1))
pool.exp.wc2.r2<-rsquare(pool.exp.wc2,nls.data.wc2)

nls.data.wc3 = subset(poolh, Site =="Wild Cattle" & Period == "Three")
pool.exp.wc3 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.wc3,
                    start = list(a = nls.data.wc3$Depth_mm[1], r = -0.1))
pool.exp.wc3.r2<-rsquare(pool.exp.wc3,nls.data.wc3)

nls.data.wc4 = subset(poolh, Site =="Wild Cattle" & Period == "Four")
pool.exp.wc4 <- nls(Depth_mm ~  a * exp(r * Day), data = nls.data.wc4,
                    start = list(a = nls.data.wc4$Depth_mm[1], r = -0.1))
pool.exp.wc4.r2<-rsquare(pool.exp.wc4,nls.data.wc4)


lm.wc<-c(pool.lm.wc1.r2, pool.lm.wc2.r2, pool.lm.wc3.r2, pool.lm.wc4.r2)   # r square values for linear regression models for Reynolds
exp.wc=c(pool.exp.wc1.r2,pool.exp.wc2.r2,pool.exp.wc3.r2, pool.exp.wc4.r2)   # r square values for exponential regression models for Reynolds
t.test(lm.wc, exp.wc) 

lm.b
exp.b
lm.w
exp.w
lm.c
exp.c
lm.r
exp.r

lm.wc
exp.wc

