rm(list=ls())
# Running Blue Whiting example

# _____ fit models______
library(stockassessment)
load(file = "code/data/bw/dat.Rdata")

# __Case 1: OFFICIAL WGWIDE ASSESSMENT__
conf <- loadConf(dat, file = "code/conf/bw/wgwide.cfg")
par <- defpar(dat,conf)
fit <- sam.fit(dat,conf,par)
save(fit, file = "code/model/bw/wgwide.Rdata")


# __Case 1b: variance weighted model run__
load("code/data/bw/weighted.Rdata")
conf1b <- loadConf(dat3, file = "code/conf/bw/wgwide.cfg")
par1b <- defpar(dat3,conf1b)
fit1b <- sam.fit(dat3,conf1b,par1b)
save(fit1b, file = "code/model/bw/case1b.Rdata")

#__Case2: Simple Taylor
conf2 <- loadConf(dat, file = "code/conf/bw/case2.cfg")
par2<- defpar(dat,conf2)
fit2 <- sam.fit(dat, conf2, par2)
save(fit2, file = "code/model/bw/case2.Rdata")


#__Case2b: Simple Taylor except catches
conf2b <- loadConf(dat,file = "code/conf/bw/case2b.cfg")
par2b<- defpar(dat,conf2b)
fit2b <- sam.fit(dat, conf2b, par2b)
save(fit2b, file = "code/model/bw/case2b.Rdata")

#___Case 3: Model with internal variance as external variance_____
load("code/data/bw/weighted_internal.Rdata")
conf3 <- loadConf(dat4, file = "code/conf/bw/wgwide.cfg")
conf3$keyVarObs[conf3$keyVarObs>=0] <- 0
par3 <- defpar(dat4,conf3)
fit3 <- sam.fit(dat4, conf3, par3)
save(fit3, file = "code/model/bw/case3.Rdata")

# __Case 4: BEST AIC__
conf4 <- loadConf(dat, file = "code/conf/bw/case4.cfg")
par4 <- defpar(dat,conf4)
fit4 <- sam.fit(dat, conf4, par4)
save(fit4, file = "code/model/bw/case4.Rdata")

