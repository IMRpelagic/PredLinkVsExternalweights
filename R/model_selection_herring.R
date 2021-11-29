load('code/data/herring/case2_data.Rda')
conf = loadConf(dat_case2,"code/conf/herring/model_case2.cfg")
par<-defpar(dat_case2,conf)
par$logSdLogN = c(-0.35, -5)
map = list(logSdLogN = as.factor(c(0,NA)))
fit_case2 <- sam.fit(dat_case2,conf,par=par,map=map)




load('code/data/herring/case2_data.Rda')

conf = loadConf(dat_case2,"code/conf/herring/model_case2.cfg")
conf$keyVarObs[1,] <- c(0,1,rep(2,9))
conf$keyVarObs[2,-1] <- c(3,3,rep(4,8))
conf$keyVarObs[3,1] <- 5
conf$keyVarObs[4,-1] <- c(6,6,rep(7,8))
conf$predVarObsLink[1,] <- 0
conf$predVarObsLink[2,-1] <- 1
conf$predVarObsLink[3,1] <- -1
conf$predVarObsLink[4,-1] <- 2
par<-defpar(dat_case2,conf)
par$logSdLogN = c(-0.35, -5)
map = list(logSdLogN = as.factor(c(0,NA)))
fit_case2b <- sam.fit(dat_case2,conf,par=par,map=map)
# save(fit_case2,file='../code/model/herring/fit_case2.Rda')
#

conf$keyVarObs[2,-1] <- c(3,3,rep(4,8))
conf$keyVarObs[3,1] <- 5
conf$keyVarObs[4,-1] <- c(6,7,rep(7,8))
par<-defpar(dat_case2,conf)
par$logSdLogN = c(-0.35, -5)
map = list(logSdLogN = as.factor(c(0,NA)))
fit_case2c <- sam.fit(dat_case2,conf,par=par,map=map)
AIC(fit_case2b, fit_case2c)

conf$predVarObsLink[1,] <- 0
conf$predVarObsLink[2,-1] <- 0
conf$predVarObsLink[3,1] <- -1
conf$predVarObsLink[4,-1] <- 1

par<-defpar(dat_case2,conf)
par$logSdLogN = c(-0.35, -5)
fit_case2ca <- sam.fit(dat_case2,conf,par=par,map=map)


conf$predVarObsLink[4,2] <- -1
par<-defpar(dat_case2,conf)
par$logSdLogN = c(-0.35, -5)
fit_case2cb <- sam.fit(dat_case2,conf,par=par,map=map)
saveConf(conf,"code/conf/herring/BEST_AIC_case2.cfg", overwrite = T)

ssbplot(c("ICES"= fit_case1, "Breivik et al"= fit_case2cb), addCI=T)
AIC(fit_case2b, fit_case2c, fit_case2cb)
ssbplot(c(fit_case2ca, fit_case2cb), addCI=T)

saveConf()

# ----------------------------------
rm(list=ls())
# ----------------------------------

load('code/data/herring/case1_data.Rda')
conf = loadConf(dat_case1,"code/conf/herring/model_case1.cfg")
par<-defpar(dat_case1,conf)
par$logSdLogN = c(-0.35, -5)
map = list(logSdLogN = as.factor(c(0,NA)))
fit1 <- sam.fit(dat_case1,conf,par=par,map=map)
# save(fit,file='code/model/herring/fit_case1.Rda')

load('code/data/herring/case2_data.Rda')
conf = loadConf(dat_case2,"code/conf/herring/BEST_AIC_case2.cfg")
par<-defpar(dat_case2,conf)
par$logSdLogN = c(-0.35, -5)
map = list(logSdLogN = as.factor(c(0,NA)))
fit2 <- sam.fit(dat_case2,conf,par=par,map=map)

source("R/general_plotting_function.R")

# plotting
load('stox_data/herring/stox_var_herring.Rda')
stox_var$mean[stox_var$fleet ==1] <- stox_var$mean[stox_var$fleet ==1]*1.7
stox_var$mean[stox_var$fleet ==3] <- stox_var$mean[stox_var$fleet ==3]/1e6
stox_var$mean[stox_var$fleet ==2] <- stox_var$mean[stox_var$fleet ==2]/1e6
stox_var$mean[stox_var$fleet ==4] <- stox_var$mean[stox_var$fleet ==4]/1e6

stox_var$v[stox_var$fleet ==1] <- stox_var$v[stox_var$fleet ==1]*(1.7)^2
stox_var$v[stox_var$fleet ==3] <- stox_var$v[stox_var$fleet ==3]/1e12
stox_var$v[stox_var$fleet %in%c(2,4)] <- stox_var$v[stox_var$fleet %in% c(2,4)]/(1e12)


stox_var <- stox_var%>% filter(!is.na(age)) %>% mutate(logm = log(mean), logv = log(v)) 
plot_curves(fit2, stox_var, fleetnames = c("Catches", "NORHERSS", "RI", "IESNS"), minage = 2, logobs =T) 
plot_curves(fit2, stox_var, fleetnames = c("Catches", "NORHERSS", "RI", "IESNS"), minage = 2, logobs =F) 

ggSAMplot(c("WGWIDE" = fit1, "PredVar" = fit2), whatToPlot = "SSB") + ggtitle("NSSH")
ggSAMplot(c("WGWIDE" = fit1, "PredVar" = fit2), whatToPlot = "Fbar") + ggtitle("NSSH")
ggSAMplot(c("WGWIDE" = fit1, "PredVar" = fit2), whatToPlot = "Recruitment") + ggtitle("NSSH")


ggSAMplot(c("WGWIDE" = fit, "PredVar" = fit2), whatToPlot = "SSB") + ggtitle("BW")
ggSAMplot(c("WGWIDE" = fit, "PredVar" = fit2), whatToPlot = "Fbar") + ggtitle("BW")
ggSAMplot(c("WGWIDE" = fit, "PredVar" = fit2), whatToPlot = "Recruitment") + ggtitle("BW")


IBWSSweights <- readxl::read_excel("stox_data/Bootstrap_BW.xlsx", 
                                   sheet = "IBWSS", 
                                   col_types = "numeric")
IBWSSweights <- IBWSSweights %>% filter(!is.na(age)) %>% 
  mutate(logm = log(mean),
         v = (cv*mean)^2,
         logv = 2*log(cv*mean),
         fleet = 2)
plot_curves(fit2, IBWSSweights, fleetnames = c("Catches", "IBWSS"), minage = 1, logobs =T) 
plot_curves(fit2, IBWSSweights, fleetnames = c("Catches", "IBWSS"), minage = 1, logobs =F) 
plot_curves(fit4, IBWSSweights, fleetnames = c("Catches", "IBWSS"), minage = 1, logobs =T) 
plot_curves(fit4, IBWSSweights, fleetnames = c("Catches", "IBWSS"), minage = 1, logobs =F) 
fit2$conf$keyVarObs
fit2$conf$predVarObsLink
