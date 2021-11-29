rm(list=ls())
# Herring
library(stockassessment)
library(tidyverse)

# Official WGWIDE assessment: 
load('code/data/herring/case1_data.Rda')
conf = loadConf(dat_case1,"code/conf/herring/model_case1.cfg")
par<-defpar(dat_case1,conf)
par$logSdLogN = c(-0.35, -5)
map = list(logSdLogN = as.factor(c(0,NA)))
fit1 <- sam.fit(dat_case1, conf, par=par,map=map)
# save(fit,file='code/model/herring/fit_case1.Rda')

# Best AIC we could find: 
load('code/data/herring/case2_data.Rda')
conf = loadConf(dat_case2,"code/conf/herring/BEST_AIC_case2.cfg")
conf$keyVarObs[1,] <- 0
conf$keyVarObs[2,-1] <- 1
conf$keyVarObs[3,1] <- 2
conf$keyVarObs[4,-1] <- 3
conf$predVarObsLink[1,] <- 0
conf$predVarObsLink[2,-1] <- 1
conf$predVarObsLink[3,1] <- -1
conf$predVarObsLink[4,-1] <- 2
par<-defpar(dat_case2,conf)
par$logSdLogN = c(-0.35, -5)
map = list(logSdLogN = as.factor(c(0,NA)))
fit2 <- sam.fit(dat_case2,conf,par=par,map=map)

recplot(c(fit1,fit2), addCI=T)

conf = loadConf(dat_case2,"code/conf/herring/BEST_AIC_case2.cfg")
par<-defpar(dat_case2,conf)
par$logSdLogN = c(-0.35, -5)
map = list(logSdLogN = as.factor(c(0,NA)))

#remotes::install_github("kaskr/adcomp/TMB", ref = "TMB_experimental") 

t1 <- Sys.time()
fit3 <- sam.fit(dat_case2,conf,par=par,map=map)
(t2 <- Sys.time() -t1)
AIC(fit1,fit2,fit3)

lo <- leaveout(fit3)

# ---------------------
source("R/general_plotting_function.R")

# - loading external variance estimates -
load('stox_data/herring/stox_var_herring.Rda')
stox_var$mean[stox_var$fleet ==1] <- stox_var$mean[stox_var$fleet ==1]*1.7
stox_var$mean[stox_var$fleet ==3] <- stox_var$mean[stox_var$fleet ==3]/1e6
stox_var$mean[stox_var$fleet ==2] <- stox_var$mean[stox_var$fleet ==2]/1e6
stox_var$mean[stox_var$fleet ==4] <- stox_var$mean[stox_var$fleet ==4]/1e6

stox_var$v[stox_var$fleet ==1] <- stox_var$v[stox_var$fleet ==1]*(1.7)^2
stox_var$v[stox_var$fleet ==3] <- stox_var$v[stox_var$fleet ==3]/(1e12)
stox_var$v[stox_var$fleet %in%c(2,4)] <- stox_var$v[stox_var$fleet %in% c(2,4)]/(1e12)

stox_var <- stox_var%>% filter(!is.na(age)) %>% mutate(logm = log(mean), logv = log(v)) 

# - plotting comparison external vs internal
plot_curves(fit3, stox_var, fleetnames = c("Catches", "NORHERSS", "RI", "IESNS"), minage = 2, logobs =F) +
  ggtitle("Norwegian Spring Spawning Herring")
ggsave("plots/Herring_logobs_vs_logvar.pdf", width = 16/2, height = 7)
plot_curves(fit2, stox_var, fleetnames = c("Catches", "NORHERSS", "RI", "IESNS"), minage = 2, logobs =T) + 
  ggtitle("Norwegian Spring Spawning Herring")#+theme(legend.position = "top")
ggsave("plots/Herring_logobs_vs_var.pdf", width = 16/2, height = 7)

# - SSB, Fbar and recruitment - 
ggSAMplot(c("XSAM"=fit1, "Simple predVar"=fit2, "Best AIC"= fit3)) + 
  ggtitle("Norwegian Spring Spawning Herring")
ggsave("plots/Herring_SSB_Fbar_rec.pdf", width = 9, height = 9)








