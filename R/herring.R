# clean slate
rm(list=ls())

# Herring
library(stockassessment)
library(tidyverse)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Official WGWIDE assessment: 
load('../code/data/herring/case1_data.Rda')
conf = loadConf(dat_case1,"../code/conf/herring/model_case1.cfg")
par<-defpar(dat_case1,conf)
par$logSdLogN = c(-0.35, -5)
map = list(logSdLogN = as.factor(c(0,NA)))
fit1 <- sam.fit(dat_case1, conf, par=par,map=map)
# save(fit,file='../code/model/herring/fit_case1.Rda')

# Best AIC we could find: 
load('../code/data/herring/case2_data.Rda')
conf = loadConf(dat_case2,"../code/conf/herring/BEST_AIC_case2.cfg")
conf$keyVarObs[1,] <- 0
conf$keyVarObs[2,-1] <- 1
conf$keyVarObs[3,1] <- 2
conf$keyVarObs[4,-1] <- 3
conf$predVarObsLink[1,] <- 0
conf$predVarObsLink[1,1] <- -1
conf$predVarObsLink[2,-1] <- 1
conf$predVarObsLink[3,1] <- -1
conf$predVarObsLink[4,-1] <- 2
par<-defpar(dat_case2,conf)
par$logSdLogN = c(-0.35, -5)
map = list(logSdLogN = as.factor(c(0,NA)))
fit2 <- sam.fit(dat_case2,conf,par=par,map=map)


AIC(fit1,fit2)


# ---------------------
source("../R/general_plotting_function.R")

# - loading external variance estimates -
load('../stox_data/herring/stox_var_herring.Rda')
stox_var$mean[stox_var$fleet ==1] <- stox_var$mean[stox_var$fleet ==1]*1.7
stox_var$mean[stox_var$fleet ==3] <- stox_var$mean[stox_var$fleet ==3]/1e6
stox_var$mean[stox_var$fleet ==2] <- stox_var$mean[stox_var$fleet ==2]/1e6
stox_var$mean[stox_var$fleet ==4] <- stox_var$mean[stox_var$fleet ==4]/1e6

stox_var$v[stox_var$fleet ==1] <- stox_var$v[stox_var$fleet ==1]*(1.7)^2
stox_var$v[stox_var$fleet ==3] <- stox_var$v[stox_var$fleet ==3]/(1e12)
stox_var$v[stox_var$fleet %in%c(2,4)] <- stox_var$v[stox_var$fleet %in% c(2,4)]/(1e12)

stox_var <- stox_var%>% filter(!is.na(age)) %>% mutate(logm = log(mean), logv = log(v)) 
lm(logv~logm,data = stox_var[stox_var$fleet==1,])
lm(logv~logm,data = stox_var[stox_var$fleet==2,])
lm(logv~logm,data = stox_var[stox_var$fleet==3,])
lm(logv~logm,data = stox_var[stox_var$fleet==4,])


# - plotting comparison external vs internal
plot_curves(fit2, stox_var, fleetnames = c("Catch at age", "NORHERSS", "RI", "IESNS"), minage = 2, logobs =F) +
  ggtitle("Norwegian Spring Spawning Herring")
ggsave("../plots/Herring_logobs_vs_logvar.pdf", width = 16/2, height = 7)
plot_curves(fit2, stox_var, fleetnames = c("Catch at age", "NORHERSS", "RI", "IESNS"), minage = 2, logobs =T) + 
  ggtitle("Norwegian Spring Spawning Herring")#+theme(legend.position = "top")
ggsave("../plots/Herring_logobs_vs_var.pdf", width = 16/2, height = 7)

# - SSB, Fbar and recruitment - 
ggSAMplot(c("WGWIDE"=fit1, "Alternative Assessment"=fit2)) + 
  ggtitle("Norwegian Spring Spawning Herring") + theme(text = element_text(size = 16))
ggsave("../plots/Herring_SSB_Fbar_rec.pdf", width = 9, height = 9)



res2 <- residuals(fit2)
res1 <- residuals(fit1)
plotResidualComp(res2,'Herring')
ggsave("../plots/HerringRES.pdf", width = 9, height = 9)


plotResidual(res1,res2)
ggsave("../plots/HerringOSA.pdf", width = 9, height = 9,dpi = 300)


# sim1 <- simstudy(fit1,nsim = 10)
# sim2 <- simstudy(fit2,nsim = 10)
# 
# plot(sim1)
# plot(sim2)
# 
# jit1<-jit(fit1)
# jit2<-jit(fit2)
