rm(list= ls())
# Blue whiting
library(stockassessment)
library(tidyverse)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# __Case 1: OFFICIAL WGWIDE ASSESSMENT__
load(file = "../code/data/bw/dat.Rdata")
conf <- loadConf(dat, file = "../code/conf/bw/wgwide.cfg")
par <- defpar(dat,conf)
fit <- sam.fit(dat,conf,par)
save(fit, file = "../code/model/bw/wgwide.Rdata")

#__Case2: Simple Taylor
conf2 <- loadConf(dat, file = "../code/conf/bw/case2.cfg")
par2<- defpar(dat,conf2)
fit2 <- sam.fit(dat, conf2, par2)
save(fit2, file = "../code/model/bw/predVarsimple.Rdata")

# __Case 3: BEST AIC__
# conf3 <- loadConf(dat, file = "../code/conf/bw/case4.cfg")
# par3  <- defpar(dat,conf3)
# fit3  <- sam.fit(dat, conf3, par3)
# save(fit3, file = "../code/model/bw/bestAIC.Rdata")

# - loading external variance estimates -
IBWSS <- readxl::read_excel("../stox_data/Bootstrap_BW.xlsx", 
                                   sheet = "IBWSS", 
                                   col_types = "numeric")
IBWSS <- IBWSS %>% filter(!is.na(age)) %>% 
  mutate(logm  = log(mean),
         v = (cv*mean)^2,
         logv  = 2*log(cv*mean),
         fleet = 2)
lm(logv~logm,data = IBWSS[IBWSS$fleet==2,])

source("../R/general_plotting_function.R")

# - plotting comparison external vs internal
plot_curves(fit2, IBWSS, fleetnames = c("Catch at age", "IBWSS"), minage = 1, logobs =T) + ggtitle("Blue Whiting")
ggsave("../plots/BW_logobs_vs_var.pdf", width = 16/2, height = 9/2)
plot_curves(fit2, IBWSS, fleetnames = c("Catch at age", "IBWSS"), minage = 1, logobs =F) + ggtitle("Blue Whiting")
ggsave("../plots/BW_logobs_vs_logvar.pdf", width = 16/2, height = 9/2)

# - SSB, Fbar and recruitment - 
ggSAMplot(c("WGWIDE"=fit,"Alternativ Assessment"=fit2)) + ggtitle("Blue Whiting") + theme(text = element_text(size = 16))
ggsave("../plots/BW_SSB_Fbar_rec.pdf", width = 9, height = 9)



res2 <- residuals(fit2)
res1 <- residuals(fit)
plotResidualComp(res2,'Blue Whiting',fleetnames = c("Catch at age", "IBWSS"))
ggsave("../plots/BWRES.pdf", width = 9, height = 9)


plotResidual(res1,res2,fleetnames = c("Catch at age", "IBWSS"))
ggsave("../plots/BWOSA.pdf", width = 9, height = 9,dpi = 300)



# 
# sim1 <- simstudy(fit,nsim = 10)
# sim2 <- simstudy(fit2,nsim = 10)
# 
# plot(sim1)
# plot(sim2)
# 
# jit1<-jit(fit)
# jit2<-jit(fit2)
