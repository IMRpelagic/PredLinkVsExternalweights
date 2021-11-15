# Install the SAM stockassessment model
# devtools::install_github("fishfollower/SAM/stockassessment", INSTALL_opts=c("--no-multiarch"))


#-------------------------------------------------------------------------------#
# Remove everything from memory to avoide conflicts between runs
#-------------------------------------------------------------------------------#
rm(list=ls())


#-------------------------------------------------------------------------------#
# Set the working directory to location of the script. 
#-------------------------------------------------------------------------------#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


library(stockassessment)




#-------------------------------------------------------------------------------#
#Run SAM for case 1.
#-------------------------------------------------------------------------------#
load('../code/data/herring/case1_data.Rda')
conf = loadConf(dat_case1,"../code/conf/herring/model_case1.cfg")
par<-defpar(dat_case1,conf)
par$logSdLogN = c(-0.35, -5)
map = list(logSdLogN = as.factor(c(0,NA)))
fit_case1 <- sam.fit(dat_case1,conf,par=par,map=map)
save(fit_case1,file='../code/model/herring/fit_case1.Rda')


#-------------------------------------------------------------------------------#
#run SAM for case2
#-------------------------------------------------------------------------------#
load('../code/data/herring/case2_data.Rda')
conf = loadConf(dat_case2,"../code/conf/herring/model_case2.cfg")
par<-defpar(dat_case2,conf)
par$logSdLogN = c(-0.35, -5)
map = list(logSdLogN = as.factor(c(0,NA)))
fit_case2 <- sam.fit(dat_case2,conf,par=par,map=map)
save(fit_case2,file='../code/model/herring/fit_case2.Rda')



load('../stox_data/herring/stox_var_herring.Rda')
#-------------------------------------------------------------------------------#
#Grab alpha/beta parameter from fit
#-------------------------------------------------------------------------------#
logSdLogObs <- exp(as.numeric(fit_case2$sdrep$par.fixed[names(fit_case2$sdrep$par.fixed)=='logSdLogObs']))
preVarObs <-exp(fit_case2$sdrep$par.fixed[names(fit_case2$sdrep$par.fixed)=='predVarObs'])

out <- c()
for (fleet in seq(1,4,by = 1)){
  alpha <- (logSdLogObs[max(conf$keyVarObs[fleet,],na.rm = T)+1])
  beta <- as.numeric(preVarObs[max(conf$predVarObsLink[fleet,],na.rm = T)+1])+1
  out <- rbind(out,c(fleet,alpha,beta))
}
out<-as.data.frame(out)
names(out)<-c('fleet','alpha','beta')
stox_var<-merge(stox_var,out)
stox_var$logv2=log(stox_var$alpha) + stox_var$beta * log(stox_var$mean)
stox_var$logv1 <- log(stox_var$v)

save(stox_var,file = '../stox_data/herring/stox_var_herring_output.Rda')






#-------------------------------------------------------------------------------#
#Grab alpha/beta parameter from fit

cn<-read.ices("../data/herring_assessment_data/caa.dat")
cw<-read.ices("../data/herring_assessment_data/cw.dat")
dw<-read.ices("../data/herring_assessment_data/dw.dat")
lf<-read.ices("../data/herring_assessment_data/lf.dat")
lw<-read.ices("../data/herring_assessment_data/lw.dat")
mo<-read.ices("../data/herring_assessment_data/mo.dat")
nm<-read.ices("../data/herring_assessment_data/nm.dat")
pf<-read.ices("../data/herring_assessment_data/pf.dat")
pm<-read.ices("../data/herring_assessment_data/pm.dat")
sw<-read.ices("../data/herring_assessment_data/sw.dat")
surveys<-read.ices("../data/herring_assessment_data/survey.dat")

alpha <- unique(stox_var[stox_var$fleet==1,]$alpha)
beta <- unique(stox_var[stox_var$fleet==1,]$beta)
varC<-alpha + beta * log(cn*1000)
varC[cn<=0]<-1
attributes(cn)$weight = 1/(varC)

alpha <- unique(stox_var[stox_var$fleet==2,]$alpha)
beta <- unique(stox_var[stox_var$fleet==2,]$beta)
varS1<-alpha + beta * log(surveys[[1]]*1000)
varS1[is.na(surveys[[1]])]<-1
attributes(surveys[[1]])$weight = 1/(varS1)


alpha <- unique(stox_var[stox_var$fleet==3,]$alpha)
beta <- unique(stox_var[stox_var$fleet==3,]$beta)
varS1<-t(alpha + beta * log(surveys[[2]]))
varS1[is.na(varS1)]<-1
attributes(surveys[[2]])$weight = 1/(varS1)


alpha <- unique(stox_var[stox_var$fleet==4,]$alpha)
beta <- unique(stox_var[stox_var$fleet==4,]$beta)
varS1<-alpha + beta * log(surveys[[3]]*1000)
varS1[is.na(varS1)]<-1
attributes(surveys[[3]])$weight = 1/(varS1)


#-------------------------------------------------------------------------------#
#Prepare the SAM data object
#-------------------------------------------------------------------------------#
dat_case3<-setup.sam.data(surveys=surveys,
                    residual.fleet = cn,
                    prop.mature = mo,
                    stock.mean.weight = sw,
                    catch.mean.weight = cw,
                    dis.mean.weight = dw,
                    land.mean.weight = lw,
                    prop.f=pf,
                    prop.m = pm,
                    natural.mortality = nm,
                    land.frac = lf)

save(dat_case3,file = '../code/data/herring/dat_case3.Rda')

conf = loadConf(dat_case1,"../code/conf/herring/model_case1.cfg")
conf$fixVarToWeight<-1
par<-defpar(dat_case3,conf)
par$logSdLogN = c(-0.35, -5)
map = list(logSdLogN = as.factor(c(0,NA)))
fit_case3 <- sam.fit(dat_case3,conf,par=par,map=map)
save(fit_case3,file = '../code/model/herring/fit_case3.Rda')
