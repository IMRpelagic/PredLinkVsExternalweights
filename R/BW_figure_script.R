
# __ standard plots __
par(mfrow=c(3,1))
ssbplot( c("WGWIDE"= fit,"withpredlink"=fit2), addCI=T)
ssbplot( c("WGWIDE"= fit,"withweights"=fit3), addCI=T)
ssbplot( c("WGWIDE"= fit,"predlinkasweights"=fit4), addCI=T)

fbarplot( c("WGWIDE"= fit,"withpredlink"=fit2), addCI=T)
fbarplot( c("WGWIDE"= fit,"withweights"=fit3), addCI=T)
fbarplot( c("WGWIDE"= fit,"predlinkasweights"=fit4), addCI=T)

recplot( c("WGWIDE"= fit,"withpredlink"=fit2), addCI=T)
recplot( c("WGWIDE"= fit,"withweights"=fit3), addCI=T)
recplot( c("WGWIDE"= fit,"predlinkasweights"=fit4), addCI=T)


# __ OSA residuals __
res <- residuals(fit)
res2 <- residuals(fit2)
res3 <- residuals(fit3)
res4 <- residuals(fit4)

# __bubble plots__
plot(res,  type = "bubble") # WGWIDE
plot(res2, type = "bubble") # with predlink
plot(res3, type = "bubble") # with XSAM weights
plot(res3, type = "bubble") # with predlink as weights

#__qqplots__
plot(res,  type = "summary") # WGWIDE
plot(res2, type = "summary") # with predlink
plot(res3, type = "summary") # with XSAM weights
plot(res3, type = "summary") # with predlink as weights
# __jitrun__
# jj <- jit(fit2, nojit = 50)
# jj

#_______closer look at fit2______________
N = 20000
ss = rmvnorm(N,mu = fit2$opt$par,Sigma = fit2$sdrep$cov.fixed)
alpha = exp(ss[,which(names(fit2$opt$par)=="logSdLogObs")])
beta = exp(ss[,which(names(fit2$opt$par)=="predVarObs")])

plotLink = function(fitSandard,fitLink,f,a,col = 1,main = "",ylim = c(0,3)){
  
  aaConstant = fitSandard$conf$keyVarObs[f,a]+1
  
  aaa = fitLink$conf$keyVarObs[f,a]+1
  aab = fitLink$conf$predVarObs[f,a]+1
  
  age = which(fitSandard$conf$keyVarObs[f,]==aaConstant-1) + fitSandard$conf$minAge -1
  obsIndex = which(fitLink$data$aux[,2]==f & fitLink$data$aux[,3] %in% age)
  
  pred = fitLink$rep$predObs
  pred = pred[obsIndex]
  obs = fitLink$data$logobs[obsIndex]
  obs = obs[!is.na(obs)]
  
  l = seq(min(pred),max(pred), by = 0.01)
  
  sd = matrix(0,N,length(l))
  for(i in 1:N){
    if(aab!=0){
      sd[i,] = sqrt(log(alpha[i,aaa]* exp(l*(beta[i,aab]-1)) +1))
    }else{
      sd[i,] = alpha[i,aaa]
    }
  }
  
  sdM = rep(0,length(l))
  sdL = rep(0,length(l))
  sdU = rep(0,length(l))
  for(i in 1:length(l)){
    sdM[i] = mean(sd[,i])
    sdL[i] = quantile(sd[,i], 0.025)
    sdU[i] = quantile(sd[,i], 0.975)
  }
  
  if(length(age)==1){
    plot(l,sdM,type = 'l', ylim = ylim,lwd = 2,col = col,main = paste0(main, " age ",age),
         ylab = "", xlab = "",cex.main = 2.5,
         cex.lab = 1.5,
         cex.axis = 1.5)
  }else{
    plot(l,sdM,type = 'l', ylim = ylim,lwd = 2,col = col,main = paste0(main, " age ",min(age),"-",max(age)),
         ylab = "", xlab = "",cex.main = 2.5,
         cex.lab = 2,
         cex.axis = 1.5)
  }
  mtext("Standard deviation", cex = 2,side = 2, line=2.7,)
  mtext("Log-prediction", cex = 2,side = 1, line=2.7,)
  lines(l,sdL,col = col)
  lines(l,sdU,col = col)
  abline(h = exp(fitSandard$pl$logSdLogObs[aaConstant]), col = col+1)
  abline(h = exp(fitSandard$pl$logSdLogObs[aaConstant] + 1.96*fitSandard$plsd$logSdLogObs[aaConstant]),col = col+1,lt = 2)
  abline(h = exp(fitSandard$pl$logSdLogObs[aaConstant] - 1.96*fitSandard$plsd$logSdLogObs[aaConstant]),col = col+1,lt = 2)
  
  points(pred,rep(exp(fitSandard$pl$logSdLogObs[aaConstant]),length(pred)))
}
ylim = c(0,1)
pdf("plots/CATCHES.pdf", width = 1920, height = 1200)
par(mfrow=c(2,2))
plotLink(fitSandard = fit,fitLink = fit2,f = 1,a = 1,main = "Blue Whiting Catch sd",ylim = ylim)
plotLink(fitSandard = fit,fitLink = fit2,f = 1,a = 2,main = "Blue Whiting Catch sd",ylim = ylim)
plotLink(fitSandard = fit,fitLink = fit2,f = 1,a = 3,main = "Blue Whiting Catch sd",ylim = ylim)
plotLink(fitSandard = fit,fitLink = fit2,f = 1,a = 9,main = "Blue Whiting Catch sd",ylim = ylim)
dev.off()

pdf("plots/SURVEYS.pdf", width = 1920, height = 1200)
par(mfrow=c(2,2))
plotLink(fitSandard = fit,fitLink = fit2,f = 2,a = 1,main = "Blue Whiting Survey sd",ylim = ylim)
plotLink(fitSandard = fit,fitLink = fit2,f = 2,a = 2,main = "Blue Whiting Survey sd",ylim = ylim)
plotLink(fitSandard = fit,fitLink = fit2,f = 2,a = 3,main = "Blue Whiting Survey sd",ylim = ylim)
plotLink(fitSandard = fit,fitLink = fit2,f = 2,a = 4,main = "Blue Whiting Survey sd",ylim = ylim)
dev.off()


# ----------------------------------------------------------
# -- Comparing estimates of variance internal vs external --
# -- for the IBWSS survey                                 --
# ----------------------------------------------------------
load(file = "data/stox_estimates_BW.Rdata")
library(tidyverse)
ptab <- partable(fit)
ptab2 <- partable(fit2)
ptab3 <- partable(fit3)

a <- b <-sig <-sig3 <-  matrix(NA_real_, ncol = ncol(conf$keyVarObs), 
                               nrow = nrow(conf$keyVarObs))


sd1 <- ptab [str_detect(rownames(ptab),  pattern = "logSdLogObs"),"exp(par)"]
a2  <- ptab2[str_detect(rownames(ptab2), pattern = "logSdLogObs"),"exp(par)"]
b2  <- ptab2[str_detect(rownames(ptab2), pattern = "predVarObs"), "exp(par)"]
sd3 <- ptab3[str_detect(rownames(ptab3), pattern = "logSdLogObs"),"exp(par)"]

for(j in 1:ncol(a)){
  for(i in 1:nrow(a)){
    a[i,j] <- ifelse(fit5$conf$keyVarObs[i,j]!=-1, a2[fit5$conf$keyVarObs[i,j]+1],NA)
    b[i,j] <- ifelse(fit5$conf$predVarObsLink[i,j]!=-1, b2[fit5$conf$predVarObsLink[i,j]+1]+1,NA)
    sig[i,j] <- ifelse(fit$conf$keyVarObs[i,j]!=-1, sd1[fit$conf$keyVarObs[i,j]+1],NA)
    sig3[i,j] <- ifelse(fit3$conf$keyVarObs[i,j]!=-1, sd3[fit3$conf$keyVarObs[i,j]+1],NA)
  }
}

df <- data.frame(
  a = a[2,1],
  b = b[2,1]
  #  agefac = c("1","2","3-8")
)
IBWSS <- mutate(IBWSS, agefac = ifelse(age%in%3:8,"3-8",ifelse(age ==1,1,ifelse(age==2,2,">8"))))
IBWSS$predv <- log(df$a)+df$b*IBWSS$logobs
ggplot(IBWSS, aes(x = logobs, y = logv)) +
  geom_point()+#aes(col = agefac))+
  geom_smooth(method = "lm") +
  theme(panel.grid = element_blank())+
  geom_abline(data=df, aes(intercept = log(a)/2, slope= b),# col = agefac),
              lwd = 1.2, col = 2)+
  ggtitle("Comparing IBWSS survey uncertainty to estimated relationship") + theme_bw() +
  geom_text(x = -Inf, y = Inf, label = expression("y="~log(alpha)/2+beta~"* logobs"), hjust = -.05, vjust = 1.2)
ggsave("plots/logobs-vs-logvar_a2.pdf", width = 1920, height = 1200, unit = "px")
ggplot(IBWSS, aes(x = logobs, y = logv)) +
  geom_point()+#aes(col = agefac))+
  geom_smooth(method = "lm") +
  theme(panel.grid = element_blank())+
  geom_abline(data=df, aes(intercept = log(a), slope= b),# col = agefac),
              lwd = 1.2, col = 2)+
  ggtitle("Comparing IBWSS survey uncertainty to estimated relationship") + theme_bw() +
  geom_text(x = -Inf, y = Inf, label = expression("y="~log(alpha)+beta~"* logobs"), hjust = -.05, vjust = 1.2)
ggsave("plots/logobs-vs-logvar.pdf", width = 1920, height = 1200, unit = "px")


ggplot(IBWSS, aes(x = logv, y = predv)) +
  geom_point()+#aes(col = agefac))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  geom_abline(intercept = seq(-20,20,2)[-11], slope = 1, col = "grey80", lwd = .5, lty =2)+
  scale_x_continuous(limits =c(0,16), name = "Bootstrapped variance from Stox", breaks = seq(0,20,5))+
  scale_y_continuous(limits =c(0,16), name = "Internally predicted variance from Sam", breaks = seq(0,20,5))+
  geom_abline(intercept = 0, slope = 1, col = 2, lty = 2, lwd = 1)
ggsave("plots/logvar-vs-predvar.pdf", width = 1920, height = 1200, unit = "px")

IBWSS$predv <- log(df$a)/2+df$b*IBWSS$logobs
ggplot(IBWSS, aes(x = logv, y = predv)) +
  geom_point()+#aes(col = agefac))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  geom_abline(intercept = seq(-20,20,2)[-11], slope = 1, col = "grey80", lwd = .5, lty =2)+
  scale_x_continuous(limits =c(0,16), name = "Bootstrapped variance from Stox", breaks = seq(0,20,5))+
  scale_y_continuous(limits =c(0,16), name = "Internally predicted variance from Sam", breaks = seq(0,20,5))+
  geom_abline(intercept = 0, slope = 1, col = 2, lty = 2, lwd = 1)
ggsave("plots/logvar-vs-predvar_a2.pdf", width = 1920, height = 1200, unit = "px")

cmp<-rbind(c(log(df$a),df$b),lm(IBWSS$logv ~ IBWSS$logobs)$coef)
rownames(cmp) <- c("SAM", "StoX")
colnames(cmp) <- c("log a", "b")
cmp
