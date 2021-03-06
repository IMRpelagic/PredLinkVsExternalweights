# Function for comparing model fits with predVarObs to stox variances: 

plot_curves <- function(fit, stox, fleetnames=NULL, minage = 1,logobs = FALSE){
  aconf <- fit$conf$keyVarObs
  bconf <- fit$conf$predVarObs
  a <- aconf*NA
  b <- bconf*NA
  aest <-   exp(fit$opt$par[which(names(fit$opt$par)=="logSdLogObs")])
  best <-   exp(fit$opt$par[which(names(fit$opt$par)=="predVarObs")])+1
  for(i in 1:nrow(a)){
    for(j in 1:ncol(a)){
      if(aconf[i,j]!=-1 & !is.na(aconf[i,j]))
        a[i,j] <- ifelse(bconf[i,j]!=-1, log(aest[aconf[i,j]+1]),log(exp(aest[aconf[i,j]+1]^2)-1))
      if(!is.na(bconf[i,j]))
        b[i,j] <- ifelse(bconf[i,j]!=-1, best[bconf[i,j]+1], 2)
    }
  }
  rownames(a) <-rownames(b) <-  1:nrow(a)
  colnames(a) <-colnames(b) <-  1:ncol(a)
  df <- left_join(reshape2::melt(a) %>% rename("fleet" = Var1, age = Var2, alpha = value) ,
                  reshape2::melt(b) %>% rename("fleet" = Var1, age = Var2, beta = value) %>% mutate(beta = beta),
                  by = c("fleet","age")) %>% mutate(age = minage-1+age) %>%
    filter(!is.na(beta)) 
  df <- df %>% group_by(fleet,alpha,beta) %>% 
    summarize(agegroup = ifelse(min(age)==max(age),
                                paste0(min(age)),
                                ifelse(max(age)== max(df$age) & fit$conf$maxAgePlusGroup== 1, 
                                       paste0(min(age),"-",max(age), "+"),
                                paste0(min(age),"-",max(age)))))
  if(!is.null(fleetnames)){
    stox$fleet <- factor(fleetnames[as.integer(stox$fleet)], levels = fleetnames)
    df$fleet <- factor(fleetnames[df$fleet], levels = fleetnames)
  }
  stox <- filter(stox, !is.na(age))
  if(!logobs){
    dt <- data.frame(ID = seq(-100,100,by=5),a = seq(-100,100,by=5),b=2)
    ggplot(stox, aes(x = logm, y = logv)) + facet_wrap( ~fleet) + geom_point()+
      geom_smooth(method = "lm", se = FALSE)+
      geom_abline(data = df, aes(intercept = alpha, slope= beta, col = factor(agegroup)))+theme_bw() +
      scale_x_continuous(name = "Log Mean") + scale_y_continuous("Log variance")+
      scale_color_discrete("Ages") +
      theme(strip.background = element_rect(fill = "transparent", color = "transparent"))+
      geom_abline(data = dt,aes(intercept=a,slope=b,group=ID),colour='grey')+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  }else{
    xx <- stox %>% group_by(fleet) %>% summarize(minlogm = min(logm), maxlogm = max(logm))
    xx2 <- data.frame()
    for(i in xx$fleet){
      tmp <- cbind(fleet = i, xx = seq(xx$minlogm[xx$fleet == i ], xx$maxlogm[xx$fleet == i ], length.out = 1000))
      xx2 <- rbind(xx2,tmp)
    }
    df2 <- left_join(xx2 %>% mutate(fleet = factor(fleet, levels = fleetnames)), df,by = "fleet")
    df2 <- df2 %>% rename("logm" = xx) %>% mutate(logm = as.numeric(logm))
    df2 <- df2 %>% mutate(vlog = ifelse(beta ==1, exp(alpha),log(exp(alpha)*exp(logm*(beta-2))+1)))
    stox <- mutate(stox,vlog = log(v/mean^2+1))
    return(
      ggplot(df2, aes(x = logm, y = vlog))+ geom_line(aes(col = factor(agegroup)))   + facet_wrap( ~fleet, scales = "free")+
        geom_point(data = stox, aes(x = logm, y = vlog)) + 
        #geom_smooth(method = "lm")+
        #geom_abline(data = df, aes(intercept = alpha, slope= beta, col = factor(agegroup)))+
        theme_bw() +
        scale_x_continuous(name = "Log Mean") + scale_y_continuous("Variance of log-observations")+
        scale_color_discrete("Ages") +
        #geom_line(data =df2, aes(x = logm, y = vlog, col = factor(agegroup)))+ theme_bw() +
        theme(strip.background = element_rect(fill = "transparent", color = "transparent"))
    )
  }
}


# Function for plott SAM (ssb, rec, fbar) with ggplot:
ggSAMplot <- function(fit, whatToPlot = c("SSB", "Fbar", "Recruitment")){
  if(class(fit)=="sam"){
    df <- as.data.frame(summary(fit))
    names(df) <- c("Rec", "RecLow", "RecHigh", "SSB", "SSBlow", "SSBhigh", "Fbar", "Fbarlow", "Fbarhigh")
    df <- df %>% rownames_to_column("year") %>% mutate(year = as.numeric(year))
    d1 <- cbind(df[,1:4], what = "Recruitment")
    d2 <- cbind(df[,c(1,5:7)],what = "SSB")
    d3 <- cbind(df[,c(1,8:10)],what = "Fbar")
    names(d1) <-names(d2) <-names(d3) <-c("year", "mean", "low","high", "what")
    df <- as.data.frame(rbind(d1,d2,d3))
    
    df <- filter(df, what %in% whatToPlot)
    df <- transform(df, what = factor(what, levels = c("SSB", "Fbar", "Recruitment")))
    return(
      ggplot(df, aes(x = year, y = mean, ymin = low, ymax= high)) +
        geom_ribbon(alpha =.2, fill = "darkblue") +
        geom_line(lwd = 1.2, col = "darkblue")+
        facet_wrap( ~what, scales = "free", ncol = 1, strip.position = "left")+
        theme_bw() +theme(legend.position = "none",
                          strip.background = element_rect(fill = "transparent", color = "transparent"), 
                          strip.placement = "outside",
                          axis.title = element_blank())
    )
  } else if(class(fit)=="samset"){
    df2 <- rbind()
    for(i in 1:length(fit)){
      df <- as.data.frame(summary(fit[[i]]))
      names(df) <- c("Rec", "RecLow", "RecHigh", "SSB", "SSBlow", "SSBhigh", "Fbar", "Fbarlow", "Fbarhigh")
      df <- df %>% rownames_to_column("year") %>% mutate(year = as.numeric(year))
      d1 <- cbind(df[,1:4], what = "Recruitment")
      d2 <- cbind(df[,c(1,5:7)],what = "SSB")
      d3 <- cbind(df[,c(1,8:10)],what = "Fbar")
      names(d1) <-names(d2) <-names(d3) <-c("year", "mean", "low","high", "what")
      df <- as.data.frame(rbind(d1,d2,d3))
      df <- filter(df, what %in% whatToPlot)
      df$model <- ifelse(!is.null(names(fit)), names(fit)[i],i)
      df2 <- rbind(df2,df)
    }
    df <- transform(df2, what = factor(what, levels = c("SSB", "Fbar", "Recruitment")))
    return(
      ggplot(df, aes(x = year, y = mean, ymin = low, ymax= high, col = factor(model), fill = factor(model), group = model)) +
        geom_ribbon(alpha =.2) +
        geom_line(lwd = 1.2)+
        facet_wrap( ~what, scales = "free", ncol = 1, strip.position = "left")+
        theme_bw() +theme(legend.position = "top",
                          strip.background = element_rect(fill = "transparent", color = "transparent"), 
                          strip.placement = "outside",
                          axis.title = element_blank(),
                          legend.title = element_blank())
    )
  }
}



plotResidualComp <-function(res,title = '',fleetnames = c("Catch at age", "NORHERSS", "RI", "IESNS")){
  
  class(res)<-'data.frame'
  
  if(!is.null(fleetnames)){
    res$fleet <- factor(fleetnames[res$fleet], levels = fleetnames)
  }
  # res$fleet<-as.factor(res$fleet)
  ggplot(res,aes(y=observation,x=observation-residual,colour=fleet)) +
    geom_point()+xlab('Prediction')+ylab("Observation") +geom_abline(intercept = 0, slope = 1) +
    ggtitle(title) +
    theme_bw() + theme(text = element_text(size = 20),
                       legend.title = element_blank(),
                       legend.position = "top")  
}


plotResidual <- function(res1,res2,fleetnames = c("Catch at age", "NORHERSS", "RI", "IESNS")){
  
  
  resa <- res1
  resb <- res2
  resa$model <- 'WGWIDE'
  resb$model <- 'Alternative'
  
  class(resa)<-'data.frame'
  class(resb)<-'data.frame'
  
  res<-rbind(resa,resb)
  res$colour<-'positive'
  res$colour[res$residual<0]<-'negative'
  res<-res[!is.na(res$observation),]
  
  if(!is.null(fleetnames)){
    res$fleet <- factor(fleetnames[res$fleet], levels = fleetnames)
  }
  
  
  res <- transform(res, model = factor(model, levels = c("WGWIDE", "Alternative")))
  res <- transform(res, colour = factor(colour, levels = c("positive", "negative")))
  
  ggplot(res,aes(x=year,y=age,size=abs(residual),colour=colour))+
    facet_grid(fleet~model)+geom_point(alpha=0.5)+theme_bw() + 
    theme(strip.background = element_rect(fill = "transparent", color = "transparent"),panel.grid = element_blank())+
    scale_x_continuous("Year") + scale_y_continuous("Age", breaks = 1:12)+
    scale_size_continuous("Absolute value",range=c(0,8)) +
    scale_color_discrete(name = "Sign")
  
}

