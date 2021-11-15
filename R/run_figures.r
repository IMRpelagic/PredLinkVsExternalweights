
#-------------------------------------------------------------------------------#
# Remove everything from memory to avoide conflicts between runs
#-------------------------------------------------------------------------------#
rm(list=ls())


#-------------------------------------------------------------------------------#
# Set the working directory to location of the script. 
#-------------------------------------------------------------------------------#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(ggplot2)

#-------------------------------------------------------------------------------#
#Figures
#-------------------------------------------------------------------------------#
plotVar <- function(stox_var,species = 'Herring'){
  
  one_to_one <-unique(stox_var[names(stox_var)%in%c('fleet','alpha','beta')])
  one_to_one[is.na(one_to_one)]<-0
  
  p<-ggplot(stox_var, aes(x = log(mean), y = log(v))) +
    geom_point(size=0.1)+
    geom_smooth(method = "lm",size=0.3) +
    theme(panel.grid = element_blank())+
    ggtitle(paste0("Comparing ", species," survey uncertainties to estimated relationship")) + 
    theme_classic()
  for(i in seq(-30,30,by=5)){
    one2one<-one_to_one
    one2one$alpha<-log(one2one$alpha)+i
    p<-p+geom_abline(data = one2one,aes(intercept = (alpha),slope=beta),colour='grey',size=0.1)
  }
  
  p<-p+geom_abline(data = one_to_one,
                   aes(intercept = log(alpha), slope= beta),
                   lwd = 0.2, col = 2,size=0.1)
  
  
  p<-p+theme(axis.title=element_text(size=6),axis.text =element_text(size=6))
  p<-p + theme(legend.text =element_text(size=6),legend.title = element_text(size=6))
  p<-p + theme(title = element_text(size=6))
  p<-p + theme(strip.text = element_text(size=6))
  p<-p+facet_wrap(~fleet, ncol=2)
  show(p)
  ggsave(paste0("../plots/",species,"_logobs-vs-logvar.png"),
         width=85,height=65,units='mm',dpi=1200)
}

load('../stox_data/herring/stox_var_herring_output.Rda')
load('../code/model/herring/fit_case1.Rda')
load('../code/model/herring/fit_case2.Rda')
load('../code/model/herring/fit_case3.Rda')

plotVar(stox_var,species='Herring')
ssbplot(c('Case 1'=fit_case1,'Case 2'=fit_case2,'Case 3'=fit_case3))