#from page 512 of the R Book
# yv = bar hieghts
# z = error size
# nn = names of bars

#ybar <- tapply(m_norm, factors,mean)
#labels <- levels(factor-vector)
#sdev<-tapply(m_norm, factors, sd)
#create standard error function -
#se<-function(x) sd(x)/sqrt(length(x))
#then
#serror<-tapply(m_norm,factors,se)

errort.bars<-function(gcobj) {
  
  #generates barplot of the measure columns (11:14) from gcfile output data
  
  #calls tplot, written by http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode
  
  #windows(60,50)
  par(mfrow=c(2,2))
  
  gcobj$strain<-droplevels(gcobj$strain)
  #gcobj$strain<-factor(gcobj$strain,levels=c("AM373","r_McpA","McpA","LiaS","L_rM","LM"),ordered=TRUE)
  
  gcobj<-gcobj[order(gcobj$strain,gcobj$plate),]
  
  source('~/Bacillus_subtilis/Growth Rates/growth-curves-ryan/tplot.R')
 
  for (i in 11:14) {
    
    tplot( gcobj[,i] ~ gcobj$strain, data=gcobj, las=1, cex=1, cex.axis=1, bty='L', show.n=F, dist=.25, jit=.05, type=rep('db',length(levels(gcobj$strain))),
         group.pch=T, pch= 6, 
        col=c(1:length(levels(gcobj$plate)))[gcobj$plate], boxcol=c('lightsteelblue1'), boxborder=grey(.8),boxplot.pars=list(notch=T, boxwex=.5),main=colnames(gcobj)[i])
  }
  

 }

