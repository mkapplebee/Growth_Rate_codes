#Generates TukeyHSD for measurment columns from gcfile objects (output of gcfall-media-lnmerg.R), for every column between strt_col/end_col
# collects the measure, the strains compared, and the calculated pval


tukeyrun<-function(gcobj,range) {

 ptab<-NULL
  
 for (i in range) { #11:20 corresponds to measurement columns, 11:15 for spline, 16:20 for .bt, 21:25 for stdev, 46:55 for daynorm data
   
   #lmtest<-lm(gcobj[,i]~gcobj$plate+gcobj$strain)
   aovtest<-aov(gcobj[,i]~gcobj$plate+gcobj$strain)
   
   tab<-model.tables(aovtest,"means")$tables$'gcobj$strain'
   
   #for all 6 strains
   #tab2<-c(rep(tab[1],5),rep(tab[2],4),rep(tab[3],3),rep(tab[4],2),tab[5])
   
   #for 4 strains
   #tab2<-c(rep(tab[1],times=3),tab[2],tab[2],tab[3])
   
   #for 3 strains
   tab2<-c(tab[1],tab[1],tab[2])
   
   tuk<-TukeyHSD(aovtest)
   
   difs<-tuk[[2]][,1]
   perc_dif<-signif(difs/tab2,digits=3)
   pvals<-tuk[[2]][,4]
   strains<-row.names(tuk[[2]])
   meas<-rep(colnames(gcobj)[i],times=length(pvals))
   
   x<-cbind.data.frame(meas,strains,difs,perc_dif,pvals)
   
   ptab<-rbind.data.frame(ptab,x)
   
 }
#ptab<-as.data.frame(ptab)

ptab[,4]<-signif(ptab[,4],digits=3)
ptab[,3]<-signif(ptab[,3],digits=4)

p05<-ifelse(ptab$pvals<0.05,"*"," ")
ptab<-cbind(ptab,p05)

#ptab2<-ptab[which(ptab[,3]<0.05),]
return(ptab)

}