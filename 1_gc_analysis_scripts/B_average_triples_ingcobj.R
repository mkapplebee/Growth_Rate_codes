avg_triples<-function(gcobj,rows){
  #for some data, each strain was measured in 3 wells
  #however, these are not each legitimate biological replicates; they need to be averaged into one value
  
  #finds all datapoints that have same plate name and same strain name, so will correctly calculate for datasets with variable numbers of replicates (not just tripples)
  
  #run separately for each plate
  
  gcobj$plate<-droplevels(gcobj$plate)
  gcobj$strain<-droplevels(gcobj$strain)
  
  plates<-levels(gcobj$plate)
  strain<-levels(gcobj$strain)
  
  out1<-NULL
  out2<-NULL
  out3<-NULL
  reps<-NULL
  
  for(i in 1:length(plates)){
    
    subset<-gcobj[which(gcobj$plate==plates[i]),]
    
    for(j in 1:length(strain)) {

      strain_sub<-subset[which(subset$strain==strain[j]),]
      
      reps1<-dim(strain_sub)[1]
      
      facts<-strain_sub[1,1:10]
      
      vals<-colMeans(strain_sub[,11:18])
      
      sds<-apply(strain_sub[,11:18],2,sd)
      
      out1<-rbind(out1,facts)
      out2<-rbind(out2,vals)
      out3<-rbind(out3,sds)
      reps<-rbind(reps,reps1)
      
    } #end j-loop
    
  } #end i-loop
  
  colnames(out3)<-paste0("sd.",colnames(gcobj)[11:18])
  out<-cbind.data.frame(out1,reps,out2,out3)

  out$plate.row<-as.factor(rep(rows,times=dim(out)[1]))
  

  
  out<-out[,-which(colnames(out)=="X"|colnames(out)=="used.model")]  #remove column 7 ($used.model, all blank) so that measurement data is still in col 11
  colnames(out)[10]<-"reps"
  
  out$well<-as.factor(paste0(out$plate.row,out$plate.col))
  
  return(out)
  
  
}