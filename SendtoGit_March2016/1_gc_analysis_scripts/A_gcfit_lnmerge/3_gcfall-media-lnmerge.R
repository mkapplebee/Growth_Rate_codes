#take a file with all Eon data for a given media  (col 1:4 - plate, media, strain, well) which has already been tested in gcfall_no for bad fits
#and generate output for ln.y=FALSE and ln.y=TRUE, merge the tables, and generate plot with all data


gcfall_mergeln<-function(gcobj) {
  
  source('~/Bacillus_subtilis/Growth Rates/growth-curves-ryan/GrowthRateScripts_Dec2015/3b_grofit-combined-Dec2015.R')
  #source('~/Bacillus_subtilis/Growth Rates/growth-curves-ryan/grofit-scripts-Feb2014/norm_gccolumns.R')
  
  #gcobj has 4 extra variables, but gcfall needs object with only 3 extra columns
    #strain & well need to be the 2nd/3rd columns, respectively
  
  gcfall_ln_output<-gcfall(gcobj[,-2],ln.y=TRUE)  #removes media data
  #gcfall_ln_output$plate<-gcobj[-1,1]
  
  gcfout<-gcfall(gcobj[,-2],ln.y=FALSE)
  #gcfout$plate<-gcobj[-1,1]

    
    
  ln_mu.spline<-gcfall_ln_output[,11]
  
  ln_mu.bt<-gcfall_ln_output[,15]
  
  stdln_mu.bt<-gcfall_ln_output[,19]
  
  ci90.ln_mu.bt.lo<-gcfall_ln_output[,23]
  
  ci90.ln_mu.bt.up<-gcfall_ln_output[,24]
  
  ci95.ln_mu.bt.lo<-gcfall_ln_output[,31]
  
  ci95.ln_mu.bt.up<-gcfall_ln_output[,32]
  
  table<-cbind.data.frame(gcfout[,1:10],ln_mu.spline,gcfout[,11:14],ln_mu.bt,gcfout[,15:18],stdln_mu.bt,gcfout[,19:22],
               ci90.ln_mu.bt.lo,ci90.ln_mu.bt.up,gcfout[,23:30],ci95.ln_mu.bt.lo,ci95.ln_mu.bt.up,gcfout[,31:38])
  
  
  table$plate<-as.factor(table$plate)
  
  table$strain<-as.factor(table$strain)
  table$strain<-droplevels(table$strain)
  
  
  #table1<-normalize(table)
  
  return(table)

}