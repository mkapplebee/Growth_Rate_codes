gcfall<-function(x,ln.y=FALSE) {
  
  library("grofit")
  
  source('~/Bacillus_subtilis/Growth Rates/growth-curves-ryan/GrowthRateScripts_Dec2015/3c_gcFit5.R')
  
  #generate name of x as text
  #objtxt<-deparse(substitute(x))
  
  # "x" is dataframe with 3 columns of extra data in it
  
  #generate OD matrix
  OD_mat<-x[-1,]  #remove first row, which is the time row
  OD_num<-OD_mat[,-1:-3]
  
  #generate time matrix
  m_rows <-nrow(x)
  n_col<-ncol(x)
  timepoints<-x[1,4:n_col]  #assumes first 3 columns are info
  tp<-as.numeric(timepoints)
  
  time_mat<-t(matrix(rep(timepoints,(m_rows-1)),c((n_col-3),(m_rows-1))))
  
  if (ln.y==TRUE) {
  controler<-grofit.control(fit.opt="s",nboot.gc=12,interactive=TRUE,log.y.gc=TRUE)
  ylabel<-"ln(OD)"}
  
  else {controler<-grofit.control(fit.opt="s",nboot.gc=12,interactive=TRUE,log.y.gc=FALSE)
        ylabel<-"OD600"
  }
  
  plot.new()
  
  plot(x=max(tp),y=max(OD_num),xlim=c(0,max(tp,na.rm=TRUE)),ylim=c(0,(max(OD_num,na.rm=TRUE))),col="white",ylab=ylabel,xlab="Time (Hrs)")
  
  gc_out<-gcFit5(time_mat,OD_mat,controler)   #Uses gcFit3
  
  data<-summary(gc_out)
  char<-data$concentration
  plate.row<-substr(char,1,1)
  plate.col<-substr(char,2,3)
  data<-cbind(plate.row,plate.col,data)
  data$plate.col<-ordered(data$plate.col,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
  #data$AddId<-ordered(data$AddId,levels=c("AM373","LiaS","McpA","NhaK","LM","LN","MN"))
  data<-data[,c(1:10,30:57)]  # gets rid of unused columns
  colnames(data)[3:5]<-c("plate","strain","well")
  
  return(data)
  
}