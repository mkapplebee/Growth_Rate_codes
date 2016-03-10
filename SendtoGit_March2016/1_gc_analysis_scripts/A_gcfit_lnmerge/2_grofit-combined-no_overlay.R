gcfall_no<-function(x,ln.y=FALSE) {
  
  #this version does not overlay plots
  #assumes object with 4 data columns: plate, media, strain, well
  
  library("grofit")
  source('~/Bacillus_subtilis/Growth Rates/growth-curves-ryan/no_overlay-gc_combined/gcFit3-nonoverlap.R')
  
  #generate name of x as text
  #objtxt<-deparse(substitute(x))
  
  #generate OD matrix
  OD_mat<-x[-1,-2]  #OD_mat needs to be matrix with all OD data + 3 info columns (plate,strain,well)
  #plate<-rep(as.character(objtxt),length(y[,1])) #if want to get rid of "plate" input
  #plate<-rep(as.character(plate_name),length(y[,1]))
  #OD_mat<-cbind(plate,y)
  
  OD_num<-x[-1,-c(1:4)] #to identify max OD for plot limits
  
  #generate time matrix
  m_rows <-dim(OD_mat)[1]
  n_col<-dim(OD_mat)[2]
  timepoints<-x[1,-c(1:4)]
  tp<-as.numeric(timepoints)
  
  time_mat<-t(matrix(rep(timepoints,(m_rows)),c(length(timepoints),(m_rows))))
  
  
  
  if (ln.y==TRUE) {
  controler<-grofit.control(fit.opt="s",nboot.gc=12,interactive=TRUE,log.y.gc=TRUE)
  ylabel<-"ln(OD)"}
  
  else {controler<-grofit.control(fit.opt="s",nboot.gc=12,interactive=TRUE,log.y.gc=FALSE)
        ylabel<-"OD600"
  }
  
  plot.new()
  
  plot(x=max(tp),y=max(OD_num),xlim=c(0,max(tp,na.rm=TRUE)),ylim=c(0,(max(OD_num,na.rm=TRUE))),col="white",ylab=ylabel,xlab="Time (Hrs)")
  
  gc_out<-gcFit3_difplots(time_mat,OD_mat,controler)   #Uses gcFit3
  
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