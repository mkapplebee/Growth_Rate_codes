gcFit5<- function (time, data, control = grofit.control()) 
{  # time - numeric matrix, n columns for timepoints, m rows for different experiments
  # 3+n columns; n columns of data corresponding to time, m rows for different experiments
  # extra 3 columns:  1) experiment ident 2)add ID (strain)  3) concentration of substrate
  # if-statement added at line 35 to convert OD-data to ln-converted data if control$log.y.gc=TRUE
  
  #creates plots of growth curves for each day that are overlaid
  
  #color controls for plotting - put before for-loop to reduce redudancy
  col.sel<-c("black","blue","red","orange","darkorchid","green4","deeppink","lawngreen","hotpink","darkslategrey","deeppink4")
  strain.list<-levels(droplevels(as.factor(data[,2])))
  col.assign<-col.sel[match((as.character(data[,2])),strain.list)]
  num.levs<-length(strain.list)

  
  
  if (is(control) != "grofit.control")  
    stop("control must be of class grofit.control!")
  if ((dim(time)[1]) != (dim(data)[1]))   # must have same number of experiments (rows) in time and data matrix
    stop("gcFit: Different number of datasets in data and time")
  if (!is.element(control$fit.opt, c("s", "m", "b"))) {
    warning("fit.opt must be set to 's', 'm' or 'b'. Changed to 'b'!")
    fit.opt = "b"
  }
  out.table <- NULL
  used.model <- NULL
  fitpara.all <- list()
  fitnonpara.all <- list()
  boot.all <- list()
  fitted.param <- NULL
  fitted.nonparam <- NULL
  bootstrap.param <- NULL
  
  
  for (i in 1:nrow(data)) {   #1:number of rows (1 experiment/row)
  
    
    acttime <- as.numeric(as.matrix(time[i, ]))  #acttime - time of experiment i
    
    actwell <- as.numeric(as.matrix((data[i, -1:-3])))  #actwell - data of experiment i, with first 3 datapoints removed
    if (control$log.y.gc == TRUE) { actwell <- log(1 + actwell)}
    
    gcID <- as.matrix(data[i, 1:3])  #first three identifiers of experiment i
    if ((control$suppress.messages == FALSE)) {
      cat("\n\n")
      cat(paste("= ", as.character(i), ". growth curve =================================\n", 
                sep = ""))
      cat("----------------------------------------------------\n")
    }
    
    
#run gcFitModel.  
                #If fit to a model, takes created objects for experiment i and feeds them to gcFitModel
                # Adds fitpara output for exp i as "i-th" object in fitpara.all.  If "s", then an empty object is created and stored in "i-th" place
    
    if ((control$fit.opt == "m") || (control$fit.opt == 
                                       "b")) {
      fitpara <- gcFitModel(acttime, actwell, gcID, control)  
      fitpara.all[[i]] <- fitpara                              
    } else {
      fitpara <- list(raw.x = acttime, raw.y = actwell, 
                      gcID = gcID, fit.x = NA, fit.y = NA, parameters = list(A = NA, 
                                                                             mu = NA, lambda = NA, integral = NA), model = NA, 
                      nls = NA, reliable = NULL, fitFlag = FALSE, 
                      control = control)
      class(fitpara) <- "gcFitModel"
      fitpara.all[[i]] <- fitpara
    }
    
#run gcFitSpline.  Create fitnonpara.all list for each row.
    
    if ((control$fit.opt == "s") || (control$fit.opt == 
                                       "b")) {
      nonpara <- gcFitSpline(acttime, actwell, gcID, control)  #if fit to a model, takes created objects for experiment i and feeds them to gcFitSpline
      fitnonpara.all[[i]] <- nonpara                           # adds fitpara output for exp i as "i-th" object in fitnonpara.all
    } else {
      nonpara <- list(raw.x = acttime, raw.y = actwell, 
                      gcID = gcID, fit.x = NA, fit.y = NA, parameters = list(A = NA, 
                                                                             mu = NA, lambda = NA, integral = NA), parametersLowess = list(A = NA, 
                                                                                                                                           mu = NA, lambda = NA), spline = NA, reliable = NULL, 
                      fitFlag = FALSE, control = control)
      class(nonpara) <- "gcFitSpline"
      fitnonpara.all[[i]] <- nonpara
    }

#Generate plot, if control$interactive == TRUE

    if ((control$interactive == TRUE)) {
      #if (fitpara$fitFlag == TRUE) {
        #plot(fitpara, colData = 1, colModel = 1, cex = 1.5)
        #plot(nonpara, add = TRUE, raw = FALSE, colData = 0, 
        #     colSpline = 2, cex = 1.5)
      #}
      
    #Plot command is here:
        plot(nonpara, add = TRUE, raw = FALSE,colData = col.assign[i], colSpline = col.assign[i], cex = 1.5)
        indexA<-which.max(nonpara$spline$y)
        points(x=nonpara$spline$x[indexA],y=nonpara$spline$y[indexA],pch=20)
      #}
      #colData: data color, colSplie: spline color.  Spline & mu-line are both incoded in "nonpara"
      #add=TRUE will overlay sequential plots
      #raw = TRUE means datapoints will be ploted on spline fit
      #plot parameters also encoded in grofit-combined, based on first plot
      
      #Add title
      #title(as.character(data[i, 1]))
      
        legend("bottomright", legend = strain.list[1:num.levs], 
               col = col.sel[1:num.levs], lty = 2,cex=0.6)
    }  #else {     *there is no else-command, so function won't work if control$interactive != TRUE
    
    
    reliability_tag <- NA
    if (control$interactive == TRUE) {
      answer <- readline("Are you satisfied (y/n)?")
      if (substr(answer, 1, 1) == "n") {
        cat("\n Tagged this well as unreliable !\n\n")
        reliability_tag <- FALSE
        fitpara.all[[i]]$reliable <- FALSE
        fitnonpara.all[[i]]$reliable <- FALSE
      }
      else {
        reliability_tag <- TRUE
        fitpara.all[[i]]$reliable <- TRUE
        fitnonpara.all[[i]]$reliable <- TRUE
        cat("Well was (more ore less) o.k.\n")
      }
    }
    else {
      reliability_tag <- TRUE
    }
    if (control$interactive == TRUE) 

    if ((control$nboot.gc > 0) && (reliability_tag == TRUE)) {
      bt <- gcBootSpline(acttime, actwell, gcID, control)
      boot.all[[i]] <- bt
    }
    else {
      bt <- list(raw.x = acttime, raw.y = actwell, gcID = gcID, 
                 boot.x = NA, boot.y = NA, boot.gcSpline = NA, 
                 lambda = NA, mu = NA, A = NA, integral = NA, 
                 bootFlag = FALSE, control = control)
      class(bt) <- "gcBootSpline"
      boot.all[[i]] <- bt
    }
    description <- data.frame(TestId = data[i, 1], AddId = data[i, 
                                                                2], concentration = data[i, 3], reliability = reliability_tag, 
                              used.model = fitpara$model, log.x = control$log.x.gc, 
                              log.y = control$log.y.gc, nboot.gc = control$nboot.gc)
    fitted <- cbind(description, summary(fitpara), summary(nonpara), 
                    summary(bt))
    out.table <- rbind(out.table, fitted)
  }
  
  
  gcFit <- list(raw.time = time, raw.data = data, gcTable = out.table, 
                gcFittedModels = fitpara.all, gcFittedSplines = fitnonpara.all, 
                gcBootSplines = boot.all, control = control)
  class(gcFit) <- "gcFit"
  gcFit
}
