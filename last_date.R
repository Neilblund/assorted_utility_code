############Get the index of the most recent end date after a given start date
lastDate<-function(start_dates, end_dates, xvar=NULL){
  ord<-order(end_dates,start_dates  ,decreasing=T)
  xout<-(rep(NA, length(start_dates)))
  for (i in 1:length(ord)) {
    start_i <- start_dates[ord[i]]
    j <- 0
    while (i + j < length(ord)) {
      j <- j + 1
      
      ind <- end_dates[ord[(i + j)]]
      if (!is.na(ind)) {
        if (ind < start_i) {
          xout[ord[i]] <- ord[(i + j)]
          break
        }
      }
    }
  }
  
  
  if(!is.null(xvar)){
    xout<-xvar[xout]
  }
  
  return(xout)  
}
######################
