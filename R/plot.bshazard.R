plot.bshazard <-
function(x,conf.int=T,overall=T,col=1,lwd=1,lty=1,xlab="Time", ylab="Hazard rate",border=NA,col.fill="lightgrey",...){
 if (overall==T){
  plot(x$time,x$hazard,xlab=xlab,type="l", ylab=ylab,lwd=lwd,lty=lty, col=col,...)
    if (conf.int==T) {
      polygon(c(x$time, rev(x$time)),c(x$low, rev(x$up)),col =col.fill, border = border,...)
      lines(x$time,x$hazard,xlab=xlab,type="l", ylab=ylab,lwd=2,lty=lty, col=col,...)
        #lines(x$time, x$low, lty=2, col=col,lwd=lwd)
        #lines(x$time, x$up, lty=2, col=col,lwd=lwd)
    }#se CI
 }#se overall
 if (overall==F & !is.null(x$cov.value)) {
 covs<-unique(x$raw.data[,attr(x$cov.value,"names")])
    lin<-(covs-matrix(as.numeric(x$cov.value),nrow(covs),ncol(covs),byrow=T))*matrix(x$coefficients,nrow(covs),ncol(covs),byrow=T)
    HR<-exp(rowSums(lin))
  plot(x$time,x$hazard,xlab=xlab,type="n", ylab=ylab,lwd=lwd,lty=lty,col=col,...)
    col  <- rep(col, length.out=nrow(covs))
    lty  <- rep(lty, length.out=nrow(covs))
    lwd  <- rep(lwd, length.out=nrow(covs))
  for (i in 1:nrow(covs)) {
          h<-x$hazard*HR[i]
           lines(x$time,h,xlab=xlab,type="l", ylab=ylab,lwd=lwd[i],lty=lty[i],col=col[i],...)
  }
 }
}
