lines.bshazard <-
function(x,conf.int=T,overall=T,col=1,lwd=1,...){
 if (overall==T){
  lines(x$time,x$hazard,lwd=lwd,lty=1, col=col,...)
    if (conf.int==T) {
        lines(x$time, x$low, lty=2, col=col,lwd=lwd,...)
        lines(x$time, x$up, lty=2, col=col,lwd=lwd,...)
    }#se CI
 }#se overall
 if (overall==F & !is.null(x$cov.value)) {
 covs<-unique(x$raw.data[,attr(x$cov.value,"names")])
    lin<-(covs-matrix(as.numeric(x$cov.value),nrow(covs),ncol(covs),byrow=T))*matrix(x$coefficients,nrow(covs),ncol(covs),byrow=T)
    HR<-exp(rowSums(lin))
# lines(x$time,x$hazard,xlab=xlab,type="n", ylab=ylab,lwd=lwd,lty=1, col=col)
    for (i in 1:nrow(covs)) {
          h<-x$hazard*HR[i]
          lines(x$time,h,lwd=lwd,lty=1, col=col,...)
  }
 }
}
