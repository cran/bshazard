summary.bshazard <-
function(object,digits=4,...){
  #  cat("Call:\n")
#  cat("Call: ")
#  print(x$call)
  #  dput(x)
        #x$raw.data$timex$raw.data$n.event,x$raw.data$person.time
  TAB<-cbind(time=object$time,hazard=object$hazard,lower=object$low,upper=object$up)
 
#   cat("Smoothing estimates:")
#    cat("\n")        
#          cat(,sep=" ");cat("\n")#Smoothing parameter
#          cat(,sep=" ");cat("\n")#degree of freedom
#          cat(",sep=" ");cat("\n")#overdispersion parameter
  #  print(x$sv2,x$phi,x$df)
  temp<-list(call=object$call,covariate.value=object$cov.value,HazardEstimates=TAB,
             lambda=as.numeric(1/object$sv2),df=as.numeric(object$df),phi=as.numeric(object$phi))
  class(temp) <- "summary.bshazard"
  return(temp)
}
