print.bshazard <-
function(x,...){
#  cat("Call:\n")
   cat("Call: ")
   print(x$call)
#  dput(x)
  cat("\n")
  cat("Number of records: ",x$n)
   cat("\n")
   ncov<-length(x$cov.value)
  if (is.null(x$cov.value)) {
      #n<-length(x$raw.data$time)
     n.e<-sum(x$raw.data$n.event)
     pt<-sum(x$raw.data$person.time)
     r<-n.e/pt
     print(data.frame("n.events"=as.numeric(n.e),"person.time"=as.numeric(pt),"rate"=as.numeric(r)))
  }
  if (!is.null(x$cov.value)){
    # n.e<-tapply(x$raw.data$n.event,x$raw.data[,5:(5+ncov-1)],sum)
    # pt<-tapply(x$raw.data$person.time,x$raw.data[,5],sum)
     agg<-aggregate(x$raw.data, by = list(x$raw.data[,5:(5+ncov-1)]), FUN = "sum")
     agg$rate<-agg$n.event/agg$person.time
     agg[,attr(x$cov.value,"names")]<-agg[,1:(1+ncov-1)]
     print(data.frame(agg[,c(attr(x$cov.value,"names"),"n.event","person.time","rate")]))
  } 
   # cat("Number of records",as.numeric(n),sep=" ")
#    cat("\n")
#   cat("Number of events", as.numeric(n.e),sep=" ");cat("\n")
#   cat("Total person-time", as.numeric(pt),sep=" ");cat("\n")
#    cat("Overall rate of event",as.numeric(r),sep=" ") ;cat("\n")                                    
 }
