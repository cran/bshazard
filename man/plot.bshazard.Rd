\name{plot.bshazard}
\alias{plot.bshazard}
\alias{lines.bshazard}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Method for 'bshazard' }
\description{
A plot of hazard rate is produced. The overall option allows to plot an hazard rate for each covariate value (assuming proportional hazard). }
\usage{
\method{plot}{bshazard}(x, conf.int = T, overall = T, col = 1, lwd = 1, lty = 1, xlab = "Time", 
ylab = "Hazard rate",border=NA,col.fill="lightgrey",...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 	the result of a call to the bshazard function. 
}
  \item{conf.int}{
Determines whether confidence intervals will be plotted. The default is to do so if there is only 1 curve, i.e., no strata. }
  \item{overall}{
Determines whether an overall curve is plotted (default overall=T) or a curve for each covariate value in the data (overall=F). 
It works only if there are covariates. }
  \item{col}{
  a vector of integers specifying colors for each curve. The default value is 1. }
  \item{lwd}{
a vector of integers specifying line width for each curve. The default value is 1. }
   \item{lty}{
a vector of integers specifying line types for each curve. The default value is 1.
%%     ~~Describe \code{lty} here~~
}
 \item{xlab}{
 	label given to the x-axis. }
  \item{ylab}{
 	label given to the y-axis. }
 	  \item{border}{
 	the color to draw the border. The deafult value is NA.}
  \item{col.fill}{
 	the color for filling the polygon. The default is lightgrey.}

  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
bshazard,summary.bshazard,print.bshazard}
\examples{
data(lung)
fit<-bshazard(Surv(time, status==2) ~ 1,data=lung)
plot(fit)


#Example to graphically evaluate the Markov assumpion in an illness-death model
### data simulated under EXTENDED SEMI-MARKOV model
set.seed(123)
n  <- 500
beta<-log(3)
R  <- runif(n, min = 0, max =2)
cat <- cut(R, breaks = seq(0,2,0.5), labels = seq(1,4,1))
T12  <- 1/0.2*( (-log(runif(n, min = 0, max = 1))) / (exp(beta*R)))**(1/0.6)
C  <- runif(n, min =0, max = 10)
T12obs <- pmin(T12, C)
status  <- ifelse(T12obs < C, 1, 0)
T012obs <- R+T12obs
#R: simulated time to illness
#cat: time to illness categorised in 4 classes
#T12obs:  time from illness to death or censoring
#T012obs: time from origin to death or censoring
#status: indicator of death (1=death,0=censoring)

# Hazard estimate in the Clock Reset scale (time from illness) by time to illness class
fit <- bshazard(Surv(T12obs[cat == 1],status[cat==1]) ~ 1)
plot(fit,conf.int=FALSE,xlab='Time since illness',xlim=c(0,5),ylim=c(0,10),lwd=2,col=rainbow(1))
for(i in 2:4) {
  fit <- bshazard(Surv(T12obs[cat == i],status[cat==i]) ~ 1)
  lines(fit$time, fit$hazard, type = 'l', lwd = 2, col = rainbow(4)[i])
}
legend("top",title="Time to illness",c("<=0.5","0.5-|1","1-|1.5","1.5-|2"),col=c(rainbow(4)),lwd =2)

# Hazard estimate in the Clock Forward scale (time from origin) by time to illness class
fit <- bshazard(Surv(R[cat == 1],T012obs[cat == 1],status[cat==1]) ~ 1)
plot(fit,conf.int=FALSE,xlab='Time since origin',xlim=c(0,5),ylim=c(0,10),lwd=2,col=rainbow(1))
for(i in 2:4) {
  fit <- bshazard(Surv(R[cat == i],T012obs[cat == i],status[cat==i]) ~ 1)
  lines(fit$time, fit$hazard, type = 'l', lwd = 2, col = rainbow(4)[i])
}
legend("top",title="Time to illness",c("<=0.5","0.5-|1","1-|1.5","1.5-|2"),col=c(rainbow(4)),lwd =2)

#Alternatively an adjusted estimate can be performed, assuming proportionl hazard 
# this computation can be time consuming!
\dontrun{fit.adj <- bshazard(Surv(T12obs,status) ~ cat )
plot(fit.adj,overall=FALSE, xlab = 'Time since illness',col=rainbow(4),lwd=2, xlim = c(0,5), 
ylim = c(0,10))
legend("top",title="Time to illness",c("<=0.5","0.5-|1","1-|1.5","1.5-|2"),col=c(rainbow(4)),lwd =2)}

### A more general setting with examples of Markov assumption evaluation can be found 
### in Bernasconi et al. Stat in Med 2016

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ survival }
\keyword{ nonparametric }% __ONLY ONE__ keyword per line
\keyword{ smooth }
\keyword{ aplot }
