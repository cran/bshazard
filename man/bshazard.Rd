\name{bshazard}
\alias{bshazard}
\alias{bshazard.default}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Nonparametric Smoothing of the Hazard Function
}
\description{
The function estimates the hazard function non parametrically from a
survival object (possibly adjusted for covariates). The smoothed
estimate is based on B-splines from the perspective of generalized
linear mixed models. Left truncated and right censoring data are
allowed.
}
\usage{
%bshazard(formula, data = parent.frame(), nbin = NULL, nk = 31, degree = 1, l0=10, lambda = NULL, phi = NULL, alpha = 0.05, err = 1e-04, verbose = T)
\method{bshazard}{default}(formula,data,nbin=NULL,nk,degree,l0,lambda,phi,alpha,err,verbose)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
 a formula object, which must have a Surv object as the
response on the left of the ~ operator. On the right, if
desired are the names of the covariates in the Poisson model
separated by + operators. For unadjusted estimates the right
hand side should be ~1.
}
  \item{data}{
a data frame containing the variables named
in the formula.}
  \item{nbin}{
 number of bins (equally spaced). If omitted, the function will split the data at each
  distinct time in the data (censoring or event).
}
  \item{nk}{
number of knots for B-splines (including minimum and
maximum, default is 31): as a rule of thumb the number of
observations divided by 4, but more than 40 knots are rarely
needed.
}
  \item{degree}{
degree of B-splines, representing the effective number of parameters in the piecewise
segments of splines (default is 1, corresponding to a linear segment)
}      
\item{l0}{
Starting value for the smoothing parameter lambda (default is 10). Relevant only if lambda=NULL
}
  \item{lambda}{
smoothing parameter. If not provided (default is NULL) it is
estimated from the data as phi times the reciprocal of the
variance of the random effect (by an iterative algorithm). A
high value
imposes a smoother estimate.}
  \item{phi}{
overdispersion parameter. If not provided (default is NULL)
it is estimated from the data.}
  \item{alpha}{
1 minus the level for the two-sided confidence interval for the
hazard function. Default is 0.05.}
  \item{err}{
relative error for the
iterative process in the lambda/phi estimation (default is
0.0001)}
  \item{verbose}{
TRUE/FALSE Print each iteration step (default is T)}
}
\details{
The time axis is partitioned in a number of intervals 
equal to the number of different observations (if not fixed
otherwise by the option \code{nbin}). The number of events in each
interval is modeled by a Poisson model and the smoothing parameter
(lambda) is estimated by a mixed model framework. The number of
knots is 31 by default. The code also allows for overdispersion
(phi). Adjustment for covariates can increase the computation time.}
\value{The output of the bshazard function can be divided into three parts:
1. a data frame with the original data split in time intervals, 2.
the set of vectors containing the estimated hazard and confidence limits and 3. the parameter estimates.
  %\item ORIGINAL DATA
\item{raw.data}{data frame with original data split in bins (intervals of time), containing:} 
\item{time}{mid point of each bin}  
\item{n.event}{number of events in each bin}  
\item{PY}{total person-time in each bin (for each covariate value)} 
\item{raw.hazard}{number of events in each interval divided by PY}
\item{...}{covariate values}

\item{nobs}{number of records in input data}
%\item HAZARD ESTIMATE
  \item{time}{mid point of each bin at which the curve is computed}
  \item{hazard}{hazard estimate for each bin}
  \item{lower.ci}{lower limit of the hazard confidence interval (depends on alpha level)}
  \item{upper.ci}{upper limit of the hazard confidence interval (depends on alpha level)}
  \item{(cov.value)}{values of covariates at which hazard is computed (mean value)}
  \item{fitted.values}{estimated number of events at each time (from the Poisson model)}
%  \item PARAMETER ESTIMATES
      \item{coefficients}{coefficient estimates for each covariate}
      \item{phi}{overdispersion parameter}
      \item{sv2}{variance of the random effect corresponding to the inverse of the smoothing parameter multiplied by phi}
      \item{df}{degrees of freedom representing the effective number of smoothing parameters}
}
\references{
Rebora P, Salim A, Reilly M (2014) bshazard: A Flexible Tool for Nonparametric
Smoothing of the Hazard Function.The R Journal Vol. 6/2:114-122.

Lee Y, Nelder JA, Pawitan Y (2006). Generalized Linear Models with Random Effects: Unified
Analysis via H-likelihood, volume 106. Chapman & Hall/CRC.

Pawitan Y (2001). In All Likelihood: Statistical Modelling and Inference Using Likelihood.
Oxford University Press
}
\author{
Paola Rebora, Agus Salim, Marie Reilly
Maintainer: Paola Rebora <paola.rebora@unimib.it>
}
%\note{%%  ~~further notes~~ }
%% ~Make other sections like Warning with \section{Warning }{....} ~
%\seealso{\code{\link{help(survival}}}
\examples{
data(lung,package="survival")
  fit<-bshazard(Surv(time, status==2) ~ 1,data=lung)
  plot(fit$time, fit$hazard,xlab='Time', ylab='Rate/person-days',type='l')
  points(fit$raw.data$time,fit$raw.data$raw.hazard,cex=.3, lwd=3,col=1)
  lines(fit$time, fit$hazard, lty=1, lwd=3)
  lines(fit$time, fit$lower.ci, lty=1, lwd=1)
lines(fit$time, fit$upper.ci, lty=1, lwd=1)
# or alternatively
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

#Alternatively an adjusted estimate can be performed, assuming proportionl hazard. 
##This computation can be time consuming!
\dontrun{fit.adj <- bshazard(Surv(T12obs,status) ~ cat )
plot(fit.adj,overall=FALSE, xlab = 'Time since illness',col=rainbow(4),lwd=2, xlim = c(0,5), 
ylim = c(0,10))
legend("top",title="Time to illness",c("<=0.5","0.5-|1","1-|1.5","1.5-|2"),col=c(rainbow(4)),lwd =2)}

### A more general setting with examples of Markov assumption evaluation can be found 
### in Bernasconi et al. Stat in Med 2016
## The function is currently defined as
function (x, ...) 
UseMethod("bshazard")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ survival }
\keyword{ nonparametric }% __ONLY ONE__ keyword per line
\keyword{ smooth }
