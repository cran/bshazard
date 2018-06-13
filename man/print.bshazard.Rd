\name{print.bshazard}
\alias{print.bshazard}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print a short summary of the hazard rate}
\description{
Print number of observations, number of events, total person-time and overall rate of event.}
\usage{
\method{print}{bshazard}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 	the result of a call to the bshazard function. }
 \item{\dots}{
 %%     ~~Describe \code{\dots} here~~
}
 }
\value{
\item{Number of records}{number of records in input data}
\item{...}{covariate values}
\item{n.events}{total number of events (for each covariate value)} 
\item{person.time}{total person-time (for each covariate value)} 
\item{rate}{overall rate}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
bshazard,summary.bshazard,plot.bshazard}
\examples{
data(lung,package="survival")
 fit<-bshazard(Surv(time, status==2) ~ 1,data=lung)
print(fit)    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ survival }
\keyword{ nonparametric }% __ONLY ONE__ keyword per line
