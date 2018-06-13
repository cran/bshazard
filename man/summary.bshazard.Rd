\name{summary.bshazard}
\alias{summary.bshazard}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ 
Summary of hazard curve}
\description{
Returns a list containing the hazard curve and confidence limits for the curve.}
\usage{
\method{summary}{bshazard}(object, digits = 4,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
 	the result of a call to the bshazard function. 
 }
  \item{digits}{
Number of digits to print}
 \item{\dots}{
 %%     ~~Describe \code{\dots} here~~
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use

 \item{(covariate.value)}{values of covariates at which hazard is computed (mean value)}
 \item{time}{mid point of each bin at which the curve is computed}
  \item{hazard}{hazard estimate for each bin}
  \item{lower.ci}{lower limit of the hazard confidence interval (depends on alpha level)}
  \item{upper.ci}{upper limit of the hazard confidence interval (depends on alpha level)}

\item{lambda}{smoothing parameter}
\item{df}{degrees of freedom representing the effective number of smoothing parameters}
\item{phi}{overdispersion parameter}
 }
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
bshazard,print.bshazard,plot.bshazard}
\examples{
data(lung,package="survival")
 summary(bshazard(Surv(time, status==2) ~ 1,data=lung))
 }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
