\name{bshazard-package}
\alias{bshazard-package}
\docType{package}
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
\details{
The DESCRIPTION file:
\packageDESCRIPTION{bshazard}
\packageIndices{bshazard}
}
\author{
Paola Rebora, Agus Salim, Marie Reilly
Maintainer: Paola Rebora <paola.rebora@unimib.it>
}
\references{
Rebora P, Salim A, Reilly M (2014) bshazard: A Flexible Tool for Nonparametric
Smoothing of the Hazard Function.The R Journal Vol. 6/2:114-122.

Lee Y, Nelder JA, Pawitan Y (2006). Generalized Linear Models with Random Effects: Unified
Analysis via H-likelihood, volume 106. Chapman & Hall/CRC.

Pawitan Y (2001). In All Likelihood: Statistical Modelling and Inference Using Likelihood.
Oxford University Press
}
\keyword{ survival }
\keyword{ nonparametric }% __ONLY ONE__ keyword per line
\keyword{ smooth }  

\examples{
data(lung,package="survival")
  fit<-bshazard(Surv(time, status==2) ~ 1,data=lung)
  plot(fit)
}
