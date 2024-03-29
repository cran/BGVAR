% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BGVAR.R
\name{logLik}
\alias{logLik}
\alias{logLik.bgvar}
\title{Extract Log-likelihood of Bayesian GVAR}
\usage{
\method{logLik}{bgvar}(object, ..., quantile = 0.5)
}
\arguments{
\item{object}{An object of class \code{bgvar}.}

\item{...}{Additional arguments.}

\item{quantile}{Reported quantiles. Default is set to median.}
}
\value{
Returns an vector of dimension \code{q} (number of specified quantiles) of global log-likelihoods.
}
\description{
Extracts Log-Likelihood for \code{bgvar}.
}
\examples{
\donttest{
library(BGVAR)
data(testdata)
model.ng <- bgvar(Data=testdata,W=W.test,plag=1,draws=100,burnin=100)
logLik(model.ng)
}
}
\seealso{
\code{\link{bgvar}} for estimation of a \code{bgvar} object.
}
