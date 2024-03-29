% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{conv.diag}
\alias{conv.diag}
\title{MCMC Convergence Diagnostics}
\usage{
conv.diag(object, crit.val=1.96)
}
\arguments{
\item{object}{A fitted \code{bgvar} object.}

\item{crit.val}{Critical value used for test statistic.}
}
\value{
Returns an object of class \code{bgvar.CD}. This is a list with \describe{
\item{\code{geweke.z}}{ Z-scores for a test of equality of means between the first and last parts of the chain. A separate statistic is calculated for each variable in each chain.}
\item{\code{perc}}{ is the percentage of Z-scores exceeding \code{crit.val} (in absolute terms).}
}
}
\description{
This function computes Geweke's Convergence diagnostic making use of the \code{coda} package.
}
\details{
Geweke (1992) proposed a convergence diagnostic for Markov chains based on a test for equality of the means of the first and last part of a Markov chain (by default we use the first 10\% and the last 50\%). If the samples are drawn from the stationary distribution of the chain, the two means are equal and Geweke's statistic has an asymptotically standard normal distribution. The test statistic is a standard Z-score: the difference between the two sample means divided by its estimated standard error. The standard error is estimated from the spectral density at zero and so takes into account any autocorrelation.
}
\examples{
\donttest{
library(BGVAR)
data(testdata)
model.mn <- bgvar(Data=testdata,W=W.test,plag=1,draws=200,burnin=200,prior="MN")
geweke <- conv.diag(model.mn)
}
}
\references{
Geweke, J. (1992) Evaluating the accuracy of sampling-based approaches to calculating posterior moments. \emph{Bayesian Statistics} 4 (edited by JM Bernado, JO Berger, AP Dawid and AFM Smith). Clarendon Press, Oxford, UK.
}
\seealso{
\code{\link[coda]{geweke.diag}} in the \code{coda} package.
\code{\link{bgvar}} for estimation of a \code{bgvar} object.
}
\author{
Martin Feldkircher
}
