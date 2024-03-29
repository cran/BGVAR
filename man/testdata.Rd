% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bgvar-package.R
\docType{data}
\name{testdata}
\alias{testdata}
\alias{W.test}
\title{Example data set to show functionality of the package}
\format{
The data loads two objects \code{eerDatasmall}, which is a list object of length \code{N} (i.e, the number of countries) and \code{W.trade0012}, which is an \code{N} times \code{N} weight matrix with rowsums summing up to unity and zero elements on its diagonal. The global variable, oil prices, is included in the US country model as e.g., in Dees et al. (2007). The countries are abbreviated using ISO-2 codes. The weight matrix corresponds to average annual bilateral trade flows (including services) over the period from 2000 to 2012.\code{eerDatasmall} contains the country data, for more details, see below:
\describe{
  \item{\code{W.test}}{ Weight matrix based on trade flows, rowsums equal unity.}
  \item{\code{testdata}}{ List object of length \code{N} containing \describe{
        \item{\code{y}}{ Real GDP, average of 2005=100. Seasonally adjusted, in logarithms.}
        \item{\code{Dp}}{ Consumer prices (period-on-period). CPI seasonally adjusted, in logarithm.}
        \item{\code{stir}}{ Short-term interest rate, typically 3-months money market rate.}
        \item{\code{ltir}}{ Long-term interest rates, typically 10-year government bond yields.}
        \item{\code{reer}}{ Real effective exchange rate, deflated by consumer prices.}
        \item{\code{tb}}{ Trade balance (ratio of real exports to real imports).}
        \item{\code{poil}}{ Price of oil, seasonally adjusted, in logarithms.}}
  }
}
}
\usage{
testdata
}
\description{
This data set is a subset of \code{eerData} containing just three countries with 76 quarterly observations, spanning the period from 1995Q1 to 2013Q4. The country coverage are the United States, the United Kingdom and the Euro area (EA) as a regional aggregate.
}
\keyword{datasets}
