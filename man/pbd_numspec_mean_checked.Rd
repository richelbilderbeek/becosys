% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pbd_numspec_checked.R
\name{pbd_numspec_mean_checked}
\alias{pbd_numspec_mean_checked}
\title{Calcates the mean number of species under protracted birth-death model of
diversification. This is a checked version with a more explicit interface
than \code{\link{pbd_numspec_mean}}.}
\usage{
pbd_numspec_mean_checked(ergs, eris, scrs, sirs, crown_ages)
}
\arguments{
\item{ergs}{one or more good species' extinction rates}

\item{eris}{one or more incipient species' extinction rates}

\item{scrs}{one or more speciation completion rates}

\item{sirs}{one or more speciation initiation rates.
This function assumes the speciation initiation rates of
good and incipient species are equal.}

\item{crown_ages}{one or more crown ages}
}
\value{
The expected number of representative species
}
\description{
Calcates the mean number of species under protracted birth-death model of
diversification. This is a checked version with a more explicit interface
than \code{\link{pbd_numspec_mean}}.
}
\examples{
  n <- pbd_numspec_mean_checked(
    ergs = 0.1,
    eris = 0.2,
    scrs = 0.3,
    sirs = 0.4,
    crown_ages = 0.5
  )
  testthat::expect_equal(n, 1.029111871)

  n <- pbd_numspec_mean_checked(
    ergs = 0.2,
    eris = 0.4,
    scrs = 0.6,
    sirs = 0.8,
    crown_ages = 1.0
  )
  testthat::expect_equal(n, 1.440746567)

  # Vectorized use
  ns <- pbd_numspec_mean_checked(
    ergs = c(0.1, 0.2),
    eris = c(0.2, 0.4),
    scrs = c(0.3, 0.6),
    sirs = c(0.4, 0.8),
    crown_ages = c(0.5, 1.0)
  )
  testthat::expect_equal(ns, c(1.029111871, 1.440746567))
}
\seealso{
\code{\link{pbd_numspec_mean}} provides for setting a time-dependence
  in the parameters and/or specify a stem age.
  \code{\link{pbd_numspec_median_checked}} calculates the median
  number of species.
}
\author{
Richel J.C. Bilderbeek
}
