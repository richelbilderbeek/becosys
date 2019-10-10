#' Calcates the mean number of species under protracted birth-death model of
#' diversification. This is a checked version with a more explicit interface
#' than \code{\link{pbd_numspec_mean}}.
#' @param sirs one or more speciation initiation rates.
#'   This function assumes the speciation initiation rates of
#'   good and incipient species are equal.
#' @param ergs one or more good species' extinction rates
#' @param scrs one or more speciation completion rates
#' @param eris one or more incipient species' extinction rates
#' @param crown_ages one or more crown ages
#' @return The expected number of representative species
#' @seealso
#'   \code{\link{pbd_numspec_mean}} provides for setting a time-dependence
#'   in the parameters and/or specify a stem age.
#'   \code{\link{pbd_numspec_median_checked}} calculates the median
#'   number of species.
#' @examples
#'   n <- pbd_numspec_mean_checked(
#'     ergs = 0.1,
#'     eris = 0.2,
#'     scrs = 0.3,
#'     sirs = 0.4,
#'     crown_ages = 0.5
#'   )
#'   testthat::expect_equal(n, 1.029111871)
#'
#'   n <- pbd_numspec_mean_checked(
#'     ergs = 0.2,
#'     eris = 0.4,
#'     scrs = 0.6,
#'     sirs = 0.8,
#'     crown_ages = 1.0
#'   )
#'   testthat::expect_equal(n, 1.440746567)
#'
#'   # Vectorized use
#'   ns <- pbd_numspec_mean_checked(
#'     ergs = c(0.1, 0.2),
#'     eris = c(0.2, 0.4),
#'     scrs = c(0.3, 0.6),
#'     sirs = c(0.4, 0.8),
#'     crown_ages = c(0.5, 1.0)
#'   )
#'   testthat::expect_equal(ns, c(1.029111871, 1.440746567))
#' @author Richel J.C. Bilderbeek
#' @export
pbd_numspec_mean_checked <- function(
  ergs,
  eris,
  scrs,
  sirs,
  crown_ages
) {
  if (any(sirs < 0.0)) stop("All 'sirs' must be positive")
  if (any(ergs < 0.0)) stop("All 'ergs' must be positive")
  if (any(scrs < 0.0)) stop("All 'scrs' must be positive")
  if (any(eris < 0.0)) stop("All 'eris' must be positive")
  if (any(crown_ages < 0.0)) stop("All 'crown_ages' must be positive")
  if (length(ergs) != length(eris)) {
    stop("ergs' and 'eris' must have same length")
  }
  if (length(ergs) != length(scrs)) {
    stop("ergs' and 'scrs' must have same length")
  }
  if (length(ergs) != length(sirs)) {
    stop("ergs' and 'sirs' must have same length")
  }
  if (length(ergs) != length(crown_ages)) {
    stop("ergs' and 'crown_ages' must have same length")
  }
  n <- length(ergs)
  ns <- rep(NA, n)
  for (i in seq_along(ergs)) {
    ns[i] <- pbd_numspec_mean_checked_impl(
      erg = ergs[i],
      eri = eris[i],
      scr = scrs[i],
      sir = sirs[i],
      crown_age = crown_ages[i]
    )
  }
  ns
}

#' Implementation of \code{\link{pbd_numspec_mean_checked}}.
#' @param sir speciation initiation rate. This function assumes the
#'   speciation initiation rates of good and incipient species are equal.
#' @param erg extinction rate of a good species
#' @param scr speciation completion rate
#' @param eri extinction rate of an incipient species
#' @param crown_age the crown age
#' @return The expected number of representative species
#' @seealso
#'   \code{\link{pbd_numspec_mean}} provides for setting a time-dependence
#'   in the parameters and/or specify a stem age.
#'   \code{\link{pbd_numspec_median_checked}} calculates the median
#'   number of species.
#' @examples
#'  mean_n_species <- pbd_numspec_mean_checked_impl(
#'    erg = 0.12,
#'    eri = 0.23,
#'    scr = 0.34,
#'    sir = 0.45,
#'    crown_age = 0.56
#'  )
#'  expected_mean_n_species <- 1.046121595
#'  testthat::expect_equal(mean_n_species, expected_mean_n_species)
#' @author Richel J.C. Bilderbeek
#' @noRd
pbd_numspec_mean_checked_impl <- function(
  erg,
  eri,
  scr,
  sir,
  crown_age
) {
  if (sir < 0.0) stop("'sir' must be positive")
  if (erg < 0.0) stop("'erg' must be positive")
  if (scr < 0.0) stop("'scr' must be positive")
  if (eri < 0.0) stop("'eri' must be positive")
  if (crown_age < 0.0) stop("'crown_age' must be positive")
  PBD::pbd_numspec_mean(pars = c(sir, erg, scr, eri), age = crown_age, soc = 2)
}

#' Calcates the median number of species under protracted birth-death model of
#' diversification. This is a checked version with a more explicit interface
#' than \code{\link{pbd_numspec_median}}.
#' @param sir speciation initiation rate. This function assumes the
#'   speciation initiation rates of good and incipient species are equal.
#' @param erg extinction rate of a good species
#' @param scr speciation completion rate
#' @param eri extinction rate of an incipient species
#' @param crown_age the crown age
#' @return The median number of representative species
#' @seealso
#'   \code{\link{pbd_numspec_median}} provides for setting a time-dependence
#'   in the parameters and/or specify a stem age.
#'   \code{\link{pbd_numspec_mean_checked}} calculates the
#'   mean number of species.
#' @examples
#'   median_n_species <- pbd_numspec_median_checked(
#'     erg = 1.2,
#'     eri = 2.3,
#'     scr = 3.4,
#'     sir = 4.5,
#'     crown_age = 6.7
#'   )
#'   median_n_species_expected <- 99408712
#'   testthat::expect_equal(median_n_species, median_n_species_expected)
#' @author Richel J.C. Bilderbeek
#' @export
pbd_numspec_median_checked <- function(
  erg,
  eri,
  scr,
  sir,
  crown_age
) {
  if (sir < 0.0) stop("'sir' must be positive")
  if (erg < 0.0) stop("'erg' must be positive")
  if (scr < 0.0) stop("'scr' must be positive")
  if (eri < 0.0) stop("'eri' must be positive")
  if (crown_age < 0.0) stop("'crown_age' must be positive")
  PBD::pbd_numspec_median(
    pars = c(sir, erg, scr, eri),
    age = crown_age,
    soc = 2
  )
}
