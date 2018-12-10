#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#' @param crown_age the crown age, in time units ago
#' @param erg extinction rate of a good species
#' @param eri extinction rate of an incipient species
#' @param ext_rate per-lineage extinction rate
#' @param lambda the sympatric speciation rate
#' @param mu the extinction rate
#' @param nu the multiple allopatric speciation trigger rate
#' @param q the single-lineage speciation probability at a triggered event
#' @param quantile a quantile, a value between (and including) zero
#'   to (and including) one.
#' @param scr speciation completion rate
#' @param sir speciation initiation rate
#' @param sirg speciation initiation rate of a good species
#' @param siri speciation initiation rate of an incipient species
#' @param spec_rate per-lineage speciation rate
#' @param stem_age the stem age, in time units ago
#' @author Documentation by Giovanni Laudanno,
#'   use of this function by Richel J.C. Bilderbeek
#' @note This is an internal function, so it should be marked with
#'   \code{@noRd}. This is not done, as this will disallow all
#'   functions to find the documentation parameters
default_params_doc <- function(
  crown_age,
  erg,
  eri,
  ext_rate,
  lambda,
  mu,
  nu,
  q,
  quantile,
  scr,
  sir,
  sirg,
  siri,
  spec_rate,
  stem_age
) {
  # Nothing
}
