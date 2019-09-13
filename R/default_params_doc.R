#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#' @param conditioned_on what the process is conditioned on.
#' This can be either 'nothing' or 'non_extinction'"
#' @param crown_age the crown age, in time units ago
#' @param erg extinction rate of a good species
#' @param eri extinction rate of an incipient species
#' @param ext_rate per-lineage extinction rate
#' @param init_n_species initial number of species at the
#' start of simulating a phylogeny. Can be 1 (to start at the
#' stem) or 2 (to start at the crown)
#' @param lambda the sympatric speciation rate
#' @param mbd_params parameter set for the MBD model,
#'   as can be created by \link{create_mbd_params}
#' @param mbd_params_selector a structure to select MBD parameters,
#'   as can be created by \link{create_mbd_params_selector}
#' @param mu the extinction rate
#' @param nu the multiple allopatric speciation trigger rate
#' @param pbd_params parameter set for the PBD model,
#'   as can be created by \link{create_pbd_params}
#' @param pbd_params_selector a structure to select PBD parameters,
#'   as can be created by \link{create_pbd_params_selector}
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
  conditioned_on,
  crown_age,
  erg,
  eri,
  ext_rate,
  init_n_species,
  lambda,
  mbd_params,
  mbd_params_selector,
  mu,
  nu,
  pbd_params,
  pbd_params_selector,
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
