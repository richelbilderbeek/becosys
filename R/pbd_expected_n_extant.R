#' Get the median of the number of extant species from a PBD process.
#' @param crown_age the crown age
#' @param scr speciation completion rate
#' @param sirg speciation initiation rate of a good species
#' @param siri speciation initiation rate of an incipient species
#' @param erg extinction rate of a good species
#' @param eri extinction rate of an incipient species
#' @param n_sims number of PBD simulations to take the median of. The
#'   higher this number, the error on this calculation is decreased
#' @export
#' @author Richel J.C. Bilderbeek
pbd_expected_n_extant <- function(
  crown_age,
  scr,
  sirg,
  siri,
  erg,
  eri,
  n_sims = 10
) {
  if (crown_age <= 0.0) {
    stop("'crown age' must be non-zero and positive")
  }
  if (scr < 0.0) {
    stop("'scr' must be zero or positive")
  }
  if (sirg < 0.0) {
    stop("'sirg' must be zero or positive")
  }
  if (siri < 0.0) {
    stop("'siri' must be zero or positive")
  }
  if (erg < 0.0) {
    stop("'erg' must be zero or positive")
  }
  if (eri < 0.0) {
    stop("'eri' must be zero or positive")
  }
  if (n_sims < 1) {
    stop("'n_sims' must be equal or greater than 1")
  }

  n_lineages <- rep(NA, n_sims)
  for (i in seq(1, n_sims)) {
    out <- pbd_sim_checked(
      erg = erg,
      eri = eri,
      scr = scr,
      sirg = sirg,
      siri = siri,
      stem_age = NULL,
      crown_age = crown_age,
      max_n_taxa = 10000
    )
    n_lineages[i] <- length(out$stree_random$tip.label)
  }
  stats::median(n_lineages)
}
