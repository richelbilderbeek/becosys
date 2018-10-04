#' More intuitive interface of \link{pbd_sim}
#' @param pbd_params PBD parameters,
#'   as created by \link{create_pbd_params}
#' @param crown_age the crown age.
#'   Either \code{crown_age} or \code{stem_age} must be exclusively set
#' @param stem_age the stem age
#'   Either \code{crown_age} or \code{stem_age} must be exclusively set
#' @export
#' @author Richel J.C. Bilderbeek
pbd_sim_checked <- function(
  pbd_params,
  crown_age = NA,
  stem_age = NA
) {
  if (!is_pbd_params(pbd_params)) {
    stop("'pbd_params' must be a valid PBD parameter set")
  }
  if (!is.na(crown_age) && crown_age < 0.0) {
    stop("'crown_age' must be positive")
  }
  if (!is.na(stem_age) && stem_age < 0.0) {
    stop("'stem_age' must be positive")
  }
  if (is.na(crown_age) && is.na(stem_age)) {
    stop("'crown_age' or 'stem_age' must be set")
  }
  if (!is.na(crown_age) && !is.na(stem_age)) {
    stop("'crown_age' or 'stem_age' must be set exclusively")
  }
  # Data transformation
  pars <- as.numeric(unlist(pbd_params))
  soc <- 1
  age <- stem_age
  if (!is.na(crown_age)) {
    soc <- 2
    age <- crown_age
  }

  # pars[1] corresponds to b_1, the speciation-initiation rate of good species
  # pars[2] corresponds to la_1, the speciation-completion rate
  # pars[3] corresponds to b_2, the speciation-initiation rate of incipient species
  # pars[4] corresponds to mu_1, the extinction rate of good species
  # pars[5] corresponds to mu_2, the extinction rate of incipient species
  pars <- c(
    pbd_params$sirg,
    pbd_params$scr,
    pbd_params$siri,
    pbd_params$erg,
    pbd_params$eri
  )

  PBD::pbd_sim(
    pars = pars,
    soc = soc,
    age = age
  )
}
