#' Conformized name of \link{mbd_sim}
#' @param mbd_params MBD parameters,
#'   as created by \link[mbd]{create_mbd_params}
#' @param crown_age the crown age.
#'   Either \code{crown_age} or \code{stem_age} must be exclusively set
#' @param stem_age the stem age
#'   Either \code{crown_age} or \code{stem_age} must be exclusively set
#' @param conditioned_on conditioning of the simulation, can be:
#'   \itemize{
#'     \item \code{nothing} no conditioning, species can all go extinct
#'     \item \code{no_extinction} species cannot all go extinct
#'   }
#' @export
#' @author Richel J.C. Bilderbeek
bco_mbd_sim <- function(
  mbd_params,
  crown_age = NA,
  stem_age = NA,
  conditioned_on = "nothing"
) {
  mbd_sim(
    mbd_params = mbd_params,
    crown_age = crown_age,
    stem_age = stem_age,
    conditioned_on = conditioned_on
  )
}
