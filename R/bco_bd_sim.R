#' Conformized name of \link{bd_sim}
#' @param bd_params BD parameters,
#'   as created by \link{create_bd_params}
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
bco_bd_sim <- function(
  bd_params,
  crown_age = NA,
  stem_age = NA,
  conditioned_on = "nothing"
) {
  bd_sim(
    bd_params = bd_params,
    crown_age = crown_age,
    stem_age = stem_age,
    conditioned_on = conditioned_on
  )
}
