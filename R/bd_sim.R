#' More intuitive interface of \link[bd]{bd_sim}
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
bd_sim <- function(
  bd_params,
  crown_age = NA,
  stem_age = NA,
  conditioned_on = "nothing"
) {
  if (!is_bd_params(bd_params)) {
    stop("'bd_params' must be a valid BD parameter set")
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
  if (!conditioned_on %in% c("nothing", "non_extinction")) {
    stop("'conditioned_on' must be either 'nothing' or 'non_extinction'")
  }
  mbd_sim(
    mbd_params = create_mbd_params(
      lambda = bd_params$spec_rate,
      mu = bd_params$ext_rate,
      nu = 0.0,
      q = 0.0
    ),
    crown_age = crown_age,
    stem_age = stem_age,
    conditioned_on = conditioned_on
  )
}
