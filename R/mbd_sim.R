#' More intuitive interface of \link[mbd]{mbd_sim}
#' @param mbd_params MBD parameters,
#'   as created by \link{create_mbd_params}
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
mbd_sim <- function(
  mbd_params,
  crown_age = NA,
  stem_age = NA,
  conditioned_on = "nothing"
) {
  check_mbd_params(mbd_params)
  if (!is.na(crown_age)) check_crown_age(crown_age)
  if (!is.na(stem_age) && stem_age < 0.0) {
    stop("'stem_age' must be positive")
  }
  if (is.na(crown_age) && is.na(stem_age)) {
    stop("'crown_age' or 'stem_age' must be set")
  }
  if (!is.na(crown_age) && !is.na(stem_age)) {
    stop("'crown_age' or 'stem_age' must be set exclusively")
  }
  check_conditioned_on(conditioned_on)
  # Data transformation
  pars <- as.numeric(unlist(mbd_params))
  n_0 <- 1
  age <- stem_age
  if (!is.na(crown_age)) {
    n_0 <- 2
    age <- crown_age
  }
  cond <- 0
  if (conditioned_on == "non_extinction") {
    cond <- 1
  }
  mbd::mbd_sim(
    pars = pars,
    n_0 = n_0,
    age = age,
    cond = cond
  )
}
