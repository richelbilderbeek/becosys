#' Higher-level interface to \link{pbd_sim}
#' @param pbd_params PBD parameters,
#'   as created by \link{create_pbd_params}
#' @param crown_age the crown age.
#'   Either \code{crown_age} or \code{stem_age} must be exclusively set
#' @param stem_age the stem age
#'   Either \code{crown_age} or \code{stem_age} must be exclusively set
#' @param add_shortest_and_longest if TRUE, the result of this
#'   function will also include the species trees obtained
#'   by sampling with 'shortest' and 'longest'
#' @export
#' @author Richel J.C. Bilderbeek
bco_pbd_sim <- function(
  pbd_params,
  crown_age = NULL,
  stem_age = NULL,
  add_shortest_and_longest = FALSE
) {
  if (!is_pbd_params(pbd_params)) {
    stop("'pbd_params' must be a valid PBD parameter set")
  }
  check_pbd_params(pbd_params) # nolint becosys function
  if (!is.null(crown_age) && crown_age < 0.0) {
    stop("'crown_age' must be positive")
  }
  if (!is.null(stem_age) && stem_age < 0.0) {
    stop("'stem_age' must be positive")
  }
  if (is.null(crown_age) && is.null(stem_age)) {
    stop("'crown_age' or 'stem_age' must be set")
  }
  if (!is.null(crown_age) && !is.null(stem_age)) {
    stop("'crown_age' or 'stem_age' must be set exclusively")
  }
  pbd_sim_checked(
    erg = pbd_params$erg,
    eri = pbd_params$eri,
    scr = pbd_params$scr,
    sirg = pbd_params$sirg,
    siri = pbd_params$siri,
    stem_age = stem_age,
    crown_age = crown_age,
    add_shortest_and_longest = add_shortest_and_longest
  )
}
