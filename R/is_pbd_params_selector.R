#' Determine if the supplied object in an PBD parameter selector,
#' as can be created by \code{create_pbd_params_selector}
#' @param x the object to be determined if it is an PBD
#'   parameters selector
#' @return TRUE if x is an PBD parameters selector
#' @examples
#' s <- create_pbd_params_selector()
#'
#' expect_true(is_pbd_params_selector(s))
#' expect_false(is_pbd_params_selector("nonsense"))
#' @author Richel J.C. Bilderbeek
#' @export
is_pbd_params_selector <- function(x) {
  result <- FALSE
  tryCatch({
    becosys::check_pbd_params_selector(x)
    result <- TRUE
  }, error = function(e) {} # nolint indeed ignore e
  )
  result
}
