#' Determine if the supplied object in an PBD parameter selector,
#' as can be created by \code{create_pbd_params_selector}
#' @param x the object to be determined if it is an PBD
#'   parameters selector
#' @return TRUE if x is an PBD parameters selector
#' @examples
#'   s <- create_pbd_params_selector()
#'   testthat::expect_true(is_pbd_params_selector(s))
#'
#'   testthat::expect_false(is_pbd_params_selector("nonsense"))
#' @author Richel J.C. Bilderbeek
#' @export
is_pbd_params_selector <- function(x) {
  for (name in becosys::get_pbd_param_names()) {
    if (!name %in% names(x)) return(FALSE)
  }
  if (!is.logical(x$erg)) return(FALSE)
  if (!is.logical(x$eri)) return(FALSE)
  if (!is.logical(x$scr)) return(FALSE)
  if (!is.logical(x$sirg)) return(FALSE)
  if (!is.logical(x$siri)) return(FALSE)
  TRUE
}
