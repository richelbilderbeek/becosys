#' Determine if the supplied object in an MBD parameter selector,
#' as can be created by \code{create_mbd_params_selector}
#' @inheritParams default_params_doc
#' @return TRUE if x is an MBD parameters selector
#' @examples
#'
#' s <- create_mbd_params_selector()
#'
#' is_mbd_params_selector(s))
#' is_mbd_params_selector("nonsense"))
#' @author Richel J.C. Bilderbeek
#' @export
is_mbd_params_selector <- function(mbd_params_selector) {
  result <- FALSE
  tryCatch({
    becosys::check_mbd_params_selector(mbd_params_selector)
    result <- TRUE
  }, error = function(e) {} # nolint indeed ignore e
  )
  result
}
