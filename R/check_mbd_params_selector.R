#' Check if the supplied object in an MBD parameter selector.
#'
#' Will link{stop} if not.
#' @inheritParams default_params_doc
#' @examples
#' library(testthat)
#' expect_silent(check_mbd_params_selector(create_mbd_params_selector()))
#' expect_error(check_mbd_params_selector("nonsense"))
#' @author Richel J.C. Bilderbeek
#' @export
check_mbd_params_selector <- function(mbd_params_selector) {
  for (name in get_mbd_param_names()) { # nolint internal function
    if (!name %in% names(mbd_params_selector)) {
      stop(
        "'", deparse(substitute(mbd_params_selector)), "' ",
        "is missing element '", name, "'"
      )
    }
  }
  assertive::assert_is_if_condition(mbd_params_selector$lambda)
  assertive::assert_is_if_condition(mbd_params_selector$mu)
  assertive::assert_is_if_condition(mbd_params_selector$nu)
  assertive::assert_is_if_condition(mbd_params_selector$q)
}
