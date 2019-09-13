#' Check a MBD parameters set.
#'
#' Will \link{stop} if invalid, will do nothing otherwise
#' @inheritParams default_params_doc
#' @return nothing.
#' @author Richel J.C. Bilderbeek
#' @export
check_mbd_params <- function(
  mbd_params
) {
  if (!is.list(mbd_params)) {
    stop(
      "'", deparse(substitute(mbd_params)), "' must be a list"
    )
  }
  if (mbd_params$lambda < 0.0) {
    stop("'lambda' must be positive")
  }
  if (mbd_params$mu < 0.0) {
    stop("'mu' must be positive")
  }
  if (mbd_params$nu < 0.0) {
    stop("'nu' must be positive")
  }
  if (mbd_params$q < 0.0) {
    stop("'q' must be positive")
  }
  if (mbd_params$q > 1.0) {
    stop("'q' must be a value from zero to and including one")
  }
}
