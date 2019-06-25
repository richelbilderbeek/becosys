#' Create the MBD parameters
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
create_mbd_params <- function(
  lambda,
  mu,
  nu,
  q
) {
  mbd_params <- list(
    lambda = lambda,
    mu = mu,
    nu = nu,
    q = q
  )
  check_mbd_params(mbd_params) # nolint becosys function
  mbd_params
}
