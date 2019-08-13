#' Check a PBD parameters set.
#'
#' Will \link{stop} if invalid, will do nothing otherwise
#' @inheritParams default_params_doc
#' @return nothing.
#' @author Richel J.C. Bilderbeek
#' @export
check_pbd_params <- function(
  pbd_params
) {
  argument_names <- c(
    "erg",
    "eri",
    "scr",
    "sirg",
    "siri"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(pbd_params)) {
      stop(
        "'", arg_name, "' must be an element of an 'pbd_params'.\n",
        "Tip: use 'create_pbd_params'"
      )
    }
  }

  if (pbd_params$erg < 0.0) {
    stop("'erg' must be positive")
  }
  if (pbd_params$eri < 0.0) {
    stop("'eri' must be positive")
  }
  if (pbd_params$scr < 0.0) {
    stop("'scr' must be positive")
  }
  if (pbd_params$sirg < 0.0) {
    stop("'sirg' must be positive")
  }
  if (pbd_params$siri < 0.0) {
    stop("'siri' must be positive")
  }
}
