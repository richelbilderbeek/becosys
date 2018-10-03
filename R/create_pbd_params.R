#' Create the PBD parameters
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
create_pbd_params <- function(
    erg,
    eri,
    scr,
    sirg,
    siri
) {
  if (erg < 0.0) {
    stop("'erg' must be positive")
  }
  if (eri < 0.0) {
    stop("'eri' must be positive")
  }
  if (scr < 0.0) {
    stop("'scr' must be positive")
  }
  if (sirg < 0.0) {
    stop("'sirg' must be positive")
  }
  if (siri < 0.0) {
    stop("'siri' must be positive")
  }
  list(
    erg = erg,
    eri = eri,
    scr = scr,
    sirg = sirg,
    siri = siri
  )
}
