#' Create a PBD parameter selector
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
create_pbd_params_selector <- function(
  erg = FALSE,
  eri = FALSE,
  scr = FALSE,
  sirg = FALSE,
  siri = FALSE
) {
  if (erg != TRUE && erg != FALSE) {
    stop("'erg' must be either TRUE or FALSE")
  }
  if (eri != TRUE && eri != FALSE) {
    stop("'eri' must be either TRUE or FALSE")
  }
  if (scr != TRUE && scr != FALSE) {
    stop("'scr' must be either TRUE or FALSE")
  }
  if (sirg != TRUE && sirg != FALSE) {
    stop("'sirg' must be either TRUE or FALSE")
  }
  if (siri != TRUE && siri != FALSE) {
    stop("'siri' must be either TRUE or FALSE")
  }
  list(erg = erg, eri = eri, scr = scr, sirg = sirg, siri = siri)
}
