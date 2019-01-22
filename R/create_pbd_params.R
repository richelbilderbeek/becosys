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
  pbd_params <- list(
    erg = erg,
    eri = eri,
    scr = scr,
    sirg = sirg,
    siri = siri
  )
  check_pbd_params(pbd_params) # nolint becosys function
  pbd_params
}
