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
  assertive::assert_is_if_condition(erg)
  assertive::assert_is_if_condition(eri)
  assertive::assert_is_if_condition(scr)
  assertive::assert_is_if_condition(sirg)
  assertive::assert_is_if_condition(siri)
  list(erg = erg, eri = eri, scr = scr, sirg = sirg, siri = siri)
}
