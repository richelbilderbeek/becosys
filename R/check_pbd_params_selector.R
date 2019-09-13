#' @export
check_pbd_params_selector <- function(pbd_params_selector) {
  for (name in becosys::get_pbd_param_names()) {
    if (!name %in% names(pbd_params_selector)) {
      stop(
        "'pbd_params_selector' is missing element '", name, "'"
      )
    }
  }
  assertive::assert_is_if_condition(pbd_params_selector$erg)
  assertive::assert_is_if_condition(pbd_params_selector$eri)
  assertive::assert_is_if_condition(pbd_params_selector$scr)
  assertive::assert_is_if_condition(pbd_params_selector$sirg)
  assertive::assert_is_if_condition(pbd_params_selector$siri)
}
