#' Check if the two \code{pbd_params_selector}s
#' together select each PBD parameter once.
#'
#' Will \link{stop} if not
#' @param fixed_params first \code{pbd_params_selector}
#' @param opt_params second \code{pbd_params_selector}
#' @export
check_each_pbd_param_selected_once <- function( # nolint long function name indeed, which is fine for an internal function
  fixed_params,
  opt_params
) {
  becosys::check_pbd_params_selector(fixed_params)
  becosys::check_pbd_params_selector(opt_params)

  erg_once <- xor(fixed_params$erg, opt_params$erg)
  eri_once <- xor(fixed_params$eri, opt_params$eri)
  scr_once <- xor(fixed_params$scr, opt_params$scr)
  sirg_once <- xor(fixed_params$sirg, opt_params$sirg)
  siri_once <- xor(fixed_params$siri, opt_params$siri)
  if (!all(c(erg_once, eri_once, scr_once, sirg_once, siri_once))) {
    stop(
      "'fixed_params' and 'opt_params' together must select each ",
      "of the PBD parameters exactly once"
    )
  }
}
