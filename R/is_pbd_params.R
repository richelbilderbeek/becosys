#' Is object \code{x} an PBD parameter set,
#'   as can be created by \code{create_pbd_params}?
#' @param x the object to be determined of if
#'   its an PBD parameter set
#' @return TRUE if yes, else FALSE
#' @author Richel J.C. Bilderbeek
#' @export
is_pbd_params <- function(x) {
  all(becosys::get_pbd_param_names() %in% names(x))
}
