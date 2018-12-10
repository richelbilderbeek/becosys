#' Is object \code{x} an BD parameter set,
#'   as can be created by \code{create_bd_params}?
#' @param x the object to be determined of if
#'   its an BD parameter set
#' @return TRUE if yes, else FALSE
#' @author Richel J.C. Bilderbeek
is_bd_params <- function(x) {
  all(becosys::get_bd_param_names() %in% names(x))
}
