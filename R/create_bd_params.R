#' Create the BD parameters
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
create_bd_params <- function(
  ext_rate,
  spec_rate
) {
  if (ext_rate < 0.0) {
    stop("'ext_rate' must be positive")
  }
  if (spec_rate < 0.0) {
    stop("'spec_rate' must be positive")
  }
  list(
    ext_rate = ext_rate,
    spec_rate = spec_rate
  )
}
