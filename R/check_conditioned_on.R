#' Check if the conditioning is a proper descriptor
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @export
check_conditioned_on <- function(conditioned_on) {
  if (length(conditioned_on) != 1) {
    stop("'conditioned_on' must be one string")
  }
  if (!conditioned_on %in% c("nothing", "non_extinction")) {
    stop("'conditioned_on' must be either 'nothing' or 'non_extinction'")
  }
}
