#' Check the result of a PBD simulation.
#'
#' Will \link{stop} if invalid, will do nothing otherwise
#' @inheritParams default_params_doc
#' @return nothing.
#' @author Richel J.C. Bilderbeek
#' @export
check_pbd_sim_out <- function(
  pbd_sim_out
) {
  argument_names <- c(
    "tree",
    "stree_random",
    "stree_oldest",
    "stree_youngest",
    "L",
    "sL_random",
    "sL_oldest",
    "sL_youngest",
    "igtree.extinct",
    "igtree.extant",
    "recontree",
    "reconL",
    "L0"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(pbd_sim_out)) {
      stop(
        "'", arg_name, "' must be an element of an 'pbd_sim_out'.\n"
      )
    }
  }

  # "stree_oldest",
  # "stree_youngest",

names(pbd_sim_out)
}
