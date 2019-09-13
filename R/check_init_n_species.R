#' Check if the initial number of species equals one or two
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @export
check_init_n_species <- function(init_n_species) {
  assertive::assert_is_a_number(init_n_species)
  assertive::assert_all_are_whole_numbers(init_n_species)
  if (init_n_species != 1 && init_n_species != 2) {
    stop("'init_n_species' must be 1 or 2")
  }

}
