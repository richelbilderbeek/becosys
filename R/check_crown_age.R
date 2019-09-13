#' Check if the crown age is valid
#'
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @export
check_crown_age <- function(crown_age) {
  assertive::assert_is_a_number(crown_age)
  assertive::assert_all_are_positive(crown_age)
}
