#' Check if the stem age is valid
#'
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @export
check_stem_age <- function(stem_age) {
  assertive::assert_is_a_number(stem_age)
  assertive::assert_all_are_positive(stem_age)
}
