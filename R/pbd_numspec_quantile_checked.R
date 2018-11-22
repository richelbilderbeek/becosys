#' Checked version of \code{\link[PBD]{pbd_numspec_quantile}}
#' @note uses equation 6 from \insertCite{etienne2014;textual}{raket}
#' @inheritParams default_params_doc
#' @references \insertAllCited{}
#' @export
pbd_numspec_quantile_checked <- function(
  erg,
  eri,
  scr,
  sir,
  stem_age = NULL,
  crown_age = NULL,
  quantile
) {
  if (erg < 0.0) {
    stop("'erg' must be positive")
  }
  if (eri < 0.0) {
    stop("'eri' must be positive")
  }
  if (sir < 0.0) {
    stop("'sir' must be positive")
  }
  if (scr < 0.0) {
    stop("'scr' must be positive")
  }
  if (is.null(crown_age)) {
    if (is.null(stem_age)) {
      stop(
        "At least one of 'crown_age' or 'stem_age' ",
        "must be non-zero and positive"
      )
    } else if (stem_age <= 0.0) {
      stop("'stem_age' must be non-zero and positive")
    }
  } else if (crown_age <= 0.0) {
    stop("'crown_age' must be non-zero and positive")
  } else if (crown_age > 0.0) {
    if (!is.null(stem_age)) {
      stop("Must set either 'crown_age' or 'stem_age'")
    }
  }
  if (quantile < 0.0 || quantile > 1.0) {
    stop("'quantile' must be a value between, and including, zero to one")
  }

  # age
  age <- stem_age
  if (is.null(age)) age <- crown_age
  testit::assert(!is.null(age))
  testit::assert(age > 0.0)

  # soc
  soc <- 1
  if (!is.null(crown_age)) soc <- 2
  testit::assert(!is.null(soc))
  testit::assert(soc == 1 || soc == 2)

  PBD::pbd_numspec_quantile(
    pars = c(sir, erg, scr, eri),
    age = age,
    soc = soc,
    quantile = quantile
  )
}
