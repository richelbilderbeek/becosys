#' Get the expected number of good and incipient lineages from a protracted
#' pure birth model
#' @param sirg The speciation initiation rate from a good species
#' @param scr The speciation completion rate
#' @param siri The speciation initiation rate from an incipient species
#' @param age The age
#' @param ng The number of good species at t=0
#' @param ni The number of incipient species at t=0
#' @export
#' @author Joris Damhuis
pbd_get_exp_n_lineages_pb <- function(
  sirg,
  scr,
  siri,
  age,
  ng,
  ni
) {
  if (sirg <= 0.0) {
    stop("'sirg' must be non-zero and positive")
  }
  if (scr < 0.0) {
    stop("'scr' must be non-zero and positive")
  }
  if (siri <= 0.0) {
    stop("'siri' must be non-zero and positive")
  }
  if (age <= 0.0) {
    stop("'age' must be non-zero and positive")
  }
  if (ng < 0.0) {
    stop("'ng' must be zero or positive")
  } else if (ng == 0.0) {
    if (ni < 0.0) {
      stop("'ni' must be zero or positive")
    } else if (ni == 0.0) {
      stop("One of 'ng' and 'ni' must be non-zero")
    }
  } else {
    if (ni < 0.0) {
      stop("'ni' must be zero or positive")
    } else if (ni > 0.0) {
      stop("Only one of 'ng' and 'ni' can be non-zero")
    }
  }

  if (ng > 0.0) {
    temp_a1 <- 1 - (siri / scr)
    temp_a2 <- temp_a1 ^ 2 + (4 * (sirg / scr))
    temp_a <- sqrt(temp_a2)
    temp_b <- temp_a + 1 - (siri / scr)
    temp_c <- exp(0.5 * (siri + (temp_a - 1) * scr) * age)
    temp_d <- exp(0.5 * (siri - (temp_a + 1) * scr) * age)
    exp_ng <- (ng / (2 * temp_a)) * (temp_b * temp_c + (temp_a - 1 +
              (siri / scr)) * temp_d)
    exp_ni <- (ng / temp_a) * (sirg / scr) * (temp_c - temp_d)
  } else {
    exp_ng <- (ni / (1 + (sirg / scr))) * (exp(sirg * age) - exp(- scr * age))
    exp_ni <- (ni / (1 + (scr / sirg))) * exp(sirg * age) +
              (ni / (1 + (sirg / scr))) * exp(- scr * age)
  }

  out <- list(exp_ng = exp_ng, exp_ni = exp_ni)
  return(out)

}
