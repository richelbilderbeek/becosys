#' Do an PBD maximum likelihood estimate.
#' @param branching_times the branching times of the phylogeny
#' @param init_param_values initial parameter values,
#'   as created with \code{create_pbd_params}
#' @param fixed_params the parameters that are fixed.
#'   This is a list of the five parameter names, and a
#'   boolean to indicate if it is fixed yes/no.
#'   Use \link{create_pbd_params_selector} to create
#'   such a list
#' @param opt_params the parameters that are estimated.
#'   This is a list of the five parameter names, and a
#'   boolean to indicate if it is optimized yes/no.
#'   Use \link{create_pbd_params_selector} to create
#'   such a list
#' @param init_n_species the number of species at the moment of the
#'   first branching time. Can be either one or two:
#'   \itemize{
#'     \item 1: stem
#'     \item 2: crown
#'   }
#' @param n_missing_species the number of species missing
#' @param conditioned_on on what must be the likelihood estimation be
#'   conditioned:
#'   \itemize{
#'     \item \code{"nothing"}: the branch lengths were obtained from
#'       a random phylogeny that could also have gone extinct
#'     \item \code{"non_extinction"}: the branch lengths were obtained from
#'       a phylogeny conditioned on non-extinction
#'   }
#' @author Richel J.C. Bilderbeek
#' @export
pbd_calc_max_lik <- function(
  branching_times,
  init_param_values,
  fixed_params,
  opt_params,
  init_n_species = 2,
  n_missing_species = 0,
  conditioned_on = "nothing"
) {
  assertive::assert_all_are_positive(branching_times)
  assertive::assert_is_a_number(init_n_species)
  assertive::assert_is_a_number(n_missing_species)

  check_pbd_params(init_param_values)
  check_pbd_params_selector(fixed_params)
  check_pbd_params_selector(opt_params)

  erg_once <- xor(fixed_params$erg, opt_params$erg)
  eri_once <- xor(fixed_params$eri, opt_params$eri)
  scr_once <- xor(fixed_params$scr, opt_params$scr)
  sirg_once <- xor(fixed_params$sirg, opt_params$sirg)
  siri_once <- xor(fixed_params$siri, opt_params$siri)
  if (!(erg_once && eri_once && scr_once && sirg_once && siri_once)) {
    stop(
      "'fixed_params' and 'opt_params' together must select each ",
      "of the PBD parameters exactly once"
    )
  }
  if (init_n_species != 1 && init_n_species != 2) {
    stop("'init_n_species' must be 1 or 2")
  }
  if (n_missing_species < 0) {
    stop("'n_missing_species' must be positive")
  }
  if (!conditioned_on %in% c("nothing", "non_extinction")) {
    stop("'conditioned_on' must be either 'nothing' or 'non_extinction'")
  }
  if (init_param_values$sirg != init_param_values$siri) {
    stop(
      "Can only optimize for equal speciation rates ",
      "in good and incipient species"
    )
  }

  # Convert data
  idparsopt <- NULL
  idparsfix <- NULL
  # id == 1 corresponds to b (speciation-initiation rate)
  if (opt_params$sirg) {
    idparsopt <- c(idparsopt, 1)
  } else {
    idparsfix <- c(idparsfix, 1)
  }
  # id == 2 corresponds to mu_1 (extinction rate of good species)
  if (opt_params$erg) {
    idparsopt <- c(idparsopt, 2)
  } else {
    idparsfix <- c(idparsfix, 2)
  }
  # id == 3 corresponds to la_1 (speciation-completion rate)
  if (opt_params$scr) {
    idparsopt <- c(idparsopt, 3)
  } else {
    idparsfix <- c(idparsfix, 3)
  }
  # id == 4 corresponds to mu_2 (extinction rate of incipient species)
  if (opt_params$eri) {
    idparsopt <- c(idparsopt, 4)
  } else {
    idparsfix <- c(idparsfix, 4)
  }

  # The initial values of the parameters that must be optimized:
  # initparsopt[1] = b (= la_1 in ER2012) = speciation initiation rate
  # initparsopt[2] = mu_1 (= mu_g in ER2012) = extinction rate of good species
  # initparsopt[3] = la_1 (= la_2 in ER2012) = speciation completion rate
  # initparsopt[4] = mu_2 (= mu_i in ER2012) = extinction rate
  #  of incipient species
  values <- c(
    init_param_values$sirg,
    init_param_values$erg,
    init_param_values$scr,
    init_param_values$eri
  )
  initparsopt <- values[idparsopt]
  parsfix <- values[idparsfix]

  conditioned_on_code <- 0
  if (conditioned_on == "non_extinction") {
    conditioned_on_code <- 1
  }

  # Must have length 4, as PBD::pbd_ML assumes sirg == siri
  testit::assert(length(idparsopt) + length(idparsfix) == 4)

  PBD::pbd_ML(
    brts = branching_times,
    initparsopt = initparsopt,
    idparsopt = idparsopt,
    idparsfix = idparsfix,
    parsfix = parsfix,
    missnumspec = n_missing_species,
    cond = conditioned_on_code,
    soc = init_n_species,
    exteq = FALSE,
    verbose = FALSE
  )
}
