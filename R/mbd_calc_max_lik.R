#' Do an MBD maximum likelihood estimate.
#' @param branching_times the branching times of the phylogeny
#' @param init_param_values initial parameter values,
#'   as created with \code{create_mbd_params}
#' @param fixed_params the parameters that are fixed.
#'   This is a list of the four parameter names, and a
#'   boolean to indicate if it is fixed yes/no.
#'   Use \link{create_mbd_params_selector} to create
#'   such a list
#' @param opt_params the parameters that are optimized.
#'   This is a list of the four parameter names, and a
#'   boolean to indicate if it is optimized yes/no.
#'   Use \link{create_mbd_params_selector} to create
#'   such a list
#' @param init_n_species the number of species at the moment of the
#'   first branching time. Can be either one or two:
#'   \itemize{
#'     \item 1: stem
#'     \item 2: crown
#'   }
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
mbd_calc_max_lik <- function(
  branching_times,
  init_param_values,
  fixed_params,
  opt_params,
  init_n_species = 2,
  conditioned_on = "nothing"
) {
  assertive::assert_all_are_positive()
  check_mbd_params(init_param_values)
  check_mbd_params_selector(fixed_params)
  check_mbd_params_selector(opt_params)

  # Check if all parameters are either exclusively estimated or fixed
  lambda_once <- xor(fixed_params$lambda, opt_params$lambda)
  mu_once <- xor(fixed_params$mu, opt_params$mu)
  nu_once <- xor(fixed_params$nu, opt_params$nu)
  q_once <- xor(fixed_params$q, opt_params$q)
  if (!(lambda_once && mu_once && nu_once && q_once)) {
    stop(
      "'fixed_params' and 'opt_params' together must select each ",
      "of the MBD parameters exactly once"
    )
  }
  if (init_n_species != 1 && init_n_species != 2) {
    stop("'init_n_species' must be 1 or 2")
  }
  if (!conditioned_on %in% c("nothing", "non_extinction")) {
    stop("'conditioned_on' must be either 'nothing' or 'non_extinction'")
  }

  # Convert data
  conditioned_on_code <- 0
  if (conditioned_on == "non_extinction") {
    conditioned_on_code <- 1
  }

  # Create lambda, mu, nu, q parameters
  pars <- c(
    init_param_values$lambda,
    init_param_values$mu,
    init_param_values$nu,
    init_param_values$q
  )

  # 'IDs' is a misleading name
  is_optimizeds <- c(
    opt_params$lambda,
    opt_params$mu,
    opt_params$nu,
    opt_params$q
  )

  mbd::mbd_ml(
    brts = branching_times,
    start_pars = pars,
    true_pars = pars,
    optim_ids = is_optimizeds,
    cond = conditioned_on_code,
    n_0 = init_n_species,
    verbose = FALSE
  )
}
