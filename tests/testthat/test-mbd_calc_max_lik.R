context("mbd_calc_max_lik")

test_that("compare style", {

  # Optimize for q
  branching_times <- c(1, 2, 3)
  lambda <- 0.3
  mu <- 0.1
  nu <- 0.11
  q <- 0.15

  #############################
  # Classic interface
  #############################
  set.seed(10)
  out_classic <- mbd::mbd_ml(
    brts = branching_times,
    true_pars = c(lambda, mu, nu, q),
    optim_ids = c(FALSE, FALSE, FALSE, TRUE),
    start_pars = c(lambda, mu, nu, q),
    cond = 1,
    n_0 = 2,
    verbose = FALSE
  )

  #############################
  # More intuitive interface
  #############################
  set.seed(10)
  out_new <- mbd_calc_max_lik(
    branching_times = branching_times,
    init_param_values = create_mbd_params(
      lambda = lambda,
      mu = mu,
      nu = nu,
      q = q
    ),
    fixed_params = create_mbd_params_selector(
      lambda = TRUE, mu = TRUE, nu = TRUE
    ),
    opt_params = create_mbd_params_selector(q = TRUE),
    init_n_species = 2,
    conditioned_on = "non_extinction"
  )
  expect_equal(out_classic, out_new)
})

test_that("can estimate BD trees", {

  # Simulate a BD tree
  set.seed(12)
  lambda <- 0.3
  mu <- 0.1
  nu <- 0.0
  q <- 0.0
  mbd_params <- create_mbd_params(lambda = lambda, mu = mu, nu = nu, q = q)
  phylogeny <- mbd_sim(
    mbd_params = mbd_params,
    crown_age = 2,
    conditioned_on = "non_extinction"
  )$reconstructed_tree

  # Maximum likelihood of BD tree
  ml_est <- mbd_calc_max_lik(
    branching_times = ape::branching.times(phylogeny),
    init_param_values = mbd_params,
    opt_params = create_mbd_params_selector(lambda = TRUE, mu = TRUE),
    fixed_params = create_mbd_params_selector(nu = TRUE, q = TRUE),
    init_n_species = 2,
    conditioned_on = "non_extinction"
  )
  ml_est
  expect_true(ml_est$lambda >= 0)
})

test_that("abuse", {

  mbd_params <- create_mbd_params(0.1, 0.2, 0.3, 0.4)
  fixed_params <- create_mbd_params_selector(lambda = TRUE, mu = TRUE)
  opt_params <- create_mbd_params_selector(nu = TRUE, q = TRUE)

  expect_error(
    mbd_calc_max_lik(
      branching_times = "nonsense",
      init_param_values = mbd_params,
      fixed_params = fixed_params,
      opt_params = opt_params
    ),
    "'branching_times' must be numeric"
  )

  expect_error(
    mbd_calc_max_lik(
      branching_times = c(1, 2, -34.56),
      init_param_values = mbd_params,
      fixed_params = fixed_params,
      opt_params = opt_params
    ),
    "All 'branching_times' must be positive"
  )

  expect_error(
    mbd_calc_max_lik(
      branching_times = c(1, 2, 3),
      init_param_values = "nonsense",
      fixed_params = fixed_params,
      opt_params = opt_params
    ),
    paste0(
      "'init_param_values' must be an mbd_params, ",
      "as created by 'create_mbd_params'"
    )
  )
  expect_error(
    mbd_calc_max_lik(
      branching_times = c(1, 2, 3),
      init_param_values = mbd_params,
      fixed_params = "nonsense",
      opt_params = opt_params
    ),
    paste0("'fixed_params' must be an MBD parameter selector, ",
      "as created by 'create_mbd_params_selector'"
    )
  )
  expect_error(
    mbd_calc_max_lik(
      branching_times = c(1, 2, 3),
      init_param_values = mbd_params,
      fixed_params = fixed_params,
      opt_params = "nonsense"
    ),
    paste0("'opt_params' must be an MBD parameter selector, ",
      "as created by 'create_mbd_params_selector'"
    )
  )
  expect_error(
    mbd_calc_max_lik(
      branching_times = c(1, 2, 3),
      init_param_values = mbd_params,
      fixed_params = create_mbd_params_selector(TRUE, TRUE, TRUE, TRUE),
      opt_params = create_mbd_params_selector(TRUE, TRUE, TRUE, TRUE)
    ),
    paste0(
      "'fixed_params' and 'opt_params' together must select each ",
      "of the MBD parameters exactly once"
    )
  )
  expect_error(
    mbd_calc_max_lik(
      branching_times = c(1, 2, 3),
      init_param_values = mbd_params,
      fixed_params = fixed_params,
      opt_params = opt_params,
      init_n_species = 0
    ),
    "'init_n_species' must be 1 or 2"
  )
  expect_error(
    mbd_calc_max_lik(
      branching_times = c(1, 2, 3),
      init_param_values = mbd_params,
      fixed_params = fixed_params,
      opt_params = opt_params,
      conditioned_on = "nonsense"
    ),
    "'conditioned_on' must be either 'nothing' or 'non_extinction'"
  )
})
