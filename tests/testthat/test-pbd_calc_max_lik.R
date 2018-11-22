context("pbd_calc_max_lik")

test_that("compare style", {

  # Goal: optimize the SCR
  branching_times <- c(1, 2, 3, 4)
  erg <- 0.1
  eri <- 0.2
  scr <- 0.3
  sirg <- 0.4
  siri <- sirg
  #############################
  # Classic interface
  #############################

  set.seed(10)
  out_classic <- PBD::pbd_ML(
    brts = branching_times,
    idparsopt = 3, # SCR
    initparsopt = scr,
    idparsfix = c(1, 2, 4),
    parsfix = c(sirg, erg, eri),
    exteq = FALSE,
    verbose = FALSE
  )

  #############################
  # More intuitive interface
  #############################
  set.seed(10)
  out_new <- pbd_calc_max_lik(
    branching_times = branching_times,
    init_param_values = create_pbd_params(
      erg = erg,
      eri = eri,
      scr = scr,
      sirg = sirg,
      siri = siri
    ),
    fixed_params = create_pbd_params_selector(
      erg = TRUE, eri = TRUE, sirg = TRUE, siri = TRUE
    ),
    opt_params = create_pbd_params_selector(
      scr = TRUE
    ),
    conditioned_on = "non_extinction"
  )
  expect_equal(out_classic, out_new)
})

test_that("can estimate BD trees", {

  skip("Cannot do ML estimation on BD trees, Issue #4, #4")
  # Simulate a BD tree
  set.seed(12)
  laPBDa <- 0.3
  mu <- 0.1
  nu <- 0.0
  q <- 0.0
  pbd_params <- create_pbd_params(laPBDa = laPBDa, mu = mu, nu = nu, q = q)
  phylogeny <- pbd_sim(
    pbd_params = pbd_params,
    crown_age = 2,
    conditioned_on = "non_extinction"
  )$reconstructed_tree

  # Maximum likelihood of BD tree
  ml_est <- pbd_calc_max_lik(
    branching_times = ape::branching.times(phylogeny),
    init_param_values = pbd_params,
    opt_params = create_pbd_params_selector(laPBDa = TRUE, mu = TRUE),
    fixed_params = create_pbd_params_selector(nu = TRUE, q = TRUE),
    init_n_species = 2,
    n_missing_species = 0,
    conditioned_on = "non_extinction"
  )
  ml_est
  expect_true(ml_est$laPBDa >= 0)
})

test_that("abuse", {

  pbd_params <- create_pbd_params(
    erg = 0.1,
    eri = 0.2,
    scr = 0.3,
    sirg = 0.4,
    siri = 0.5
  )
  fixed_params <- create_pbd_params_selector(
    erg = TRUE, eri = TRUE, sirg = TRUE, siri = TRUE
  )
  opt_params <- create_pbd_params_selector(scr = TRUE)


  expect_error(
    pbd_calc_max_lik(
      branching_times = "nonsense",
      init_param_values = pbd_params,
      fixed_params = fixed_params,
      opt_params = opt_params
    ),
    "'branching_times' must be numeric"
  )

  expect_error(
    pbd_calc_max_lik(
      branching_times = c(1, 2, -34.56),
      init_param_values = pbd_params,
      fixed_params = fixed_params,
      opt_params = opt_params
    ),
    "All 'branching_times' must be positive"
  )

  expect_error(
    pbd_calc_max_lik(
      branching_times = c(1, 2, 3),
      init_param_values = "nonsense",
      fixed_params = fixed_params,
      opt_params = opt_params
    ),
    paste0(
      "'init_param_values' must be an pbd_params, ",
      "as created by 'create_pbd_params'"
    )
  )
  expect_error(
    pbd_calc_max_lik(
      branching_times = c(1, 2, 3),
      init_param_values = pbd_params,
      fixed_params = "nonsense",
      opt_params = opt_params
    ),
    paste0("'fixed_params' must be a PBD parameter selector, ",
      "as created by 'create_pbd_params_selector'"
    )
  )
  expect_error(
    pbd_calc_max_lik(
      branching_times = c(1, 2, 3),
      init_param_values = pbd_params,
      fixed_params = fixed_params,
      opt_params = "nonsense"
    ),
    paste0("'opt_params' must be a PBD parameter selector, ",
      "as created by 'create_pbd_params_selector'"
    )
  )
  expect_error(
    pbd_calc_max_lik(
      branching_times = c(1, 2, 3),
      init_param_values = pbd_params,
      fixed_params = create_pbd_params_selector(TRUE, TRUE, TRUE, TRUE),
      opt_params = create_pbd_params_selector(TRUE, TRUE, TRUE, TRUE)
    ),
    paste0(
      "'fixed_params' and 'opt_params' together must select each ",
      "of the PBD parameters exactly once"
    )
  )
  expect_error(
    pbd_calc_max_lik(
      branching_times = c(1, 2, 3),
      init_param_values = pbd_params,
      fixed_params = fixed_params,
      opt_params = opt_params,
      init_n_species = 0
    ),
    "'init_n_species' must be 1 or 2"
  )
  expect_error(
    pbd_calc_max_lik(
      branching_times = c(1, 2, 3),
      init_param_values = pbd_params,
      fixed_params = fixed_params,
      opt_params = opt_params,
      n_missing_species = -12345
    ),
    "'n_missing_species' must be positive"
  )
  expect_error(
    pbd_calc_max_lik(
      branching_times = c(1, 2, 3),
      init_param_values = pbd_params,
      fixed_params = fixed_params,
      opt_params = opt_params,
      conditioned_on = "nonsense"
    ),
    "'conditioned_on' must be either 'nothing' or 'non_extinction'"
  )
})