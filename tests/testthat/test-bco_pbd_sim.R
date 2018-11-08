context("pbd_sim")

test_that("same as classic interface", {

  erg <- 0.1
  eri <- 0.2
  scr <- 0.3
  sirg <- 0.4
  siri <- 0.5
  # pars[1] corresponds to b_1, the speciation-initiation rate of good species
  # pars[2] corresponds to la_1, the speciation-completion rate
  # pars[3] corresponds to b_2, the speciation-initiation
  #  rate of incipient species
  # pars[4] corresponds to mu_1, the extinction rate of good species
  # pars[5] corresponds to mu_2, the extinction rate of incipient species
  sim_pars <- c(sirg, scr, siri, erg, eri)

  crown_age <- 1
  set.seed(42)
  classic_sim <- PBD::pbd_sim(
    pars = sim_pars,
    soc = 2,
    age = crown_age
  )

  set.seed(42)
  new_sim <- bco_pbd_sim(
    pbd_params = create_pbd_params(
      erg = erg, eri = eri, scr = scr, sirg = sirg, siri = siri
    ),
    crown_age = crown_age
  )
  expect_equal(names(classic_sim), names(new_sim))
  expect_equal(classic_sim$tree, new_sim$tree)
})

test_that("abuse", {

  pbd_params <- create_pbd_params(0.1, 0.2, 0.3, 0.4, 0.5)
  crown_age <- 15

  expect_error(
    bco_pbd_sim(
      pbd_params = "nonsense",
      crown_age = crown_age
    ),
    "'pbd_params' must be a valid PBD parameter set"
  )
  expect_error(
    bco_pbd_sim(
      pbd_params = pbd_params,
      crown_age = -12.34
    ),
    "'crown_age' must be positive"
  )
  expect_error(
    bco_pbd_sim(
      pbd_params = pbd_params,
      stem_age = -12.34
    ),
    "'stem_age' must be positive"
  )
  expect_error(
    bco_pbd_sim(
      pbd_params = pbd_params,
      crown_age = NA,
      stem_age = NA
    ),
    "'crown_age' or 'stem_age' must be set"
  )
  expect_error(
    bco_pbd_sim(
      pbd_params = pbd_params,
      crown_age = 1.2,
      stem_age = 2.3
    ),
    "'crown_age' or 'stem_age' must be set exclusively"
  )
})
