context("bco_bd_sim")

test_that("same as mbd interface", {

  lambda <- 0.2 # sympatric speciation rate
  mu <- 0.15 # extinction rate
  nu <- 0.0 # multiple allopatric speciation trigger rate
  q <- 0.0 # single-lineage speciation probability
  crown_age <- 1

  set.seed(42)

  classic_sim <- mbd::mbd_sim(
    pars = c(lambda, mu, nu, q),
    n_0 = 2,
    age = crown_age,
    cond = 1 # non_extinction
  )

  set.seed(42)
  new_sim <- bco_bd_sim(
    bd_params = create_bd_params(ext_rate = lambda, spec_rate = mu),
    crown_age = crown_age,
    conditioned_on = "non_extinction"
  )

  expect_equal(names(classic_sim), names(new_sim))
  expect_equal(classic_sim$btrs, new_sim$btrs)
})
