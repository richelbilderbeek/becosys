context("mbd_sim_checked")

test_that("same as mbd interface", {

  lambda <- 0.2 # sympatric speciation rate
  mu <- 0.15 # extinction rate
  nu <- 2.0 # multiple allopatric speciation trigger rate
  q <- 0.1 # single-lineage speciation probability
  crown_age <- 1

  set.seed(42)

  classic_sim <- mbd::mbd_sim_checked(
    mbd_params = mbd::create_mbd_params(lambda = lambda, mu = mu, nu = nu, q = q),
    crown_age = crown_age,
    conditioned_on = "non_extinction"
  )

  set.seed(42)
  new_sim <- bco_mbd_sim(
    mbd_params = mbd::create_mbd_params(lambda = lambda, mu = mu, nu = nu, q = q),
    crown_age = crown_age,
    conditioned_on = "non_extinction"
  )

  expect_equal(names(classic_sim), names(new_sim))
  expect_equal(classic_sim$btrs, new_sim$btrs)
})
