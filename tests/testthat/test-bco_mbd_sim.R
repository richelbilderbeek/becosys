context("mbd_sim_checked")

test_that("same as mbd interface", {

  set.seed(42)
  classic_sim <- mbd_sim_checked(
    mbd_params = create_mbd_params(lambda = lambda, mu = mu, nu = nu, q = q),
    crown_age = crown_age,
    conditioned_on = "non_extinction"
  )

  set.seed(42)
  new_sim <- bco_mbd_sim(
    mbd_params = create_mbd_params(lambda = lambda, mu = mu, nu = nu, q = q),
    crown_age = crown_age,
    conditioned_on = "non_extinction"
  )

  expect_equal(names(classic_sim), names(new_sim))
  expect_equal(classic_sim$btrs, new_sim$btrs)
})
