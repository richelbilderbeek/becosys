context("create_mbd_params")

test_that("use", {

  lambda <- 0.1
  mu <- 0.2
  nu <- 0.3
  q <- 0.4
  params <- create_mbd_params(
    lambda = lambda,
    mu = mu,
    nu = nu,
    q = q
  )
  expect_equal(lambda, params$lambda)
  expect_equal(mu, params$mu)
  expect_equal(nu, params$nu)
  expect_equal(q, params$q)
})

test_that("abuse", {

  # Just check one, others are checked by check_mbd_params
  expect_error(
    create_mbd_params(
      lambda = -12.34,
      mu = 1.0,
      nu = 1.0,
      q = 1.0
    ),
    "'lambda' must be positive"
  )
})
