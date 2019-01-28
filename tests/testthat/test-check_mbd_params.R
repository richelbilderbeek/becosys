context("test-check_mbd_params")

test_that("use", {

  good_mbd_params <- create_mbd_params(
    lambda = 1.0,
    mu = 0.1,
    nu = 0.1,
    q = 0.1
  )
  mbd_params <- good_mbd_params
  expect_silent(check_mbd_params(mbd_params))

  mbd_params <- good_mbd_params
  mbd_params$lambda <- -12.34
  expect_error(check_mbd_params(mbd_params),
    "'lambda' must be positive"
  )

  mbd_params <- good_mbd_params
  mbd_params$mu <- -12.34
  expect_error(check_mbd_params(mbd_params),
    "'mu' must be positive"
  )

  mbd_params <- good_mbd_params
  mbd_params$nu <- -12.34
  expect_error(check_mbd_params(mbd_params),
    "'nu' must be positive"
  )

  mbd_params <- good_mbd_params
  mbd_params$q <- -12.34
  expect_error(check_mbd_params(mbd_params),
    "'q' must be positive"
  )

  mbd_params <- good_mbd_params
  mbd_params$q <- 12.34
  expect_error(check_mbd_params(mbd_params),
    "'q' must be a value from zero to and including one"
  )
})
