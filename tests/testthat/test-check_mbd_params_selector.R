test_that("use", {
  expect_silent(check_mbd_params_selector(x = create_mbd_params_selector()))
  expect_error(check_mbd_params_selector(x = "nonsense"))
  expect_error(check_mbd_params_selector(NULL))
  expect_error(check_mbd_params_selector(NA))
  expect_error(
    check_mbd_params_selector(
      list(
        lambda = "nonsense",
        mu = TRUE,
        nu = TRUE,
        q = TRUE
      )
    )
  )
  expect_error(
    check_mbd_params_selector(
      list(
        lambda = TRUE,
        mu = "nonsense",
        nu = TRUE,
        q = TRUE
      )
    )
  )
  expect_error(
    check_mbd_params_selector(
      list(
        lambda = TRUE,
        mu = TRUE,
        nu = "nonsense",
        q = TRUE
      )
    )
  )
  expect_error(
    check_mbd_params_selector(
      list(
        lambda = TRUE,
        mu = TRUE,
        nu = TRUE,
        q = "nonsense"
      )
    )
  )
})
