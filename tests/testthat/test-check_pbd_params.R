context("test-check_pbd_params")

test_that("use", {

  good_pbd_params <- create_pbd_params(
    erg = 0.1,
    eri = 1.0,
    scr = 1.0,
    sirg = 1.0,
    siri = 1.0
  )
  pbd_params <- good_pbd_params
  expect_silent(check_pbd_params(pbd_params))

  pbd_params <- good_pbd_params
  pbd_params$erg <- -12.34
  expect_error(check_pbd_params(pbd_params),
    "'erg' must be positive"
  )

  pbd_params <- good_pbd_params
  pbd_params$eri <- -12.34
  expect_error(check_pbd_params(pbd_params),
    "'eri' must be positive"
  )

  pbd_params <- good_pbd_params
  pbd_params$scr <- -12.34
  expect_error(check_pbd_params(pbd_params),
    "'scr' must be positive"
  )

  pbd_params <- good_pbd_params
  pbd_params$sirg <- -12.34
  expect_error(check_pbd_params(pbd_params),
    "'sirg' must be positive"
  )

  pbd_params <- good_pbd_params
  pbd_params$siri <- -12.34
  expect_error(check_pbd_params(pbd_params),
    "'siri' must be positive"
  )
})
