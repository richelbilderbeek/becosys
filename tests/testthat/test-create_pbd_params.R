context("create_pbd_params")

test_that("use", {

  erg <- 0.1
  eri <- 0.2
  scr <- 0.3
  sirg <- 0.4
  siri <- 0.5
  params <- create_pbd_params(
    erg = erg, eri = eri, scr = scr, sirg = sirg, siri = siri
  )
  expect_equal(erg, params$erg)
  expect_equal(eri, params$eri)
  expect_equal(scr, params$scr)
  expect_equal(sirg, params$sirg)
  expect_equal(siri, params$siri)
})

test_that("abuse", {

  expect_error(
    create_pbd_params(
      erg = -12.34,
      eri = 1.0,
      scr = 1.0,
      sirg = 1.0,
      siri = 1.0
    ),
    "'erg' must be positive"
  )
  expect_error(
    create_pbd_params(
      erg = 1.0,
      eri = -12.34,
      scr = 1.0,
      sirg = 1.0,
      siri = 1.0
    ),
    "'eri' must be positive"
  )
  expect_error(
    create_pbd_params(
      erg = 1.0,
      eri = 1.0,
      scr = -12.34,
      sirg = 1.0,
      siri = 1.0
    ),
    "'scr' must be positive"
  )
  expect_error(
    create_pbd_params(
      erg = 1.0,
      eri = 1.0,
      scr = 1.0,
      sirg = -12.34,
      siri = 1.0
    ),
    "'sirg' must be positive"
  )
  expect_error(
    create_pbd_params(
      erg = 1.0,
      eri = 1.0,
      scr = 1.0,
      sirg = 1.0,
      siri = -12.34
    ),
    "'siri' must be positive"
  )
})
