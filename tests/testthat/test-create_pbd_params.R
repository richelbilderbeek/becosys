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
