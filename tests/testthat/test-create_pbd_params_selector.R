context("create_pbd_params_selector")

test_that("use, by default all are false", {
  selector <- create_pbd_params_selector()
  expect_false(selector$erg)
  expect_false(selector$eri)
  expect_false(selector$scr)
  expect_false(selector$sirg)
  expect_false(selector$siri)
})

test_that("use, all true", {
  selector <- create_pbd_params_selector(
    erg = TRUE,
    eri = TRUE,
    scr = TRUE,
    sirg = TRUE,
    siri = TRUE
  )
  expect_true(selector$erg)
  expect_true(selector$eri)
  expect_true(selector$scr)
  expect_true(selector$sirg)
  expect_true(selector$siri)
})

test_that("abuse", {
  expect_error(
    create_pbd_params_selector(
      erg = "nonsense",
      eri = TRUE,
      scr = TRUE,
      sirg = TRUE,
      siri = TRUE
    ),
    "erg is not of class 'logical'"
  )
  expect_error(
    create_pbd_params_selector(
      erg = TRUE,
      eri = "nonsense",
      scr = TRUE,
      sirg = TRUE,
      siri = TRUE
    ),
    "eri is not of class 'logical'"
  )
  expect_error(
    create_pbd_params_selector(
      erg = TRUE,
      eri = TRUE,
      scr = "nonsense",
      sirg = TRUE,
      siri = TRUE
    ),
    "scr is not of class 'logical'"
  )
  expect_error(
    create_pbd_params_selector(
      erg = TRUE,
      eri = TRUE,
      scr = TRUE,
      sirg = "nonsense",
      siri = TRUE
    ),
    "sirg is not of class 'logical'"
  )
  expect_error(
    create_pbd_params_selector(
      erg = TRUE,
      eri = TRUE,
      scr = TRUE,
      sirg = TRUE,
      siri = "nonsense"
    ),
    "siri is not of class 'logical'"
  )
})
