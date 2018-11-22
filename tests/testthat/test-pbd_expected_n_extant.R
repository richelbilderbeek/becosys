context("pbd_expected_n_extant")

test_that("minimal use", {

  testthat::expect_silent(
    pbd_expected_n_extant(
      crown_age = 1.0,
      scr = 1.0,
      sirg = 1.0,
      siri = 1.0,
      erg = 0.0,
      eri = 0.0
    )
  )
})

test_that("bigger crown age results in more lineages", {

  scr <- 1.0
  sirg <- 1.0
  siri <- 1.0
  erg <- 0.0
  eri <- 0.0

  n_lineages_young <- pbd_expected_n_extant(
    crown_age = 1.0,
    scr = scr,
    sirg = sirg,
    siri = siri,
    erg = erg,
    eri = eri
  )

  n_lineages_old <- pbd_expected_n_extant(
    crown_age = 2.0,
    scr = scr,
    sirg = sirg,
    siri = siri,
    erg = erg,
    eri = eri
  )
  testthat::expect_lt(n_lineages_young, n_lineages_old)

})

test_that("less extinction results in more lineages", {

  # Weak test, should use the average of multiple runs
  set.seed(42)
  crown_age <- 1.0
  scr <- 1.0
  sirg <- 1.0
  siri <- 1.0

  n_lineages_more_extinction <- pbd_expected_n_extant(
    crown_age = crown_age,
    scr = scr,
    sirg = sirg,
    siri = siri,
    erg = 2.0,
    eri = 2.0
  )

  n_lineages_less_extinction <- pbd_expected_n_extant(
    crown_age = crown_age,
    scr = scr,
    sirg = sirg,
    siri = siri,
    erg = 1.0,
    eri = 1.0
  )

  testthat::expect_lte(n_lineages_more_extinction, n_lineages_less_extinction)

})

test_that("more speciation initiation results in more lineages", {

  crown_age <- 1.0
  scr <- 1.0
  erg <- 0.0
  eri <- 0.0

  n_lineages_more_initiation <- pbd_expected_n_extant(
    crown_age = crown_age,
    scr = scr,
    sirg = 2.0,
    siri = 2.0,
    erg = erg,
    eri = eri
  )

  n_lineages_less_initiation <- pbd_expected_n_extant(
    crown_age = crown_age,
    scr = scr,
    sirg = 1.0,
    siri = 1.0,
    erg = erg,
    eri = eri
  )

  testthat::expect_lte(n_lineages_less_initiation, n_lineages_more_initiation)

})

test_that("more speciation completion results in more lineages", {

  # Weak test, should use the average of multiple runs
  set.seed(41)
  crown_age <- 1.0
  sirg <- 1.0
  siri <- 1.0
  erg <- 1.0
  eri <- 1.0

  n_lineages_less_completion <- pbd_expected_n_extant(
    crown_age = crown_age,
    scr = 1.0,
    sirg = sirg,
    siri = siri,
    erg = erg,
    eri = eri
  )

  n_lineages_more_completion <- pbd_expected_n_extant(
    crown_age = crown_age,
    scr = 2.0,
    sirg = sirg,
    siri = siri,
    erg = erg,
    eri = eri
  )

  testthat::expect_lte(n_lineages_less_completion, n_lineages_more_completion)

})

test_that("abuse", {

  testthat::expect_error(
    pbd_expected_n_extant(
      crown_age = -123456, # Error
      scr = 1.0,
      sirg = 1.0,
      siri = 1.0,
      erg = 0.0,
      eri = 0.0
    ),
    "'crown age' must be non-zero and positive"
  )

  testthat::expect_error(
    pbd_expected_n_extant(
      crown_age = 1.0,
      scr = -1.0, # Error
      sirg = 1.0,
      siri = 1.0,
      erg = 0.0,
      eri = 0.0
    ),
    "'scr' must be zero or positive"
  )

  testthat::expect_error(
    pbd_expected_n_extant(
      crown_age = 1.0,
      scr = 1.0,
      sirg = -1.0, # Error
      siri = 1.0,
      erg = 0.0,
      eri = 0.0
    ),
    "'sirg' must be zero or positive"
  )

  testthat::expect_error(
    pbd_expected_n_extant(
      crown_age = 1.0,
      scr = 1.0,
      sirg = 1.0,
      siri = -1.0, # Error
      erg = 0.0,
      eri = 0.0
    ),
    "'siri' must be zero or positive"
  )

  testthat::expect_error(
    pbd_expected_n_extant(
      crown_age = 1.0,
      scr = 1.0,
      sirg = 1.0,
      siri = 1.0,
      erg = -1.0,
      eri = 0.0
    ),
    "'erg' must be zero or positive"
  )

  testthat::expect_error(
    pbd_expected_n_extant(
      crown_age = 1.0,
      scr = 1.0,
      sirg = 1.0,
      siri = 1.0,
      erg = 0.0,
      eri = -1.0
    ),
    "'eri' must be zero or positive"
  )

})
