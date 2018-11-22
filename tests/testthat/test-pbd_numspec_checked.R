context("pbd_numspec_checked")

test_that("pbd_numspec_mean_checked single-value use", {

  n <- pbd_numspec_mean_checked(
    ergs = 0.1,
    eris = 0.2,
    scrs = 0.3,
    sirs = 0.4,
    crown_ages = 0.5
  )
  expect_equal(n, 1.029111871)

  n <- pbd_numspec_mean_checked(
    ergs = 0.2,
    eris = 0.4,
    scrs = 0.6,
    sirs = 0.8,
    crown_ages = 1.0
  )
  expect_equal(n, 1.440746567)
})

test_that("pbd_numspec_mean_checked vectorized use", {

  ns <- pbd_numspec_mean_checked(
    ergs = c(0.1, 0.2),
    eris = c(0.2, 0.4),
    scrs = c(0.3, 0.6),
    sirs = c(0.4, 0.8),
    crown_ages = c(0.5, 1.0)
  )
  expect_equal(ns, c(1.029111871, 1.440746567))
})

test_that("pbd_numspec_mean_checked single-value abuse", {

  expect_silent(
    pbd_numspec_mean_checked(
      ergs = 1.0,
      eris = 1.0,
      scrs = 1.0,
      sirs = 1.0,
      crown_ages = 1.0
    )
  )

  expect_error(
    pbd_numspec_mean_checked(
      ergs = -123.456,
      eris = 1.0,
      scrs = 1.0,
      sirs = 1.0,
      crown_ages = 1.0
    ),
    "All 'ergs' must be positive"
  )

  expect_error(
    pbd_numspec_mean_checked(
      ergs = 1.0,
      eris = -123.456,
      scrs = 1.0,
      sirs = 1.0,
      crown_ages = 1.0
    ),
    "All 'eris' must be positive"
  )

  expect_error(
    pbd_numspec_mean_checked(
      ergs = 1.0,
      eris = 1.0,
      scrs = -123.456,
      sirs = 1.0,
      crown_ages = 1.0
    ),
    "All 'scrs' must be positive"
  )

  expect_error(
    pbd_numspec_mean_checked(
      ergs = 1.0,
      eris = 1.0,
      scrs = 1.0,
      sirs = -123.456,
      crown_ages = 1.0
    ),
    "All 'sirs' must be positive"
  )

  expect_error(
    pbd_numspec_mean_checked(
      ergs = 1.0,
      eris = 1.0,
      scrs = 1.0,
      sirs = 1.0,
      crown_ages = -123.456
    ),
    "All 'crown_ages' must be positive"
  )
})

test_that("pbd_numspec_mean_checked test boundary", {

  expect_silent(
    pbd_numspec_mean_checked(
      ergs = 0.0,
      eris = 1.0,
      scrs = 1.0,
      sirs = 1.0,
      crown_ages = 1.0
    )
  )

  expect_silent(
    pbd_numspec_mean_checked(
      ergs = 1.0,
      eris = 0.0,
      scrs = 1.0,
      sirs = 1.0,
      crown_ages = 1.0
    )
  )

  expect_silent(
    pbd_numspec_mean_checked(
      ergs = 1.0,
      eris = 1.0,
      scrs = 0.0,
      sirs = 1.0,
      crown_ages = 1.0
    )
  )

  expect_silent(
    pbd_numspec_mean_checked(
      ergs = 1.0,
      eris = 1.0,
      scrs = 1.0,
      sirs = 0.0,
      crown_ages = 1.0
    )
  )

  expect_silent(
    pbd_numspec_mean_checked(
      ergs = 1.0,
      eris = 1.0,
      scrs = 1.0,
      sirs = 1.0,
      crown_ages = 0.0
    )
  )
})

test_that("pbd_numspec_median_checked use", {

  expected <- pbd_numspec_median_checked(
    erg = 1.2,
    eri = 2.3,
    scr = 3.4,
    sir = 4.5,
    crown_age = 6.7
  )
  expect_equal(expected, 99408712)

})

test_that("pbd_numspec_median_checked abuse", {

  expect_silent(
    pbd_numspec_median_checked(
      erg = 1.0,
      eri = 1.0,
      scr = 1.0,
      sir = 1.0,
      crown_age = 1.0
    )
  )

  expect_error(
    pbd_numspec_median_checked(
      erg = -123.456,
      eri = 1.0,
      scr = 1.0,
      sir = 1.0,
      crown_age = 1.0
    ),
    "'erg' must be positive"
  )

  expect_error(
    pbd_numspec_median_checked(
      erg = 1.0,
      eri = -123.456,
      scr = 1.0,
      sir = 1.0,
      crown_age = 1.0
    ),
    "'eri' must be positive"
  )

  expect_error(
    pbd_numspec_median_checked(
      erg = 1.0,
      eri = 1.0,
      scr = -123.456,
      sir = 1.0,
      crown_age = 1.0
    ),
    "'scr' must be positive"
  )

  expect_error(
    pbd_numspec_median_checked(
      erg = 1.0,
      eri = 1.0,
      scr = 1.0,
      sir = -123.456,
      crown_age = 1.0
    ),
    "'sir' must be positive"
  )

  expect_error(
    pbd_numspec_median_checked(
      erg = 1.0,
      eri = 1.0,
      scr = 1.0,
      sir = 1.0,
      crown_age = -123.456
    ),
    "'crown_age' must be positive"
  )
})
