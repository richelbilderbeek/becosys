context("pbd_numspec_quantile_checked")

test_that("plot pbd_numspec_quantile", {

  sir <- 0.5
  erg <- 0.1
  eri <- 0.1
  scr <- 0.5
  age <- 15
  df <- data.frame(
    q = seq(0, 0.95, 0.05),
    n = NA
  )
  for (i in seq(1, nrow(df))) {
    df$n <- PBD::pbd_numspec_quantile(
      pars = c(sir, erg, scr, eri),
      age = age,
      soc = 2,
      quantile = df$q
    )
  }
  ggplot2::ggplot(df,
    ggplot2::aes(x = q, y = n)
  ) + ggplot2::geom_line()
})


test_that("use", {
  sir <- 1.1
  erg <- 0.1
  eri <- 0.2
  scr <- 0.9
  age <- 5
  quantile <- 0.6

  testthat::expect_equal(
    PBD::pbd_numspec_quantile(
      pars = c(sir, erg, scr, eri),
      age = age,
      soc = 2,
      quantile = quantile
    ),
    pbd_numspec_quantile_checked(
      sir = sir,
      erg = erg,
      scr = scr,
      eri = eri,
      crown_age = age,
      quantile = quantile
    )
  )

})
