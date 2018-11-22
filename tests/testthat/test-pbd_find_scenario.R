context("pbd_find_scenario")

test_that("use", {

  out <- pbd_find_scenario(
    scr = 0.25,
    sirg = 0.5,
    siri = 0.5,
    scenario = "equal",
    erg = 0.0,
    eri = 0.0
  )
  sum_youngest <- sum(out$stree_youngest$edge.length)
  sum_oldest <- sum(out$stree_oldest$edge.length)
  sum_random <- sum(out$stree_random$edge.length)
  testthat::expect_equal(sum_youngest, sum_oldest)
  testthat::expect_equal(sum_youngest, sum_random)
})
