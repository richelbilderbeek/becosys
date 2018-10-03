context("get_pbd_param_names")

test_that("use", {
  created <- get_pbd_param_names()
  expected <- c("erg", "eri", "scr", "sirg", "siri")
  expect_equal(created, expected)
})
