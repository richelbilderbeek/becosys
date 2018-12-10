context("get_bd_param_names")

test_that("use", {
  created <- get_bd_param_names()
  expected <- c("ext_rate", "spec_rate")
  expect_equal(created, expected)
})
