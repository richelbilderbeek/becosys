context("is_bd_params")

test_that("use", {
  expect_true(is_bd_params(x = create_bd_params(0.1, 0.2)))
  expect_false(is_bd_params("nonsense"))
  expect_false(is_bd_params(NULL))
  expect_false(is_bd_params(NA))
})
